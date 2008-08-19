------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2000-2008, AdaCore                     --
--                                                                          --
--  This library is free software; you can redistribute it and/or modify    --
--  it under the terms of the GNU General Public License as published by    --
--  the Free Software Foundation; either version 2 of the License, or (at   --
--  your option) any later version.                                         --
--                                                                          --
--  This library is distributed in the hope that it will be useful, but     --
--  WITHOUT ANY WARRANTY; without even the implied warranty of              --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU       --
--  General Public License for more details.                                --
--                                                                          --
--  You should have received a copy of the GNU General Public License       --
--  along with this library; if not, write to the Free Software Foundation, --
--  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.          --
--                                                                          --
--  As a special exception, if other files instantiate generics from this   --
--  unit, or you link this unit with other files to produce an executable,  --
--  this  unit  does not  by itself cause  the resulting executable to be   --
--  covered by the GNU General Public License. This exception does not      --
--  however invalidate any other reasons why the executable file  might be  --
--  covered by the  GNU Public License.                                     --
------------------------------------------------------------------------------

with Ada.Strings.Fixed;
with Ada.Unchecked_Deallocation;
with Interfaces.C;

with AWS.Net.Poll_Events;
with AWS.Net.Log;
with AWS.Net.Std;
with AWS.Net.SSL;
with AWS.OS_Lib;

with System;

package body AWS.Net is

   Timeout_Token : constant String := " timeout.";

   function Errno return Integer renames Std.Errno;

   ---------
   -- Add --
   ---------

   procedure Add
     (FD_Set : in out FD_Set_Access;
      FD     : in     FD_Type;
      Event  : in     Wait_Event_Set)
   is
      Old_Set : FD_Set_Access;
   begin
      if Length (FD_Set.all) = FD_Set.Size then
         Old_Set := FD_Set;

         if FD_Set.Size < 256 then
            FD_Set := Copy (FD_Set, FD_Set.Size * 2);
         else
            FD_Set := Copy (FD_Set, FD_Set.Size + 256);
         end if;

         Free (Old_Set);
      end if;

      Add (FD_Set.all, FD, Event);
   end Add;

   ------------
   -- Adjust --
   ------------

   overriding procedure Adjust (Socket : in out Socket_Type) is
   begin
      Socket.C.Ref_Count.Increment;
   end Adjust;

   -----------
   -- Check --
   -----------

   function Check
     (Socket : in Socket_Type'Class;
      Events : in Wait_Event_Set) return Event_Set is
   begin
      return Poll (Socket, Events, 0.0);
   end Check;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Socket : in out Socket_Type) is
      procedure Free is
        new Unchecked_Deallocation (RW_Cache, RW_Cache_Access);
      Ref_Count : Natural;
   begin
      Socket.C.Ref_Count.Decrement (Value => Ref_Count);

      if Ref_Count = 0 then
         Free (Socket_Type'Class (Socket));
         Free (Socket.C);
      end if;
   end Finalize;

   ----------
   -- Free --
   ----------

   procedure Free (Socket : in out Socket_Access) is
      procedure Free is
        new Ada.Unchecked_Deallocation (Socket_Type'Class, Socket_Access);
   begin
      Free (Socket);
   end Free;

   procedure Free (FD_Set : in out FD_Set_Access) is
      procedure Dispose is
        new Ada.Unchecked_Deallocation (Net.FD_Set'Class, FD_Set_Access);
   begin
      Dispose (FD_Set);
   end Free;

   ---------------
   -- Host_Name --
   ---------------

   function Host_Name return String is
   begin
      return Net.Std.Host_Name;
   end Host_Name;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (Socket : in out Socket_Type) is
   begin
      if Socket.C = null then
         Socket.C := new RW_Cache;
      end if;
   end Initialize;

   ----------------
   -- Is_Timeout --
   ----------------

   function Is_Timeout (E : in Exception_Occurrence) return Boolean is
   begin
      return Strings.Fixed.Index (Exception_Message (E), Timeout_Token) > 0;
   end Is_Timeout;

   ----------
   -- Poll --
   ----------

   function Poll
     (Socket  : in Socket_Type'Class;
      Events  : in Wait_Event_Set;
      Timeout : in Duration) return Event_Set
   is
      Waiter : FD_Set'Class := To_FD_Set (Socket, Events);
      Dummy  : Natural;
   begin
      Wait (Waiter, Timeout, Dummy);
      return Status (Waiter, 1);
   end Poll;

   ------------------------
   -- Raise_Socket_Error --
   ------------------------

   procedure Raise_Socket_Error
     (Socket : in Socket_Type'Class; Text : in String) is
   begin
      Log.Error (Socket, Text);
      raise Socket_Error with Text;
   end Raise_Socket_Error;

   -------------
   -- Receive --
   -------------

   function Receive
     (Socket : in Socket_Type'Class;
      Max    : in Stream_Element_Count := 4096) return Stream_Element_Array
   is
      Result : Stream_Element_Array (1 .. Max);
      Last   : Stream_Element_Offset;
   begin
      Receive (Socket, Result, Last);

      return Result (1 .. Last);
   end Receive;

   ----------
   -- Send --
   ----------

   procedure Send
     (Socket : in Socket_Type'Class; Data : in Stream_Element_Array)
   is
      First : Stream_Element_Offset := Data'First;
      Last  : Stream_Element_Offset;
   begin
      loop
         Send (Socket, Data (First .. Data'Last), Last);

         exit when Last = Data'Last;

         Wait_For (Output, Socket);

         if Last < Data'Last then
            --  Otherwise First should be unchanged, because no data sent

            First := Last + 1;
         end if;
      end loop;
   end Send;

   -----------------------
   -- Set_Blocking_Mode --
   -----------------------

   procedure Set_Blocking_Mode
     (Socket : in out Socket_Type; Blocking : in Boolean) is
   begin
      if Blocking then
         Set_Timeout (Socket_Type'Class (Socket), Forever);
      else
         Set_Timeout (Socket_Type'Class (Socket), 0.0);
      end if;
   end Set_Blocking_Mode;

   ------------------
   -- Set_No_Delay --
   ------------------

   procedure Set_No_Delay
     (Socket : in Socket_Type; Value : in Boolean := True)
   is
      use Interfaces;
      use type C.int;
      Flag : aliased Integer := Boolean'Pos (Value);
   begin
      if OS_Lib.Set_Sock_Opt
           (S       => C.int (Get_FD (Socket_Type'Class (Socket))),
            Level   => OS_Lib.IPPROTO_TCP,
            OptName => OS_Lib.TCP_NODELAY,
            OptVal  => Flag'Address,
            OptLen  => Flag'Size / System.Storage_Unit) /= 0
      then
         Raise_Socket_Error
           (Socket, "Set_No_Delay error code" & Integer'Image (Std.Errno));
      end if;
   end Set_No_Delay;

   -----------------
   -- Set_Timeout --
   -----------------

   procedure Set_Timeout
     (Socket : in out Socket_Type; Timeout : in Duration) is
   begin
      Socket.Timeout := Timeout;
   end Set_Timeout;

   ------------
   -- Socket --
   ------------

   function Socket (Security : in Boolean) return Socket_Type'Class is
   begin
      if Security then
         declare
            Result : SSL.Socket_Type;
            pragma Warnings (Off, Result);
         begin
            return Result;
         end;

      else
         declare
            Result : Std.Socket_Type;
            pragma Warnings (Off, Result);
         begin
            return Result;
         end;
      end if;
   end Socket;

   function Socket (Security : in Boolean) return Socket_Access is
   begin
      return new Socket_Type'Class'(Socket (Security));
   end Socket;

   -----------------
   -- Socket_Pair --
   -----------------

   procedure Socket_Pair (S1, S2 : out Socket_Type) is
      Server : Std.Socket_Type;
      subtype STC is Socket_Type'Class;
   begin
      Std.Bind (Server, 0);
      Std.Listen (Server);

      Connect (STC (S1), "127.0.0.1", Std.Get_Port (Server));

      Std.Set_Timeout (Server, 0.25);

      loop
         Accept_Socket (Server, New_Socket => STC (S2));

         --  to be shure that it is S1 and S2 connected together

         exit when Peer_Addr (STC (S2)) = "127.0.0.1"
           and then Peer_Port (STC (S2)) = Get_Port (STC (S1))
           and then Peer_Port (STC (S1)) = Get_Port (STC (S2));

         Shutdown (STC (S2));
      end loop;

      Std.Shutdown (Server);
   end Socket_Pair;

   ---------------
   -- To_FD_Set --
   ---------------

   function To_FD_Set
     (Socket : in Socket_Type;
      Events : in Wait_Event_Set;
      Size   : in Positive := 1) return FD_Set'Class
   is
      Result : Poll_Events.Set (Size);
   begin
      Poll_Events.Add (Result, Get_FD (Socket_Type'Class (Socket)), Events);
      return Result;
   end To_FD_Set;

   ----------
   -- Wait --
   ----------

   function Wait
     (Socket : in Socket_Type'Class;
      Events : in Wait_Event_Set) return Event_Set is
   begin
      return Poll (Socket, Events, Socket.Timeout);
   end Wait;

   --------------
   -- Wait_For --
   --------------

   procedure Wait_For
     (Mode : in Wait_Event_Type; Socket : in Socket_Type'Class)
   is
      Events : Wait_Event_Set := (others => False);
      Result : Event_Set;
   begin
      Events (Mode) := True;

      Result := Wait (Socket, Events);

      if Result = Event_Set'(others => False) then
         Raise_Socket_Error
           (Socket, Wait_Event_Type'Image (Mode) & Timeout_Token);

      elsif Result = Event_Set'(Error => True, others => False) then
         Raise_Socket_Error
           (Socket, Wait_Event_Type'Image (Mode) & "_Wait error.");

      elsif not Result (Mode) then
         raise Program_Error;
      end if;
   end Wait_For;

end AWS.Net;
