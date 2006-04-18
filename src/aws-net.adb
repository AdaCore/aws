------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2000-2006                          --
--                                 AdaCore                                  --
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

with AWS.Net.Log;
with AWS.Net.Thin;
with AWS.Net.Std;
with AWS.Net.SSL;
with AWS.OS_Lib.Definitions;

with System;

package body AWS.Net is

   Timeout_Token : constant String := " timeout.";

   function Errno return Integer renames Std.Errno;

   ------------
   -- Adjust --
   ------------

   procedure Adjust (Socket : in out Socket_Type) is
   begin
      Socket.C.Ref_Count.Increment;
   end Adjust;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Socket : in out Socket_Type) is
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

   procedure Free (Socket : in out Socket_Type) is
      pragma Unreferenced (Socket);
   begin
      null;
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

   procedure Initialize (Socket : in out Socket_Type) is
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

   ------------------------
   -- Raise_Socket_Error --
   ------------------------

   procedure Raise_Socket_Error
     (Socket : Socket_Type'Class; Text : in String) is
   begin
      Log.Error (Socket, Text);
      Raise_Exception (Socket_Error'Identity, Text);
   end Raise_Socket_Error;

   -------------
   -- Receive --
   -------------

   function Receive
     (Socket : in Socket_Type'Class;
      Max    : in Stream_Element_Count := 4096)
      return Stream_Element_Array
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
     (Socket : in Socket_Type'Class;
      Data   : in Stream_Element_Array)
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
     (Socket   : in out Socket_Type;
      Blocking : in     Boolean) is
   begin
      if Blocking then
         Socket.Timeout := Forever;
      else
         Socket.Timeout := 0.0;
      end if;
   end Set_Blocking_Mode;

   ------------------
   -- Set_No_Delay --
   ------------------

   procedure Set_No_Delay
     (Socket : in Socket_Type;
      Value  : in Boolean := True)
   is
      use Interfaces;
      use type C.int;
      Flag : aliased Integer := Boolean'Pos (Value);
   begin
      if OS_Lib.Definitions.Set_Sock_Opt
           (S       => C.int (Get_FD (Socket_Type'Class (Socket))),
            Level   => OS_Lib.Definitions.IPPROTO_TCP,
            OptName => OS_Lib.Definitions.TCP_NODELAY,
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
     (Socket  : in out Socket_Type;
      Timeout : in     Duration) is
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

   ----------
   -- Wait --
   ----------

   function Wait
     (Socket : in Socket_Type'Class;
      Events : in Wait_Event_Set)
      return Event_Set
   is
      use Interfaces;
      use OS_Lib.Definitions;

      use type C.int;
      use type Thin.Events_Type;

      FD  : constant Integer := Get_FD (Socket);

      PFD : aliased Thin.Pollfd;
      RC      : C.int;
      Timeout : C.int;
      Errno   : Integer;
   begin
      if FD < 0 then
         Raise_Socket_Error (Socket, "Socket already closed.");
      end if;

      PFD := (Fd      => Thin.FD_Type (FD),
              Events  => 0,
              REvents => 0);

      if Socket.Timeout >= Duration (C.int'Last / 1_000) then
         Timeout := C.int'Last;
      else
         Timeout := C.int (Socket.Timeout * 1_000);
      end if;

      loop
         if Events (Input) then
            PFD.Events := POLLIN or POLLPRI;
         end if;

         if Events (Output) then
            PFD.Events := PFD.Events or POLLOUT;
         end if;

         RC := Thin.Poll (PFD'Address, 1, Timeout);

         case RC is
            when -1 =>
               Errno := Std.Errno;

               if Errno /= EINTR then
                  Raise_Socket_Error
                    (Socket, "Wait error code" & Integer'Image (Errno));
               end if;

            when 0  => return (others => False);
            when 1  =>
               return (Input  => (PFD.REvents and (POLLIN or POLLPRI)) /= 0,
                       Output => (PFD.REvents and POLLOUT) /= 0,
                       Error  => (PFD.REvents
                                  and (POLLERR or POLLHUP or POLLNVAL)) /= 0);
            when others =>
               raise Program_Error;
         end case;
      end loop;
   end Wait;

   --------------
   -- Wait_For --
   --------------

   procedure Wait_For
     (Mode   : in Wait_Event_Type;
      Socket : in Socket_Type'Class)
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
