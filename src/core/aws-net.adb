------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2000-2020, AdaCore                     --
--                                                                          --
--  This library is free software;  you can redistribute it and/or modify   --
--  it under terms of the  GNU General Public License  as published by the  --
--  Free Software  Foundation;  either version 3,  or (at your  option) any --
--  later version. This library is distributed in the hope that it will be  --
--  useful, but WITHOUT ANY WARRANTY;  without even the implied warranty of --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                    --
--                                                                          --
--  As a special exception under Section 7 of GPL version 3, you are        --
--  granted additional permissions described in the GCC Runtime Library     --
--  Exception, version 3.1, as published by the Free Software Foundation.   --
--                                                                          --
--  You should have received a copy of the GNU General Public License and   --
--  a copy of the GCC Runtime Library Exception along with this program;    --
--  see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see   --
--  <http://www.gnu.org/licenses/>.                                         --
--                                                                          --
--  As a special exception, if other files instantiate generics from this   --
--  unit, or you link this unit with other files to produce an executable,  --
--  this  unit  does not  by itself cause  the resulting executable to be   --
--  covered by the GNU General Public License. This exception does not      --
--  however invalidate any other reasons why the executable file  might be  --
--  covered by the  GNU Public License.                                     --
------------------------------------------------------------------------------

with Ada.Real_Time;
with Ada.Strings.Fixed;
with Ada.Unchecked_Deallocation;

with AWS.Net.Log;
with AWS.Net.Poll_Events;
with AWS.Net.SSL;
with AWS.Net.Std;

with AWS.OS_Lib;

with Interfaces.C.Strings;

package body AWS.Net is

   Timeout_Token : constant String := " timeout ";

   ---------
   -- Add --
   ---------

   procedure Add
     (FD_Set : in out FD_Set_Access;
      FD     : FD_Type;
      Event  : Wait_Event_Set)
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
     (Socket : Socket_Type'Class;
      Events : Wait_Event_Set) return Event_Set is
   begin
      return Poll (Socket, Events, 0.0);
   end Check;

   ------------------------
   -- Cipher_Description --
   ------------------------

   function Cipher_Description (Socket : Socket_Type) return String is
      pragma Unreferenced (Socket);
   begin
      return "";
   end Cipher_Description;

   -------------------
   -- Error_Message --
   -------------------

   function Error_Message (Errno : Integer) return String is
      use Interfaces.C.Strings;
      Ptr  : constant chars_ptr := OS_Lib.Socket_StrError (Errno);
      Code : constant String    := '[' & Utils.Image (Errno) & "] ";
   begin
      if Ptr = Null_Ptr then
         return Code & "Unknown error";
      else
         return Code & Value (Ptr);
      end if;
   end Error_Message;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Socket : in out Socket_Type) is
      procedure Unchecked_Free is
        new Unchecked_Deallocation (RW_Data, RW_Data_Access);
      procedure Unchecked_Free is
        new Unchecked_Deallocation (Read_Cache, Read_Cache_Access);
      procedure Unchecked_Free is
        new Unchecked_Deallocation (Write_Cache, Write_Cache_Access);
      Cache     : RW_Data_Access := Socket.C;
      Ref_Count : Natural;
   begin
      --  Ensure call is idempotent

      Socket.C := null;

      if Cache /= null then
         Cache.Ref_Count.Decrement (Value => Ref_Count);

         if Ref_Count = 0 then
            Free (Socket_Type'Class (Socket));
            Unchecked_Free (Cache.R_Cache);
            Unchecked_Free (Cache.W_Cache);
            Unchecked_Free (Cache);
         end if;
      end if;
   end Finalize;

   ----------
   -- Free --
   ----------

   procedure Free (Socket : in out Socket_Access) is
      procedure Unchecked_Free is
        new Ada.Unchecked_Deallocation (Socket_Type'Class, Socket_Access);
   begin
      Unchecked_Free (Socket);
   end Free;

   procedure Free (FD_Set : in out FD_Set_Access) is
      procedure Unchecked_Free is
        new Ada.Unchecked_Deallocation (Net.FD_Set'Class, FD_Set_Access);
   begin
      Unchecked_Free (FD_Set);
   end Free;

   ----------------------
   -- Get_Socket_Errno --
   ----------------------

   function Get_Socket_Errno (E : Exception_Occurrence) return Natural is
      M           : constant String := Exception_Message (E);
      First, Last : Natural;
      Errno       : Natural := 0;
   begin
      if Exception_Identity (E) = Net.Socket_Error'Identity
        or else
          Strings.Fixed.Index (Exception_Name (E), "CONNECTION_ERROR") /= 0
      then
         First := M'First;

         --  Now check for the start of the message containing the errno

         First := Strings.Fixed.Index (M, "[", From => First);

         if First /= 0 then
            First := First + 1;
            Last := First;
            while Last < M'Last and then M (Last + 1) in '0' .. '9' loop
               Last := Last + 1;
            end loop;

            Errno := Natural'Value (M (First .. Last));
         end if;
      end if;

      return Errno;
   end Get_Socket_Errno;

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
         Socket.C := new RW_Data;
      end if;
   end Initialize;

   ----------------
   -- IO_Control --
   ----------------

   function IO_Control
     (Socket : Socket_Type;
      Code   : Interfaces.C.int) return Stream_Element_Offset
   is
      use Interfaces;
      use type C.int;

      S : Socket_Type'Class renames Socket_Type'Class (Socket);

      Res : C.int;
      Arg : aliased C.int;

   begin
      Res := OS_Lib.C_Ioctl (C.int (S.Get_FD), Code, Arg'Unchecked_Access);

      if Res = -1 then
         S.Raise_Socket_Error (Error_Message (OS_Lib.Socket_Errno));
      end if;

      return Stream_Element_Count (Arg);
   end IO_Control;

   --------------------
   -- IPv6_Available --
   --------------------

   function IPv6_Available return Boolean is
   begin
      return Std.IPv6_Available;
   end IPv6_Available;

   --------------------
   -- Is_Any_Address --
   --------------------

   function Is_Any_Address (Socket : Socket_Type) return Boolean is
      pragma Unreferenced (Socket);
   begin
      return False;
   end Is_Any_Address;

   -------------
   -- Is_IPv6 --
   -------------

   function Is_IPv6 (Socket : Socket_Type) return Boolean is
      pragma Unreferenced (Socket);
   begin
      return False;
   end Is_IPv6;

   ------------------
   -- Is_Listening --
   ------------------

   function Is_Listening (Socket : Socket_Type) return Boolean is
   begin
      --  Do not use SO_ACCEPCONN socket option because Mac OS/X version 10.8
      --  does not support it. Would be better to use SO_ACCEPTCONN instead of
      --  boolean flag when possible.

      return Socket.C.Listening;
   end Is_Listening;

   --------------------
   -- Is_Peer_Closed --
   --------------------

   function Is_Peer_Closed
     (Socket : Socket_Type;
      E      : Exception_Occurrence) return Boolean
   is
      pragma Unreferenced (Socket);
   begin
      return Exception_Message (E) = Peer_Closed_Message;
   end Is_Peer_Closed;

   ----------------
   -- Is_Timeout --
   ----------------

   function Is_Timeout
     (Socket : Socket_Type;
      E      : Exception_Occurrence) return Boolean
   is
      pragma Unreferenced (Socket);
   begin
      return Is_Timeout (E);
   end Is_Timeout;

   function Is_Timeout (E : Exception_Occurrence) return Boolean is
   begin
      return Strings.Fixed.Index (Exception_Message (E), Timeout_Token) > 0;
   end Is_Timeout;

   ----------------
   -- Last_Index --
   ----------------

   function Last_Index
     (First : Stream_Element_Offset;
      Count : Natural) return Ada.Streams.Stream_Element_Offset is
   begin
      if First = Stream_Element_Offset'First and then Count = 0 then
         raise Constraint_Error with
           "last index out of range (no element transferred)";
      else
         return First + Stream_Element_Offset (Count - 1);
      end if;
   end Last_Index;

   ---------------
   -- Localhost --
   ---------------

   function Localhost (IPv6 : Boolean) return String is
   begin
      if IPv6 then
         return "::1";
      else
         return "127.0.0.1";
      end if;
   end Localhost;

   -----------------
   -- Output_Busy --
   -----------------

   function Output_Busy (Socket : Socket_Type) return Stream_Element_Offset is
      --  Implementation not in the aws-net-std__ipv6 and aws-net-std__gnat.adb
      --  separately because GNAT.Sockets doesn't have appropriate Request_Type
      --  anyway, at least in GNAT GPL 2013. So, we avoid code duplication this
      --  way. We could easily make this routine abstract and split
      --  implementation between aws-net-std__ipv6 and aws-net-std__gnat.adb
      --  when GNAT.Sockets.Control_Socket get support it.
   begin
      pragma Warnings (Off, "*condition is always*");

      if OS_Lib.FIONWRITE = -1 then
         return -1;
      else
         return Socket.IO_Control (OS_Lib.FIONWRITE);
      end if;

      pragma Warnings (On, "*condition is always*");
   end Output_Busy;

   ------------------
   -- Output_Space --
   ------------------

   function Output_Space (Socket : Socket_Type) return Stream_Element_Offset is
   begin
      pragma Warnings (Off, "*condition is always*");

      if OS_Lib.FIONSPACE /= -1 then
         return Socket.IO_Control (OS_Lib.FIONSPACE);

      elsif OS_Lib.FIONWRITE /= -1 then
         return Stream_Element_Offset'Max
                  (Stream_Element_Offset
                     (Socket_Type'Class (Socket).Get_Send_Buffer_Size)
                   - Socket.IO_Control (OS_Lib.FIONWRITE), 0);
      else
         return -1;
      end if;

      pragma Warnings (On, "*condition is always*");
   end Output_Space;

   ----------
   -- Poll --
   ----------

   function Poll
     (Socket  : Socket_Type'Class;
      Events  : Wait_Event_Set;
      Timeout : Duration) return Event_Set
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
     (Socket : Socket_Type'Class; Text : String) is
   begin
      Log.Error (Socket, Text);
      raise Socket_Error with Text;
   end Raise_Socket_Error;

   -------------
   -- Receive --
   -------------

   function Receive
     (Socket : Socket_Type'Class;
      Max    : Stream_Element_Count := 4096) return Stream_Element_Array
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
     (Socket : Socket_Type'Class; Data : Stream_Element_Array)
   is
      use Ada.Real_Time;
      Chunk_Size : constant := 100 * 1_024;
      Save       : constant Boolean := Socket.C.Can_Wait;
      First      : Stream_Element_Offset := Data'First;
      Chunk_Last : Stream_Element_Offset := Data'Last;
      Last       : Stream_Element_Offset;
      Stamp      : Time;
   begin
      Socket.C.Can_Wait := True;

      if Socket.Timeout = Forever then
         Stamp := Time_Last;

      elsif Socket.Timeout < 0.0 then
         Stamp := Clock;

      else
         begin
            Stamp := Clock + To_Time_Span (Socket.Timeout);
         exception
            when Constraint_Error =>
               --  Too big
               Stamp := Time_Last;
         end;
      end if;

      loop
         Chunk_Last :=
           Stream_Element_Offset'Min (Data'Last, First + Chunk_Size - 1);

         Send (Socket, Data (First .. Chunk_Last), Last);

         exit when Last = Data'Last;

         Wait_For (Output, Socket, Timeout => To_Duration (Stamp - Clock));

         if Last < Data'Last then
            --  Otherwise First should be unchanged, because no data sent

            First := Last + 1;
         end if;
      end loop;

      Socket.C.Can_Wait := Save;
   exception
      when others =>
         Socket.C.Can_Wait := Save;
         raise;
   end Send;

   procedure Send
     (Sockets : Socket_Set; Data : Stream_Element_Array)
   is
      Wait_Events : constant Wait_Event_Set :=
                      (Input => False, Output => True);
      Set         : Poll_Events.Set (Sockets'Length);
      Socks       : Socket_Set (1 .. Sockets'Length) := Sockets;
      Index       : array (Sockets'Range) of Stream_Element_Offset :=
                      (others => 1);
      Last        : Stream_Element_Offset;
      Count       : Natural;
      Sock_Index  : Positive;
      Chunk_Size  : Stream_Element_Offset;
   begin
      --  First check if there is something to do

      if Sockets'Length = 0 then
         return;
      end if;

      --  Add all sockets into the poll

      for S of Sockets loop
         Set.Add (S.Get_FD, Wait_Events);
      end loop;

      --  Send data to available sockets

      loop
         Set.Wait (Forever, Count);

         Sock_Index := 1;

         for K in 1 .. Count loop
            Set.Next (Sock_Index);

            Chunk_Size := Socks (Sock_Index).Output_Space;

            if Chunk_Size = -1 then
               Chunk_Size := 100 * 1_024;
            end if;

            Last := Stream_Element_Offset'Min
              (Index (Sock_Index) + Chunk_Size, Data'Last);

            Socks (Sock_Index).Send
              (Data (Index (Sock_Index) .. Last));

            Index (Sock_Index) := Last + 1;

            if Index (Sock_Index) > Data'Last then
               --  No more data for this socket. The Set.Remove on the socket
               --  set move the last socket in the set to the location of the
               --  removed one. Do the same for the local data to keep data
               --  consistency.
               --
               --  Note that in this case we do not want to increment the
               --  socket index. The new loop will check the socket at the
               --  same position which is now the previous last in the set.

               if Sock_Index /= Set.Length then
                  Socks (Sock_Index) := Socks (Set.Length);
                  Index (Sock_Index) := Index (Set.Length);
               end if;

               Set.Remove (Sock_Index);

            else
               --  In this case, and only in this case we move to next socket
               --  position for next iteration.

               Sock_Index := Sock_Index + 1;
            end if;
         end loop;

         exit when Set.Length = 0;
      end loop;
   end Send;

   -----------------------
   -- Set_Blocking_Mode --
   -----------------------

   procedure Set_Blocking_Mode
     (Socket : in out Socket_Type; Blocking : Boolean) is
   begin
      if Blocking then
         Set_Timeout (Socket_Type'Class (Socket), Forever);
      else
         Set_Timeout (Socket_Type'Class (Socket), 0.0);
      end if;
   end Set_Blocking_Mode;

   -----------------
   -- Set_Timeout --
   -----------------

   procedure Set_Timeout
     (Socket : in out Socket_Type; Timeout : Duration) is
   begin
      Socket.Timeout := Timeout;
   end Set_Timeout;

   ------------
   -- Socket --
   ------------

   function Socket (Security : Boolean) return Socket_Type'Class is
   begin
      if Security then
         declare
            Result : SSL.Socket_Type;
         begin
            return Result;
         end;

      else
         declare
            Result : Std.Socket_Type;
         begin
            return Result;
         end;
      end if;
   end Socket;

   function Socket
     (Security : Boolean) return not null access Socket_Type'Class
   is
      Result : constant not null Socket_Access :=
                 new Socket_Type'Class'(Socket (Security));
   begin
      return Result;
   end Socket;

   -----------------
   -- Socket_Pair --
   -----------------

   procedure Socket_Pair (S1, S2 : out Socket_Type) is
      Server : Std.Socket_Type;
      subtype STC is Socket_Type'Class;
      Local_Host : constant String := "127.0.0.1";
   begin
      Std.Bind (Server, Host => Local_Host, Port => 0);
      Std.Listen (Server);

      Connect (STC (S1), Host => Local_Host, Port => Std.Get_Port (Server));

      Std.Set_Timeout (Server, 0.25);

      loop
         Accept_Socket (Server, New_Socket => STC (S2));

         --  to be shure that it is S1 and S2 connected together

         exit when Peer_Addr (STC (S2)) = Local_Host
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
     (Socket : Socket_Type;
      Events : Wait_Event_Set;
      Size   : Positive := 1) return FD_Set'Class
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
     (Socket : Socket_Type'Class;
      Events : Wait_Event_Set) return Event_Set is
   begin
      return Poll (Socket, Events, Socket.Timeout);
   end Wait;

   --------------
   -- Wait_For --
   --------------

   procedure Wait_For
     (Mode : Wait_Event_Type; Socket : Socket_Type'Class; Timeout : Duration)
   is
      Events : Wait_Event_Set := (others => False);
      Result : Event_Set;
   begin
      Events (Mode) := True;

      Result := Poll (Socket, Events, Timeout);

      if Result = Event_Set'(others => False) then
         Raise_Socket_Error
           (Socket,
            Wait_Event_Type'Image (Mode) & Timeout_Token
            & Utils.Significant_Image (Socket.Timeout, 2));
         --  Can't use just Timeout in tests output because it can differ from
         --  stable Socket.Timeout value on Send operation.

      elsif Result = Event_Set'(Error => True, others => False) then
         Raise_Socket_Error
           (Socket, Wait_Event_Type'Image (Mode) & "_Wait error.");

      elsif not Result (Mode) then
         raise Program_Error;
      end if;
   end Wait_For;

   procedure Wait_For (Mode : Wait_Event_Type; Socket : Socket_Type'Class) is
   begin
      Wait_For (Mode, Socket, Socket.Timeout);
   end Wait_For;

end AWS.Net;
