------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2000-2004                          --
--                                ACT-Europe                                --
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

--  $Id$

with Ada.Exceptions;
with Ada.Unchecked_Deallocation;

with AWS.Net.Log;
with AWS.OS_Lib.Definitions;
with AWS.Utils;

with GNAT.Sockets.Set_FD;

pragma Warnings (Off);

--  Ignore warning about portability of the GNAT.Sockets.Thin
--  because we are using only Socket_Errno and it is exists at all main
--  platforms Unix, Windows and VMS.

with GNAT.Sockets.Thin;

pragma Warnings (On);

with Interfaces.C.Strings;
with System;

package body AWS.Net.Std is

   use Ada;
   use GNAT;

   package OSD renames AWS.OS_Lib.Definitions;

   type Socket_Hidden is record
      FD : Sockets.Socket_Type;
   end record;

   procedure Free is
      new Ada.Unchecked_Deallocation (Socket_Hidden, Socket_Hidden_Access);

   procedure Raise_Exception
     (E       : in Exceptions.Exception_Occurrence;
      Routine : in String);
   pragma No_Return (Raise_Exception);
   --  Raise exception Socket_Error with E's message and a reference to the
   --  routine name.

   procedure Raise_Socket_Error (Error : in Integer);
   pragma No_Return (Raise_Socket_Error);

   function Errno (Socket : in Socket_Type) return Integer;
   --  Returns and clears error state in socket.

   function Get_Addr_Info
     (Host  : in String;
      Port  : in Positive;
      Flags : in Interfaces.C.int := 0)
      return OSD.Addr_Info_Access;
   --  Returns the inet address information for the given host and port.
   --  Flags should be used from getaddrinfo C routine.

   procedure Set_Non_Blocking_Mode (Socket : in Socket_Type);
   --  Set the socket to the non-blocking mode.
   --  AWS is not using blocking sockets internally.

   function Swap_Little_Endian
     (S : in Interfaces.Unsigned_16) return Interfaces.Unsigned_16;

   -------------------
   -- Accept_Socket --
   -------------------

   procedure Accept_Socket
     (Socket     : in     Net.Socket_Type'Class;
      New_Socket : in out Socket_Type)
   is
      use Interfaces;
      use Sockets;
      use type C.int;

      Sock : C.int;
      Dummy : String (1 .. 32);
      Len   : aliased C.int := Dummy'Length;
   begin
      Wait_For (Input, Socket);

      Sock := Thin.C_Accept
                (C.int (Get_FD (Socket)), Dummy'Address, Len'Access);

      if Sock = Thin.Failure then
         Raise_Socket_Error (Std.Errno);
      end if;

      if New_Socket.S = null then
         New_Socket.S := new Socket_Hidden;
      end if;

      Set_FD (New_Socket.S.FD, Sock);

      Set_Non_Blocking_Mode (New_Socket);

      Set_Cache (New_Socket);
   end Accept_Socket;

   ----------
   -- Bind --
   ----------

   procedure Bind
     (Socket : in out Socket_Type;
      Port   : in     Natural;
      Host   : in     String := "")
   is
      use Interfaces;
      use type C.int;

      Info  : constant OSD.Addr_Info_Access
        := Get_Addr_Info (Host, Port, OSD.AI_PASSIVE);
      Res   : C.int;
      Errno : Integer;

   begin
      if Socket.S = null then
         Res := Sockets.Thin.C_Socket
                  (Info.ai_family, Info.ai_socktype, Info.ai_protocol);

         if Res = Sockets.Thin.Failure then
            OSD.FreeAddrInfo (Info);
            Raise_Socket_Error (Std.Errno);
         end if;

         Socket.S := new Socket_Hidden;

         Sockets.Set_FD (Socket.S.FD, Res);

         Set_Non_Blocking_Mode (Socket);
      end if;

      Res := Sockets.Thin.C_Bind
               (C.int (Get_FD (Socket)),
                Info.ai_addr,
                C.int (Info.ai_addrlen));

      OSD.FreeAddrInfo (Info);

      if Res = Sockets.Thin.Failure then
         Errno := Std.Errno;
         Sockets.Close_Socket (Socket.S.FD);
         Free (Socket.S);
         Raise_Socket_Error (Errno);
      end if;
   end Bind;

   -------------
   -- Connect --
   -------------

   procedure Connect
     (Socket : in out Socket_Type;
      Host   : in     String;
      Port   : in     Positive;
      Wait   : in     Boolean := True)
   is
      use Interfaces;
      use type C.int;

      Info  : constant OSD.Addr_Info_Access := Get_Addr_Info (Host, Port);
      Res   : C.int;
      Errno : Integer;

   begin
      if Socket.S = null then
         Res := Sockets.Thin.C_Socket
                  (Info.ai_family, Info.ai_socktype, Info.ai_protocol);

         if Res = Sockets.Thin.Failure then
            OSD.FreeAddrInfo (Info);
            Raise_Socket_Error (Std.Errno);
         end if;

         Socket.S := new Socket_Hidden;

         Sockets.Set_FD (Socket.S.FD, Res);
      end if;

      Set_Non_Blocking_Mode (Socket);

      Res := Sockets.Thin.C_Connect
               (C.int (Get_FD (Socket)),
                Info.ai_addr,
                C.int (Info.ai_addrlen));

      OSD.FreeAddrInfo (Info);

      if Res = Sockets.Thin.Failure then
         Errno := Std.Errno;

         if Errno = OSD.EWOULDBLOCK
           or else Errno = OSD.EINPROGRESS
         then
            Errno := 0;

            if Wait then
               declare
                  Events : constant Event_Set
                    := Net.Wait (Socket, (Output => True, Input => False));
               begin
                  if Events (Error) then
                     Errno := Std.Errno (Socket);
                  elsif not Events (Output) then
                     Errno := OSD.ETIMEDOUT;
                  end if;
               end;
            end if;
         end if;

         if Errno /= 0 then
            Sockets.Close_Socket (Socket.S.FD);
            Free (Socket.S);
            Raise_Socket_Error (Errno);
         end if;
      end if;

      Set_Cache (Socket);
   end Connect;

   -----------
   -- Errno --
   -----------

   function Errno return Integer is
   begin
      return GNAT.Sockets.Thin.Socket_Errno;
   end Errno;

   function Errno (Socket : in Socket_Type) return Integer is
      use Interfaces;
      use type Interfaces.C.int;
      use Sockets;
      RC  : C.int;
      Res : aliased C.int := 0;
      Len : aliased C.int := Res'Size / System.Storage_Unit;
   begin
      RC := Thin.C_Getsockopt
              (S       => Interfaces.C.int (Get_FD (Socket)),
               Level   => OSD.SOL_SOCKET,
               Optname => OSD.SO_ERROR,
               Optval  => Res'Address,
               Optlen  => Len'Access);

      if RC = Thin.Failure then
         Raise_Socket_Error (Errno);
      end if;

      return Integer (Res);
   end Errno;

   ----------
   -- Free --
   ----------

   procedure Free (Socket : in out Socket_Type) is
   begin
      Free (Socket.S);
      Release_Cache (Socket);
   end Free;

   -------------------
   -- Get_Addr_Info --
   -------------------

   function Get_Addr_Info
     (Host  : in String;
      Port  : in Positive;
      Flags : in Interfaces.C.int := 0)
      return OSD.Addr_Info_Access
   is
      use Interfaces.C;
      use type OSD.Addr_Info_Access;

      C_Node : aliased char_array := To_C (Host);
      P_Node : Strings.chars_ptr;
      C_Serv : aliased char_array := To_C (AWS.Utils.Image (Port));
      Res    : int;
      Result : aliased OSD.Addr_Info_Access;
      Hints  : constant OSD.Addr_Info
        := (ai_family    => OSD.PF_UNSPEC,
            ai_socktype  => OSD.SOCK_STREAM,
            ai_protocol  => OSD.IPPROTO_IP,
            ai_flags     => Flags,
            ai_addrlen   => 0,
            ai_canonname => Strings.Null_Ptr,
            ai_addr      => System.Null_Address,
            ai_next      => null);
   begin
      if Host = "" then
         P_Node := Strings.Null_Ptr;
      else
         P_Node := Strings.To_Chars_Ptr (C_Node'Unchecked_Access);
      end if;

      Res := OSD.GetAddrInfo
               (node    => P_Node,
                service => Strings.To_Chars_Ptr (C_Serv'Unchecked_Access),
                hints   => Hints,
                res     => Result'Access);

      if Res = OSD.EAI_SYSTEM then
         Raise_Socket_Error (Errno);

      elsif Res /= 0 then
         Ada.Exceptions.Raise_Exception
           (Socket_Error'Identity, Strings.Value (OSD.GAI_StrError (Res)));
      end if;

      return Result;
   end Get_Addr_Info;

   ------------
   -- Get_FD --
   ------------

   function Get_FD (Socket : in Socket_Type) return Integer is
   begin
      return Sockets.To_C (Socket.S.FD);
   end Get_FD;

   -----------------------------
   -- Get_Receive_Buffer_Size --
   -----------------------------

   function Get_Receive_Buffer_Size (Socket : in Socket_Type) return Natural is
      use Sockets;
   begin
      return Get_Socket_Option (Socket.S.FD, Name => Receive_Buffer).Size;
   exception
      when E : Sockets.Socket_Error =>
         Raise_Exception (E, "Get_Receive_Buffer_Size");
   end Get_Receive_Buffer_Size;

   --------------------------
   -- Get_Send_Buffer_Size --
   --------------------------

   function Get_Send_Buffer_Size (Socket : in Socket_Type) return Natural is
      use Sockets;
   begin
      return Get_Socket_Option (Socket.S.FD, Name => Send_Buffer).Size;
   exception
      when E : Sockets.Socket_Error =>
         Raise_Exception (E, "Get_Send_Buffer_Size");
   end Get_Send_Buffer_Size;

   ---------------
   -- Host_Name --
   ---------------

   function Host_Name return String is
   begin
      return Sockets.Host_Name;
   end Host_Name;

   ------------
   -- Listen --
   ------------

   procedure Listen
     (Socket     : in Socket_Type;
      Queue_Size : in Positive := 5) is
   begin
      Sockets.Listen_Socket (Socket.S.FD, Queue_Size);
   exception
      when E : Sockets.Socket_Error =>
         Raise_Exception (E, "Listen");
   end Listen;

   ---------------
   -- Peer_Addr --
   ---------------

   function Peer_Addr (Socket : in Socket_Type) return String is
      use Sockets;
      use Interfaces;
      use type C.int;
      use type C.short;

      type In6_Addr is array (1 .. 8) of Unsigned_16;
      pragma Convention (C, In6_Addr);

      type Sockaddr_In6 is record
         Family    : C.short;          -- AF_INET6
         Port      : C.unsigned_short; -- transport layer port #
         FlowInfo  : C.unsigned_long;  -- IPv6 traffic class & flow info
         Addr      : In6_Addr;         -- IPv6 address
         Scope_Id  : C.unsigned_long;  -- set of interfaces for a scope
      end record;
      pragma Convention (C, Sockaddr_In6);

      Sin6 : aliased Sockaddr_In6;
      Sin  : aliased Thin.Sockaddr_In;
      pragma Import (C, Sin);
      for Sin'Address use Sin6'Address;

      Len : aliased C.int := Sin6'Size / 8;

   begin
      if Thin.C_Getpeername
           (C.int (Get_FD (Socket)), Sin6'Address, Len'Access) = Thin.Failure
      then
         Raise_Socket_Error (Std.Errno);
      end if;

      if Sin6.Family = OSD.PF_INET then
         return Utils.Image (Integer (Sin.Sin_Addr.S_B1))
            & '.' & Utils.Image (Integer (Sin.Sin_Addr.S_B2))
            & '.' & Utils.Image (Integer (Sin.Sin_Addr.S_B3))
            & '.' & Utils.Image (Integer (Sin.Sin_Addr.S_B4));

      elsif Sin6.Family = OSD.PF_INET6 then
         declare
            Result : String (1 .. 8 * 5);
            Index  : Positive := Result'First;
            Zero   : Boolean  := True;
         begin
            for J in Sin6.Addr'Range loop
               if Sin6.Addr (J) = 0 and Zero then
                  --  Any number of starting zeroes showing by ::.

                  if Index = Result'First then
                     Result (Result'First .. Result'First + 1) := "::";
                     Index := Index + 2;
                  end if;

               else
                  Zero := False;

                  declare
                     Img16 : constant String
                       := Utils.Hex
                            (Integer (Swap_Little_Endian (Sin6.Addr (J))));
                  begin
                     Result (Index .. Index + Img16'Length) := Img16 & ':';
                     Index := Index + Img16'Length + 1;
                  end;
               end if;
            end loop;

            --  Ignore trailing ':' in case of none zero line.

            return Result (Result'First .. Index - 1 - Boolean'Pos (not Zero));
         end;

      else
         return "unknown protocol family.";
      end if;
   end Peer_Addr;

   -------------
   -- Pending --
   -------------

   function Pending (Socket : in Socket_Type) return Stream_Element_Count is
      use Interfaces;
      use type C.int;
      Arg : aliased C.int;
      Res : constant C.int := Sockets.Thin.C_Ioctl
                                (C.int (Get_FD (Socket)),
                                 OSD.FIONREAD,
                                 Arg'Unchecked_Access);
   begin
      if Res = Sockets.Thin.Failure then
         Raise_Socket_Error (Errno);
      end if;

      return Stream_Element_Count (Arg);
   end Pending;

   ---------------------
   -- Raise_Exception --
   ---------------------

   procedure Raise_Exception
     (E       : in Exceptions.Exception_Occurrence;
      Routine : in String)
   is
      use Ada.Exceptions;
   begin
      Raise_Exception
        (Socket_Error'Identity,
         Message => Routine & " : " & Exception_Message (E));
   end Raise_Exception;

   ------------------------
   -- Raise_Socket_Error --
   ------------------------

   procedure Raise_Socket_Error (Error : in Integer) is

      use Interfaces;

      pragma Warnings (Off);
      --  Kill warnings as one of the following procedure won't be used

      function To_String (Str : in String)              return String;
      pragma Inline (To_String);
      function To_String (Str : in C.Strings.chars_ptr) return String;
      pragma Inline (To_String);
      --  The GNAT.Sockets.Thin.Socket_Error_Message has a different
      --  spec in GNAT 5.02 and 5.03. Those routines are there to be
      --  able to accommodate both compilers.

      ---------------
      -- To_String --
      ---------------

      function To_String (Str : in String) return String is
      begin
         return Str;
      end To_String;

      function To_String (Str : in C.Strings.chars_ptr) return String is
      begin
         return C.Strings.Value (Str);
      end To_String;

      pragma Warnings (On);

      Msg : String := Integer'Image (Error) & "] ";
   begin
      Msg (Msg'First) := '[';
      Ada.Exceptions.Raise_Exception
        (Socket_Error'Identity,
         Msg & To_String (Sockets.Thin.Socket_Error_Message (Error)));
   end Raise_Socket_Error;

   -------------
   -- Receive --
   -------------

   procedure Receive
     (Socket : in     Socket_Type;
      Data   :    out Stream_Element_Array;
      Last   :    out Stream_Element_Offset) is
   begin
      Wait_For (Input, Socket);

      Sockets.Receive_Socket (Socket.S.FD, Data, Last);

      --  Check if socket closed by peer.

      if Last = Data'First - 1 then
         Ada.Exceptions.Raise_Exception
           (Socket_Error'Identity,
            Message => "Receive : Socket closed by peer.");
      end if;

      if Net.Log.Is_Active then
         Net.Log.Write
           (Direction => Net.Log.Received,
            FD        => Get_FD (Socket),
            Data      => Data,
            Last      => Last);
      end if;
   exception
      when E : Sockets.Socket_Error =>
         Raise_Exception (E, "Receive");
   end Receive;

   ----------
   -- Send --
   ----------

   procedure Send
     (Socket : in     Socket_Type;
      Data   : in     Stream_Element_Array;
      Last   :    out Stream_Element_Offset)
   is
      use Interfaces;
      use type C.int;

      Errno : Integer;
      RC    : C.int;
   begin
      RC := Sockets.Thin.C_Send
              (C.int (Get_FD (Socket)),
               Data'Address,
               Data'Length,
               0);

      if RC = Sockets.Thin.Failure then
         Errno := Std.Errno;

         if Errno = OSD.EWOULDBLOCK then
            Last := Data'First - 1;

            return;

         else
            Raise_Socket_Error (Errno);
         end if;
      end if;

      Last := Data'First - 1 + Stream_Element_Offset (RC);

      if Net.Log.Is_Active then
         Net.Log.Write
           (Direction => Net.Log.Sent,
            FD        => Get_FD (Socket),
            Data      => Data,
            Last      => Last);
      end if;
   end Send;

   ---------------------------
   -- Set_Non_Blocking_Mode --
   ---------------------------

   procedure Set_Non_Blocking_Mode (Socket : in Socket_Type) is
      use Sockets;
      Mode : Request_Type (Non_Blocking_IO);
   begin
      Mode.Enabled := True;

      Control_Socket (Socket.S.FD, Mode);
   exception
      when E : Sockets.Socket_Error =>
         Raise_Exception (E, "Set_Non_Blocking_Mode");
   end Set_Non_Blocking_Mode;

   -----------------------------
   -- Set_Receive_Buffer_Size --
   -----------------------------

   procedure Set_Receive_Buffer_Size
     (Socket : in Socket_Type;
      Size   : in Natural)
   is
      use Sockets;
   begin
      Set_Socket_Option (Socket.S.FD, Option => (Receive_Buffer, Size));
   exception
      when E : Sockets.Socket_Error =>
         Raise_Exception (E, "Set_Receive_Buffer_Size");
   end Set_Receive_Buffer_Size;

   --------------------------
   -- Set_Send_Buffer_Size --
   --------------------------

   procedure Set_Send_Buffer_Size
     (Socket : in Socket_Type;
      Size   : in Natural)
   is
      use Sockets;
   begin
      Set_Socket_Option (Socket.S.FD, Option => (Send_Buffer, Size));
   exception
      when E : Sockets.Socket_Error =>
         Raise_Exception (E, "Set_Send_Buffer_Size");
   end Set_Send_Buffer_Size;

   --------------
   -- Shutdown --
   --------------

   procedure Shutdown (Socket : in Socket_Type) is
   begin
      if Socket.S /= null then
         begin
            --  We catch socket exceptions here as we do not want this call to
            --  fail. A shutdown will fail on non connected sockets.

            Sockets.Shutdown_Socket (Socket.S.FD);
         exception
            when Sockets.Socket_Error =>
               null;
         end;

         begin
            --  ??? In some cases the call above fails because the socket
            --  descriptor is not valid (errno = EABF). This happen on
            --  GNU/Linux only and the problem is not fully understood at this
            --  point. We catch the exception here to hide this problem.

            Sockets.Close_Socket (Socket.S.FD);
         exception
            when Sockets.Socket_Error | Constraint_Error =>
               null;
         end;
      end if;
   end Shutdown;

   ------------------------
   -- Swap_Little_Endian --
   ------------------------

   function Swap_Little_Endian
     (S : in Interfaces.Unsigned_16) return Interfaces.Unsigned_16
   is
      use System;
      Big_Endian : constant Boolean := Default_Bit_Order = High_Order_First;
   begin
      if Big_Endian then
         return S;
      else
         return Interfaces.Rotate_Left (S, 8);
      end if;
   end Swap_Little_Endian;

begin
   Sockets.Initialize;
end AWS.Net.Std;
