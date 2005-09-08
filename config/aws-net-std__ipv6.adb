------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2004-2005                          --
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
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;

with AWS.Net.Log;
with AWS.OS_Lib.Definitions;
with AWS.Utils;

pragma Warnings (Off);

--  Ignore warning about portability of the GNAT.Sockets.Thin

with GNAT.Sockets.Thin;

pragma Warnings (On);

with Interfaces.C.Strings;
with System;

package body AWS.Net.Std is

   use Ada;
   use GNAT;

   package OSD renames AWS.OS_Lib.Definitions;

   type Socket_Hidden is record
      FD : Interfaces.C.int;
   end record;

   type In6_Addr is array (1 .. 8) of Interfaces.Unsigned_16;
   pragma Convention (C, In6_Addr);

   type Sockaddr_In6 is record
      Family    : Interfaces.C.short;          -- AF_INET6
      Port      : Interfaces.C.unsigned_short; -- transport layer port #
      FlowInfo  : Interfaces.C.unsigned_long;  -- IPv6 traffic class&flow info
      Addr      : In6_Addr;                    -- IPv6 address
      Scope_Id  : Interfaces.C.unsigned_long;  -- set of interfaces for a scope
   end record;
   pragma Convention (C, Sockaddr_In6);

   procedure Free is
      new Ada.Unchecked_Deallocation (Socket_Hidden, Socket_Hidden_Access);

   procedure Raise_Socket_Error (Error : in Integer);
   pragma No_Return (Raise_Socket_Error);

   function Get_Addr_Info
     (Host  : in String;
      Port  : in Natural;
      Flags : in Interfaces.C.int := 0)
      return OSD.Addr_Info_Access;
   --  Returns the inet address information for the given host and port.
   --  Flags should be used from getaddrinfo C routine.

   function Get_Int_Sock_Opt
     (Socket : in Socket_Type; Name : in Interfaces.C.int) return Integer;
   --  Return socket option with Integer size.

   procedure Set_Int_Sock_Opt
     (Socket : in Socket_Type; Name : in Interfaces.C.int; Value : Integer);
   --  Return socket option with Integer size.

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

      New_Socket.S := new Socket_Hidden'(FD => Sock);

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
      Res := Sockets.Thin.C_Socket
               (Info.ai_family, Info.ai_socktype, Info.ai_protocol);

      if Res = Sockets.Thin.Failure then
         OSD.FreeAddrInfo (Info);
         Raise_Socket_Error (Std.Errno);
      end if;

      Socket.S := new Socket_Hidden'(FD => Res);

      Set_Non_Blocking_Mode (Socket);

      Res := Sockets.Thin.C_Bind
               (Socket.S.FD,
                Info.ai_addr,
                C.int (Info.ai_addrlen));

      OSD.FreeAddrInfo (Info);

      if Res = Sockets.Thin.Failure then
         Errno := Std.Errno;
         Res := Sockets.Thin.C_Close (Socket.S.FD);
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
      Res := Sockets.Thin.C_Socket
               (Info.ai_family, Info.ai_socktype, Info.ai_protocol);

      if Res = Sockets.Thin.Failure then
         OSD.FreeAddrInfo (Info);
         Raise_Socket_Error (Std.Errno);
      end if;

      Socket.S := new Socket_Hidden'(FD => Res);

      Set_Non_Blocking_Mode (Socket);

      Res := Sockets.Thin.C_Connect
               (Socket.S.FD,
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
            Res := Sockets.Thin.C_Close (Socket.S.FD);
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
   begin
      return Get_Int_Sock_Opt (Socket, OSD.SO_ERROR);
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
      Port  : in Natural;
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
      return Integer (Socket.S.FD);
   end Get_FD;

   ----------------------
   -- Get_Int_Sock_Opt --
   ----------------------

   function Get_Int_Sock_Opt
     (Socket : in Socket_Type; Name : in Interfaces.C.int) return Integer
   is
      use Interfaces;
      use Sockets;
      use type C.int;

      Res : aliased C.int := 0;
      Len : aliased C.int := Res'Size / System.Storage_Unit;

      RC  : constant C.int
        := Thin.C_Getsockopt
             (S       => Socket.S.FD,
              Level   => OSD.SOL_SOCKET,
              Optname => Name,
              Optval  => Res'Address,
              Optlen  => Len'Access);
   begin
      if RC = Thin.Failure then
         Raise_Socket_Error (Errno);
      end if;

      return Integer (Res);
   end Get_Int_Sock_Opt;

   --------------
   -- Get_Port --
   --------------

   function Get_Port (Socket : in Socket_Type) return Positive is
      use GNAT.Sockets.Thin;
      use type Interfaces.C.int;

      Name : aliased Sockaddr_In6;
      Len  : aliased Interfaces.C.int := Name'Size / 8;

   begin
      if C_Getsockname (Socket.S.FD, Name'Address, Len'Access) = Failure then
         Raise_Socket_Error (Errno);
      end if;

      return Positive
               (Swap_Little_Endian (Interfaces.Unsigned_16 (Name.Port)));
   end Get_Port;

   -----------------------------
   -- Get_Receive_Buffer_Size --
   -----------------------------

   function Get_Receive_Buffer_Size (Socket : in Socket_Type) return Natural is
   begin
      return Get_Int_Sock_Opt (Socket, OSD.SO_RCVBUF);
   end Get_Receive_Buffer_Size;

   --------------------------
   -- Get_Send_Buffer_Size --
   --------------------------

   function Get_Send_Buffer_Size (Socket : in Socket_Type) return Natural is
   begin
      return Get_Int_Sock_Opt (Socket, OSD.SO_SNDBUF);
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
      Queue_Size : in Positive := 5)
   is
      use Sockets;
      use Interfaces;
      use type C.int;
   begin
      if Thin.C_Listen (Socket.S.FD, C.int (Queue_Size))
         = Thin.Failure
      then
         Raise_Socket_Error (Errno);
      end if;
   end Listen;

   ---------------
   -- Peer_Addr --
   ---------------

   function Peer_Addr (Socket : in Socket_Type) return String is
      use Sockets;
      use Interfaces;
      use type C.int;
      use type C.short;

      type U8_2 is array (1 .. 2) of Unsigned_8;
      pragma Convention (C, U8_2);

      function Split is new Ada.Unchecked_Conversion (Unsigned_16, U8_2);

      Sin6 : aliased Sockaddr_In6;
      Sin  : aliased Thin.Sockaddr_In;
      pragma Import (C, Sin);
      for Sin'Address use Sin6'Address;

      Len : aliased C.int := Sin6'Size / 8;

   begin
      if Thin.C_Getpeername
           (Socket.S.FD, Sin6'Address, Len'Access) = Thin.Failure
      then
         Raise_Socket_Error (Std.Errno);
      end if;

      if Sin6.Family = OSD.PF_INET then
         return Utils.Image (Integer (Sin.Sin_Addr.S_B1))
            & '.' & Utils.Image (Integer (Sin.Sin_Addr.S_B2))
            & '.' & Utils.Image (Integer (Sin.Sin_Addr.S_B3))
            & '.' & Utils.Image (Integer (Sin.Sin_Addr.S_B4));

      elsif Sin6.Family = OSD.PF_INET6
        or else Sin6.Family = OSD.PF_INET6 * 256 + OSD.PF_INET6
      --  ??? looks like FreeBSD 4.10 error in ipv6 address structure result.
      then
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
                  if Zero and then J = 6 and then Sin6.Addr (J) = 16#FFFF# then
                     --  ::ffff: - IPv4 mapped address on IPv6 protocol.

                     declare
                        W7 : constant U8_2 := Split (Sin6.Addr (7));
                        W8 : constant U8_2 := Split (Sin6.Addr (8));
                     begin
                        return Utils.Image (Integer (W7 (1)))
                                & '.' & Utils.Image (Integer (W7 (2)))
                                & '.' & Utils.Image (Integer (W8 (1)))
                                & '.' & Utils.Image (Integer (W8 (2)));
                     end;
                  end if;

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
         return "unknown protocol family" & C.short'Image (Sin6.Family);
      end if;
   end Peer_Addr;

   ---------------
   -- Peer_Port --
   ---------------

   function Peer_Port (Socket : in Socket_Type) return Positive is
      use GNAT.Sockets.Thin;
      use type Interfaces.C.int;

      Name : aliased Sockaddr_In6;
      Len  : aliased Interfaces.C.int := Name'Size / 8;

   begin
      if C_Getpeername (Socket.S.FD, Name'Address, Len'Access) = Failure then
         Raise_Socket_Error (Errno);
      end if;

      return Positive
               (Swap_Little_Endian (Interfaces.Unsigned_16 (Name.Port)));
   end Peer_Port;

   -------------
   -- Pending --
   -------------

   function Pending (Socket : in Socket_Type) return Stream_Element_Count is
      use Interfaces;
      use type C.int;
      Arg : aliased C.int;
      Res : constant C.int := Sockets.Thin.C_Ioctl
                                (Socket.S.FD,
                                 OSD.FIONREAD,
                                 Arg'Unchecked_Access);
   begin
      if Res = Sockets.Thin.Failure then
         Raise_Socket_Error (Errno);
      end if;

      return Stream_Element_Count (Arg);
   end Pending;

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
      Last   :    out Stream_Element_Offset)
   is
      use Interfaces;
      use Sockets;
      use type C.int;

      Res : C.int;
   begin
      Wait_For (Input, Socket);

      Res := Thin.C_Recv
        (Socket.S.FD,
         Data (Data'First)'Address,
         Data'Length,
         0);

      if Res = Thin.Failure then
         Raise_Socket_Error (Errno);

      elsif Res = 0 then
         --  socket closed by peer.

         Ada.Exceptions.Raise_Exception
           (Socket_Error'Identity,
            Message => "Receive : Socket closed by peer.");
      end if;

      Last := Data'First + Ada.Streams.Stream_Element_Offset (Res - 1);

      if Net.Log.Is_Active then
         Net.Log.Write
           (Direction => Net.Log.Received,
            FD        => Get_FD (Socket),
            Data      => Data,
            Last      => Last);
      end if;
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
              (Socket.S.FD,
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

   ----------------------
   -- Set_Int_Sock_Opt --
   ----------------------

   procedure Set_Int_Sock_Opt
     (Socket : in Socket_Type; Name : in Interfaces.C.int; Value : Integer)
   is
      use Interfaces;
      use Sockets;
      use type C.int;

      Res : constant C.int
        := Thin.C_Setsockopt
             (Socket.S.FD,
              OSD.SOL_SOCKET,
              Name,
              Value'Address,
              Value'Size / System.Storage_Unit);

   begin
      if Res = Thin.Failure then
         Raise_Socket_Error (Errno);
      end if;
   end Set_Int_Sock_Opt;

   ---------------------------
   -- Set_Non_Blocking_Mode --
   ---------------------------

   procedure Set_Non_Blocking_Mode (Socket : in Socket_Type) is
      use Sockets;
      use Interfaces.C;
      Enabled : aliased int := 1;
   begin
      if Thin.C_Ioctl
           (Socket.S.FD,
            OSD.FIONBIO,
            Enabled'Unchecked_Access) /= 0
      then
         Raise_Socket_Error (Errno);
      end if;
   end Set_Non_Blocking_Mode;

   -----------------------------
   -- Set_Receive_Buffer_Size --
   -----------------------------

   procedure Set_Receive_Buffer_Size
     (Socket : in Socket_Type;
      Size   : in Natural) is
   begin
      Set_Int_Sock_Opt (Socket, OSD.SO_RCVBUF, Size);
   end Set_Receive_Buffer_Size;

   --------------------------
   -- Set_Send_Buffer_Size --
   --------------------------

   procedure Set_Send_Buffer_Size
     (Socket : in Socket_Type;
      Size   : in Natural) is
   begin
      Set_Int_Sock_Opt (Socket, OSD.SO_SNDBUF, Size);
   end Set_Send_Buffer_Size;

   --------------
   -- Shutdown --
   --------------

   procedure Shutdown (Socket : in Socket_Type) is
      use Sockets;
      use Interfaces;
      Dummy : C.int;
      pragma Unreferenced (Dummy);
   begin
      Dummy := Thin.C_Shutdown (Socket.S.FD, OSD.SHUT_RDWR);
      Dummy := Thin.C_Close (Socket.S.FD);
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
