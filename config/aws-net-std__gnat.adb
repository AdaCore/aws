------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2000-2005                          --
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

--  $Id$

with Ada.Exceptions;
with Ada.Strings.Maps;
with Ada.Unchecked_Deallocation;

with AWS.Net.Log;

pragma Warnings (Off);

--  Ignore warning about portability of the GNAT.Sockets.Thin
--  because we are using only Socket_Errno and it is exists at all main
--  platforms Unix, Windows and VMS.

with GNAT.Sockets.Thin;
with GNAT.Sockets.Constants;

pragma Warnings (On);

with Interfaces.C.Strings;
with System;

package body AWS.Net.Std is

   use Ada;
   use GNAT;

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

   function Get_Inet_Addr (Host : in String) return Sockets.Inet_Addr_Type;
   pragma Inline (Get_Inet_Addr);
   --  Returns the inet address for the given host.

   procedure Set_Non_Blocking_Mode (Socket : in Socket_Type);
   --  Set the socket to the non-blocking mode.
   --  AWS is not using blocking sockets internally.

   -------------------
   -- Accept_Socket --
   -------------------

   procedure Accept_Socket
     (Socket     : in     Net.Socket_Type'Class;
      New_Socket : in out Socket_Type)
   is
      Sock_Addr : Sockets.Sock_Addr_Type;
   begin
      New_Socket.S := new Socket_Hidden;

      --  Check for Accept_Socket timeout.

      Wait_For (Input, Socket);

      Sockets.Accept_Socket
        (Socket_Type (Socket).S.FD,
         New_Socket.S.FD, Sock_Addr);

      Set_Non_Blocking_Mode (New_Socket);

      Set_Cache (New_Socket);
   exception
      when E : Sockets.Socket_Error =>
         Free (New_Socket);
         Raise_Exception (E, "Accept_Socket");
   end Accept_Socket;

   ----------
   -- Bind --
   ----------

   procedure Bind
     (Socket : in out Socket_Type;
      Port   : in     Natural;
      Host   : in     String := "")
   is
      use Ada.Strings.Maps;
      Inet_Addr : Sockets.Inet_Addr_Type;
      Created : Boolean := False;
   begin
      Socket.S := new Socket_Hidden;

      if Host = "" then
         Inet_Addr := Sockets.Any_Inet_Addr;
      else
         Inet_Addr := Get_Inet_Addr (Host);
      end if;

      Sockets.Create_Socket (Socket.S.FD);
      Created := True;

      Set_Non_Blocking_Mode (Socket);

      Sockets.Bind_Socket
        (Socket.S.FD,
         (Sockets.Family_Inet, Inet_Addr, Sockets.Port_Type (Port)));

   exception
      when E : Sockets.Socket_Error | Sockets.Host_Error =>
         if Created then
            Sockets.Close_Socket (Socket.S.FD);
         end if;

         Free (Socket);

         Raise_Exception (E, "Bind");
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
      Sock_Addr : Sockets.Sock_Addr_Type;

      Close_On_Exception : Boolean := False;
   begin
      Socket.S := new Socket_Hidden;

      Sockets.Create_Socket (Socket.S.FD);
      Close_On_Exception := True;

      Sock_Addr := (Sockets.Family_Inet,
                    Get_Inet_Addr (Host),
                    Sockets.Port_Type (Port));

      Set_Non_Blocking_Mode (Socket);

      declare
         use GNAT.Sockets;
      begin
         Connect_Socket (Socket.S.FD, Sock_Addr);
      exception
         when E : GNAT.Sockets.Socket_Error =>
            --  Ignore EWOULDBLOCK and EINPROGRESS errors, because we are
            --  using none blocking connect.
            --  ??? Note, we should change this when GNAT will support
            --  Non-blocking connect.

            case Resolve_Exception (E) is
               when Operation_Now_In_Progress
                    | Resource_Temporarily_Unavailable => null;
               when others =>
                  Sockets.Close_Socket (Socket.S.FD);
                  Free (Socket);

                  Raise_Exception (E, "Connect");
            end case;
      end;

      if Wait then
         declare
            Events : constant Event_Set
              := Net.Wait (Socket, (Output => True, Input => False));

            procedure Raise_Error (Errno : Integer);

            -----------------
            -- Raise_Error --
            -----------------

            procedure Raise_Error (Errno : Integer) is
            begin
               Sockets.Close_Socket (Socket.S.FD);
               Free (Socket);
               Raise_Socket_Error (Errno);
            end Raise_Error;

         begin
            if Events (Error) then
               Raise_Error (Std.Errno (Socket));
            elsif not Events (Output) then
               Raise_Error (Sockets.Constants.ETIMEDOUT);
            end if;
         end;
      end if;

      Set_Cache (Socket);
   exception
      when E : Sockets.Socket_Error | Sockets.Host_Error =>
         if Close_On_Exception then
            Sockets.Close_Socket (Socket.S.FD);
         end if;

         Free (Socket);
         Raise_Exception (E, "Connect");
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
               Level   => Constants.SOL_SOCKET,
               Optname => Constants.SO_ERROR,
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

   ------------
   -- Get_FD --
   ------------

   function Get_FD (Socket : in Socket_Type) return Integer is
   begin
      return Sockets.To_C (Socket.S.FD);
   end Get_FD;

   -------------------
   -- Get_Inet_Addr --
   -------------------

   function Get_Inet_Addr (Host : in String) return Sockets.Inet_Addr_Type is
      use Strings.Maps;
      IP : constant Character_Set := To_Set ("0123456789.");
   begin
      if Is_Subset (To_Set (Host), IP) then
         --  Only numbers, this is an IP address
         return Sockets.Inet_Addr (Host);
      else
         return Sockets.Addresses (Sockets.Get_Host_By_Name (Host), 1);
      end if;
   end Get_Inet_Addr;

   --------------
   -- Get_Port --
   --------------

   function Get_Port (Socket : in Socket_Type) return Positive is
   begin
      return Positive (Sockets.Get_Socket_Name (Socket.S.FD).Port);
   exception
      when E : Sockets.Socket_Error =>
         Raise_Exception (E, "Get_Port");
   end Get_Port;

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
   begin
      return Image (Get_Peer_Name (Socket.S.FD).Addr);
   exception
      when E : Sockets.Socket_Error =>
         Raise_Exception (E, "Peer_Addr");
   end Peer_Addr;

   ---------------
   -- Peer_Port --
   ---------------

   function Peer_Port (Socket : in Socket_Type) return Positive is
   begin
      return Positive (Sockets.Get_Peer_Name (Socket.S.FD).Port);
   exception
      when E : Sockets.Socket_Error =>
         Raise_Exception (E, "Peer_Port");
   end Peer_Port;

   -------------
   -- Pending --
   -------------

   function Pending (Socket : in Socket_Type) return Stream_Element_Count is
      use Interfaces;
      use type C.int;
      Arg : aliased C.int;
      Res : constant C.int := Sockets.Thin.C_Ioctl
                                (C.int (Get_FD (Socket)),
                                 Sockets.Constants.FIONREAD,
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

         if Errno = Sockets.Constants.EWOULDBLOCK then
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

begin
   Sockets.Initialize;
end AWS.Net.Std;
