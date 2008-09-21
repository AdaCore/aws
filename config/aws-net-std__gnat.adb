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

with Ada.Strings.Maps;
with Ada.Unchecked_Deallocation;

with Interfaces.C;

with AWS.Net.Log;
with AWS.OS_Lib;

pragma Warnings (Off);

--  Ignore warning about portability of the GNAT.Sockets.Thin because we are
--  using only Socket_Errno which exists on all mains platforms Unix, Windows
--  and VMS.

with GNAT.Sockets.Thin;
with GNAT.Sockets.Constants;

pragma Warnings (On);

with Interfaces.C.Strings;
with System;

package body AWS.Net.Std is

   use GNAT;
   use type Interfaces.C.int;

   Failure : constant Interfaces.C.int := -1;
   --  Declared here as it used to be in GNAT.Sockets.Thin and has been moved
   --  into GNAT.Sockets.Thin.Common as part of a code refactoring. This was
   --  done on 2008/04/09, when this compiler becomes old enough the
   --  Thin.Common definition should be used and this declaration removed.

   type Socket_Hidden is record
      FD : Sockets.Socket_Type := Sockets.No_Socket;
   end record;

   procedure Free is
      new Ada.Unchecked_Deallocation (Socket_Hidden, Socket_Hidden_Access);

   procedure Raise_Exception
     (E       : in Exceptions.Exception_Occurrence;
      Routine : in String;
      Socket  : in Socket_Type);
   pragma No_Return (Raise_Exception);
   --  Raise and log exception Socket_Error with E's message and a reference to
   --  the routine name.

   procedure Raise_Socket_Error (Error : in Integer; Socket : in Socket_Type);
   pragma No_Return (Raise_Socket_Error);

   function Error_Message (Error : in Integer) return String;

   function Get_Inet_Addr (Host : in String) return Sockets.Inet_Addr_Type;
   pragma Inline (Get_Inet_Addr);
   --  Returns the inet address for the given host

   procedure Set_Non_Blocking_Mode (Socket : in Socket_Type);
   --  Set the socket to the non-blocking mode.
   --  AWS is not using blocking sockets internally.

   -------------------
   -- Accept_Socket --
   -------------------

   overriding procedure Accept_Socket
     (Socket     : in     Net.Socket_Type'Class;
      New_Socket : in out Socket_Type)
   is
      Sock_Addr : Sockets.Sock_Addr_Type;
   begin
      if New_Socket.S /= null then
         New_Socket := Socket_Type'(Net.Socket_Type with others => <>);
      end if;

      New_Socket.S := new Socket_Hidden;

      --  Check for Accept_Socket timeout

      Wait_For (Input, Socket);

      Sockets.Accept_Socket
        (Socket_Type (Socket).S.FD, New_Socket.S.FD, Sock_Addr);

      if Net.Log.Is_Event_Active then
         Net.Log.Event (Net.Log.Accept_Socket, New_Socket);
      end if;

      Set_Non_Blocking_Mode (New_Socket);
   exception
      when E : Sockets.Socket_Error =>
         Raise_Exception (E, "Accept_Socket", New_Socket);
   end Accept_Socket;

   ----------
   -- Bind --
   ----------

   overriding procedure Bind
     (Socket        : in out Socket_Type;
      Port          : in     Natural;
      Host          : in     String  := "";
      Reuse_Address : in     Boolean := False)
   is
      use Ada.Strings.Maps;
      Inet_Addr : Sockets.Inet_Addr_Type;
      Created   : Boolean := False;
   begin
      if Socket.S /= null then
         Socket := Socket_Type'(Net.Socket_Type with others => <>);
      end if;

      Socket.S := new Socket_Hidden;

      if Host = "" then
         Inet_Addr := Sockets.Any_Inet_Addr;
      else
         Inet_Addr := Get_Inet_Addr (Host);
      end if;

      Sockets.Create_Socket (Socket.S.FD);
      Created := True;

      Set_Non_Blocking_Mode (Socket);

      if Reuse_Address then
         Sockets.Set_Socket_Option
           (Socket.S.FD, Option => (Sockets.Reuse_Address, Enabled => True));
      end if;

      Sockets.Bind_Socket
        (Socket.S.FD,
         (Sockets.Family_Inet, Inet_Addr, Sockets.Port_Type (Port)));

   exception
      when E : Sockets.Socket_Error | Sockets.Host_Error =>
         if Created then
            Sockets.Close_Socket (Socket.S.FD);
         end if;

         Raise_Exception (E, "Bind", Socket);
   end Bind;

   -------------
   -- Connect --
   -------------

   overriding procedure Connect
     (Socket : in out Socket_Type;
      Host   : in     String;
      Port   : in     Positive;
      Wait   : in     Boolean := True)
   is
      Sock_Addr : Sockets.Sock_Addr_Type;

      Close_On_Exception : Boolean := False;
   begin
      if Socket.S /= null then
         Socket := Socket_Type'(Net.Socket_Type with others => <>);
      end if;

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
                  Raise_Exception (E, "Connect", Socket);
            end case;
      end;

      if Wait then
         declare
            Events : constant Event_Set
              := Net.Wait (Socket, (Output => True, Input => False));

            procedure Raise_Error (Errno : in Integer);

            -----------------
            -- Raise_Error --
            -----------------

            procedure Raise_Error (Errno : in Integer) is
               Msg : constant String := Error_Message (Errno);
            begin
               Log.Error (Socket, Msg);
               Sockets.Close_Socket (Socket.S.FD);
               raise Socket_Error with Msg;
            end Raise_Error;

         begin
            if Events (Error) then
               Raise_Error (Std.Errno (Socket));
            elsif not Events (Output) then
               Raise_Error (Sockets.Constants.ETIMEDOUT);
            end if;
         end;
      end if;

      if Net.Log.Is_Event_Active then
         Net.Log.Event (Net.Log.Connect, Socket);
      end if;
   exception
      when E : Sockets.Socket_Error | Sockets.Host_Error =>
         if Close_On_Exception then
            Sockets.Close_Socket (Socket.S.FD);
         end if;

         Raise_Exception (E, "Connect", Socket);
   end Connect;

   -----------
   -- Errno --
   -----------

   function Errno return Integer is
   begin
      return GNAT.Sockets.Thin.Socket_Errno;
   end Errno;

   overriding function Errno (Socket : in Socket_Type) return Integer is
      use Interfaces;
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

      if RC = Failure then
         Raise_Socket_Error (Errno, Socket);
      end if;

      return Integer (Res);
   end Errno;

   -------------------
   -- Error_Message --
   -------------------

   function Error_Message (Error : in Integer) return String is
      use Interfaces;
   begin
      return '[' & Utils.Image (Error) & "] "
        & C.Strings.Value (Sockets.Thin.Socket_Error_Message (Error));
   end Error_Message;

   ----------
   -- Free --
   ----------

   overriding procedure Free (Socket : in out Socket_Type) is
   begin
      Free (Socket.S);
   end Free;

   --------------
   -- Get_Addr --
   --------------

   overriding function Get_Addr (Socket : in Socket_Type) return String is
      use Sockets;
   begin
      return Image (Get_Socket_Name (Socket.S.FD).Addr);
   exception
      when E : Sockets.Socket_Error =>
         Raise_Exception (E, "Get_Addr", Socket);
   end Get_Addr;

   ------------
   -- Get_FD --
   ------------

   overriding function Get_FD (Socket : in Socket_Type) return Integer is
   begin
      if Socket.S = null then
         return Sockets.To_C (Sockets.No_Socket);
      else
         return Sockets.To_C (Socket.S.FD);
      end if;
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

   overriding function Get_Port (Socket : in Socket_Type) return Positive is
   begin
      return Positive (Sockets.Get_Socket_Name (Socket.S.FD).Port);
   exception
      when E : Sockets.Socket_Error =>
         Raise_Exception (E, "Get_Port", Socket);
   end Get_Port;

   -----------------------------
   -- Get_Receive_Buffer_Size --
   -----------------------------

   overriding function Get_Receive_Buffer_Size
     (Socket : in Socket_Type) return Natural
   is
      use Sockets;
   begin
      return Get_Socket_Option (Socket.S.FD, Name => Receive_Buffer).Size;
   exception
      when E : Sockets.Socket_Error =>
         Raise_Exception (E, "Get_Receive_Buffer_Size", Socket);
   end Get_Receive_Buffer_Size;

   --------------------------
   -- Get_Send_Buffer_Size --
   --------------------------

   overriding function Get_Send_Buffer_Size
     (Socket : in Socket_Type) return Natural
   is
      use Sockets;
   begin
      return Get_Socket_Option (Socket.S.FD, Name => Send_Buffer).Size;
   exception
      when E : Sockets.Socket_Error =>
         Raise_Exception (E, "Get_Send_Buffer_Size", Socket);
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

   overriding procedure Listen
     (Socket     : in Socket_Type;
      Queue_Size : in Positive := 5) is
   begin
      Sockets.Listen_Socket (Socket.S.FD, Queue_Size);
   exception
      when E : Sockets.Socket_Error =>
         Raise_Exception (E, "Listen", Socket);
   end Listen;

   ---------------
   -- Peer_Addr --
   ---------------

   overriding function Peer_Addr (Socket : in Socket_Type) return String is
      use Sockets;
   begin
      return Image (Get_Peer_Name (Socket.S.FD).Addr);
   exception
      when E : Sockets.Socket_Error =>
         Raise_Exception (E, "Peer_Addr", Socket);
   end Peer_Addr;

   ---------------
   -- Peer_Port --
   ---------------

   overriding function Peer_Port (Socket : in Socket_Type) return Positive is
   begin
      return Positive (Sockets.Get_Peer_Name (Socket.S.FD).Port);
   exception
      when E : Sockets.Socket_Error =>
         Raise_Exception (E, "Peer_Port", Socket);
   end Peer_Port;

   -------------
   -- Pending --
   -------------

   overriding function Pending
     (Socket : in Socket_Type) return Stream_Element_Count
   is
      use Interfaces;
      Arg : aliased C.int;
      Res : constant C.int := Sockets.Thin.C_Ioctl
                                (C.int (Get_FD (Socket)),
                                 Sockets.Constants.FIONREAD,
                                 Arg'Unchecked_Access);
   begin
      if Res = Failure then
         Raise_Socket_Error (Errno, Socket);
      end if;

      return Stream_Element_Count (Arg);
   end Pending;

   ---------------------
   -- Raise_Exception --
   ---------------------

   procedure Raise_Exception
     (E       : in Exceptions.Exception_Occurrence;
      Routine : in String;
      Socket  : in Socket_Type)
   is
      Msg : constant String := Routine & " : " & Exception_Message (E);
   begin
      Log.Error (Socket, Message => Msg);
      raise Socket_Error with Msg;
   end Raise_Exception;

   ------------------------
   -- Raise_Socket_Error --
   ------------------------

   procedure Raise_Socket_Error
     (Error : in Integer; Socket : in Socket_Type)
   is
      Msg : constant String := Error_Message (Error);
   begin
      Log.Error (Socket, Message => Msg);
      raise Socket_Error with Msg;
   end Raise_Socket_Error;

   -------------
   -- Receive --
   -------------

   overriding procedure Receive
     (Socket : in     Socket_Type;
      Data   :    out Stream_Element_Array;
      Last   :    out Stream_Element_Offset) is
   begin
      Wait_For (Input, Socket);

      Sockets.Receive_Socket (Socket.S.FD, Data, Last);

      --  Check if socket closed by peer

      if Last = Data'First - 1 then
         raise Socket_Error with "Receive : Socket closed by peer.";
      end if;

      if Net.Log.Is_Write_Active then
         Net.Log.Write
           (Direction => Net.Log.Received,
            Socket    => Socket,
            Data      => Data,
            Last      => Last);
      end if;
   exception
      when E : Sockets.Socket_Error =>
         Raise_Exception (E, "Receive", Socket);
   end Receive;

   ----------
   -- Send --
   ----------

   overriding procedure Send
     (Socket : in     Socket_Type;
      Data   : in     Stream_Element_Array;
      Last   :    out Stream_Element_Offset)
   is
      use Interfaces;

      Errno : Integer;
      RC    : C.int;
   begin
      RC := Sockets.Thin.C_Send
              (C.int (Get_FD (Socket)),
               Data'Address,
               Data'Length,
               OS_Lib.MSG_NOSIGNAL);

      if RC = Failure then
         Errno := Std.Errno;

         if Errno = Sockets.Constants.EWOULDBLOCK then
            if Data'First = Stream_Element_Offset'First then
               Last := Stream_Element_Offset'Last;
            else
               Last := Data'First - 1;
            end if;

            return;

         else
            Raise_Socket_Error (Errno, Socket);
         end if;
      end if;

      if RC = 0 and then Data'First = Stream_Element_Offset'First then
         --  Could not Last := Data'First - 1;

         Last := Stream_Element_Offset'Last;
      else
         Last := Data'First + Stream_Element_Offset (RC) - 1;
      end if;

      if Net.Log.Is_Write_Active then
         Net.Log.Write
           (Direction => Net.Log.Sent,
            Socket    => Socket,
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
         Raise_Exception (E, "Set_Non_Blocking_Mode", Socket);
   end Set_Non_Blocking_Mode;

   -----------------------------
   -- Set_Receive_Buffer_Size --
   -----------------------------

   overriding procedure Set_Receive_Buffer_Size
     (Socket : in Socket_Type;
      Size   : in Natural)
   is
      use Sockets;
   begin
      Set_Socket_Option (Socket.S.FD, Option => (Receive_Buffer, Size));
   exception
      when E : Sockets.Socket_Error =>
         Raise_Exception (E, "Set_Receive_Buffer_Size", Socket);
   end Set_Receive_Buffer_Size;

   --------------------------
   -- Set_Send_Buffer_Size --
   --------------------------

   overriding procedure Set_Send_Buffer_Size
     (Socket : in Socket_Type;
      Size   : in Natural)
   is
      use Sockets;
   begin
      Set_Socket_Option (Socket.S.FD, Option => (Send_Buffer, Size));
   exception
      when E : Sockets.Socket_Error =>
         Raise_Exception (E, "Set_Send_Buffer_Size", Socket);
   end Set_Send_Buffer_Size;

   --------------
   -- Shutdown --
   --------------

   overriding procedure Shutdown
     (Socket : in Socket_Type; How : in Shutmode_Type := Shut_Read_Write)
   is
      To_GNAT : constant array (Shutmode_Type) of Sockets.Shutmode_Type :=
                  (Shut_Read_Write => Sockets.Shut_Read_Write,
                   Shut_Read       => Sockets.Shut_Read,
                   Shut_Write      => Sockets.Shut_Write);
   begin
      if Socket.S /= null then
         if Net.Log.Is_Event_Active then
            Net.Log.Event (Net.Log.Shutdown, Socket);
         end if;

         begin
            --  We catch socket exceptions here as we do not want this call to
            --  fail. A shutdown will fail on non connected sockets.

            Sockets.Shutdown_Socket (Socket.S.FD, To_GNAT (How));
         exception
            when E : Sockets.Socket_Error =>
               Log.Error
                 (Socket,
                  Message => "Shutdown : "
                             & Ada.Exceptions.Exception_Message (E));
         end;

         if How /= Shut_Read_Write then
            return;
         end if;

         declare
            FD : constant Sockets.Socket_Type := Socket.S.FD;
         begin
            --  Avoid any activity under closed socket in other threads.
            --  Reduce risk to send/receive data on other new created sockets.

            Socket.S.FD := Sockets.No_Socket;

            --  ??? In some cases the call above fails because the socket
            --  descriptor is not valid (errno = EABF). This happen on
            --  GNU/Linux only and the problem is not fully understood at this
            --  point. We catch the exception here to hide this problem.

            Sockets.Close_Socket (FD);
         exception
            when E : Sockets.Socket_Error | Constraint_Error =>
               --  Back original socket handle to log

               Socket.S.FD := FD;

               Log.Error
                 (Socket,
                  Message => "Close : "
                             & Ada.Exceptions.Exception_Message (E));
               Socket.S.FD := Sockets.No_Socket;
         end;
      end if;
   end Shutdown;

begin
   pragma Warnings (Off);
   --  The call to Sockets.Initialize has been obsoleted. We keep this call
   --  for compatibility with older compilers.
   --  ??? This call should be removed when GNAT version 6.2 and GPL 2009
   --  will  be out.
   Sockets.Initialize;
end AWS.Net.Std;
