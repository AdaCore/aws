------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2000-2004                          --
--                                ACT-Europe                                --
--                                                                          --
--  Authors: Dmitriy Anisimkov - Pascal Obry                                --
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

with Interfaces.C;

with Sockets.Constants;
with Sockets.Naming;
with Sockets.Thin;

package body AWS.Net.Std is

   use Ada;
   use Interfaces;

   type Socket_Hidden is record
      FD : Sockets.Socket_FD;
   end record;

   procedure Free is
      new Ada.Unchecked_Deallocation (Socket_Hidden, Socket_Hidden_Access);

   procedure Raise_Exception (Errno : in Integer; Routine : in String);
   pragma No_Return (Raise_Exception);
   --  Raise exception Socket_Error with a Message.

   procedure Raise_Exception
     (E       : in Exceptions.Exception_Occurrence;
      Routine : in String);
   pragma No_Return (Raise_Exception);
   --  Raise exception Socket_Error with E's message and a reference to the
   --  routine name.

   procedure Set_Non_Blocking_Mode (Socket : in Socket_Type);
   --  Set the socket to the non-blocking mode.
   --  AWS is not using blocking sockets internally.

   -------------------
   -- Accept_Socket --
   -------------------

   procedure Accept_Socket
     (Socket     : in     Net.Socket_Type'Class;
      New_Socket : in out Socket_Type) is
   begin
      if New_Socket.S = null then
         New_Socket.S := new Socket_Hidden;
      end if;

      --  Check for Accept_Socket timeout.

      Wait_For (Input, Socket);

      Sockets.Accept_Socket
        (Socket_Type (Socket).S.FD, New_Socket.S.FD);

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
      Host   : in     String := "") is
   begin
      if Socket.S = null then
         Socket.S := new Socket_Hidden;
         Sockets.Socket (Socket.S.FD);
         Set_Non_Blocking_Mode (Socket);
      end if;

      Sockets.Bind (Socket.S.FD, Port, Host);
   exception
      when E : Sockets.Socket_Error | Sockets.Naming.Naming_Error =>
         Raise_Exception (E, "Bind");
   end Bind;

   -------------
   -- Connect --
   -------------

   procedure Connect
     (Socket   : in out Socket_Type;
      Host     : in     String;
      Port     : in     Positive)
   is
      Close_On_Exception : Boolean := True;
   begin
      if Socket.S = null then
         Socket.S := new Socket_Hidden;

         Close_On_Exception := False;
         Sockets.Socket (Socket.S.FD);
         Close_On_Exception := True;
      end if;

      Sockets.Connect (Socket.S.FD, Host, Port);

      --  AdaSockets does not support non blocking connect
      --  so we are making socket non-blocking after connect.

      Set_Non_Blocking_Mode (Socket);

      Set_Cache (Socket);
   exception
      when E : Sockets.Connection_Refused
             | Sockets.Socket_Error
             | Sockets.Naming.Naming_Error
      =>
         if Close_On_Exception then
            Sockets.Shutdown (Socket.S.FD);
         end if;

         Free (Socket);
         Raise_Exception (E, "Connect");
   end Connect;

   -----------
   -- Errno --
   -----------

   function Errno return Integer is
   begin
      return Sockets.Thin.Errno;
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
      return Integer (Sockets.Get_FD (Socket.S.FD));
   end Get_FD;

   -----------------------------
   -- Get_Receive_Buffer_Size --
   -----------------------------

   function Get_Receive_Buffer_Size (Socket : in Socket_Type) return Natural is
      use Sockets;
      Size : Natural;
   begin
      Getsockopt (Socket.S.FD, Optname => SO_RCVBUF, Optval => Size);

      return Size;
   exception
      when E : Sockets.Socket_Error =>
         Raise_Exception (E, "Get_Receive_Buffer_Size");
   end Get_Receive_Buffer_Size;

   ---------------------
   -- Get_Send_Buffer --
   ---------------------

   function Get_Send_Buffer_Size (Socket : in Socket_Type) return Natural is
      use Sockets;
      Size : Natural;
   begin
      Getsockopt (Socket.S.FD, Optname => SO_SNDBUF, Optval => Size);

      return Size;
   exception
      when E : Sockets.Socket_Error =>
         Raise_Exception (E, "Get_Send_Buffer_Size");
   end Get_Send_Buffer_Size;

   ---------------
   -- Host_Name --
   ---------------

   function Host_Name return String is
   begin
      return Sockets.Naming.Host_Name;
   end Host_Name;

   ------------
   -- Listen --
   ------------

   procedure Listen
     (Socket     : in Socket_Type;
      Queue_Size : in Positive := 5) is
   begin
      Sockets.Listen (Socket.S.FD, Queue_Size);
   exception
      when E : Sockets.Socket_Error =>
         Raise_Exception (E, "Listen");
   end Listen;

   ---------------
   -- Peer_Addr --
   ---------------

   function Peer_Addr (Socket : in Socket_Type) return String is
   begin
      return Sockets.Naming.Image
        (Sockets.Naming.Address'
           (Sockets.Naming.Get_Peer_Addr (Socket.S.FD)));
   exception
      when E : others =>
         Raise_Exception (E, "Peer_Addr");
   end Peer_Addr;

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

   ---------------------
   -- Raise_Exception --
   ---------------------

   procedure Raise_Exception (Errno : in Integer; Routine : in String) is
   begin
      Ada.Exceptions.Raise_Exception
        (Socket_Error'Identity,
         Routine & " : (error code" & Integer'Image (Errno) & ')');
   end Raise_Exception;

   -------------
   -- Receive --
   -------------

   function Receive
     (Socket : in Socket_Type;
      Max    : in Stream_Element_Count := 4096)
      return Stream_Element_Array is
   begin
      Wait_For (Input, Socket);

      return Sockets.Receive (Socket.S.FD, Max);
   exception
      when E : Sockets.Socket_Error       |
               Sockets.Connection_Closed  |
               Sockets.Connection_Refused => Raise_Exception (E, "Receive");
   end Receive;

   ----------
   -- Send --
   ----------

   procedure Send
     (Socket : in Socket_Type;
      Data   : in Stream_Element_Array)
   is
      use Sockets;
      use type C.int;

      Index : Stream_Element_Offset  := Data'First;
      Rest  : C.int := Data'Length;
      FD    : C.int := Sockets.Get_FD (Socket.S.FD);
      Count : C.int;

   begin
      loop
         Wait_For (Output, Socket);

         Count := Thin.C_Send (FD, Data (Index)'Address, Rest, 0);

         if Count < 0 then
            Raise_Exception (Thin.Errno, "Send");
         end if;

         Rest  := Rest  - Count;
         exit when Rest <= 0;

         Index := Index + Stream_Element_Offset (Count);
      end loop;
   end Send;

   ---------------------------
   -- Set_Non_Blocking_Mode --
   ---------------------------

   procedure Set_Non_Blocking_Mode (Socket : in Socket_Type) is
      use Sockets;
      use Interfaces.C;
      Enabled : aliased int := 1;
   begin
      if Thin.C_Ioctl
           (Get_FD (Socket.S.FD),
            Constants.Fionbio,
            Enabled'Access) /= 0
      then
         Ada.Exceptions.Raise_Exception
           (Socket_Error'Identity, "Set_Non_Blocking_Mode");
      end if;
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
      Setsockopt (Socket.S.FD, Optname => SO_RCVBUF, Optval => Size);
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
      Setsockopt (Socket.S.FD, Optname => SO_SNDBUF, Optval => Size);
   exception
      when E : Sockets.Socket_Error =>
         Raise_Exception (E, "Set_Send_Buffer_Size");
   end Set_Send_Buffer_Size;

   --------------
   -- Shutdown --
   --------------

   procedure Shutdown (Socket : in Socket_Type) is
   begin
      Sockets.Shutdown (Socket.S.FD);
   exception
      when E : others =>
         Raise_Exception (E, "Shutdown");
   end Shutdown;

end AWS.Net.Std;
