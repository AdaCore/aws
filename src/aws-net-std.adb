------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2000-2002                          --
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

with Sockets.Naming;

package body AWS.Net.Std is

   use Ada;

   subtype SFD is Sockets.Socket_FD;

   type Socket_Hidden is new Sockets.Socket_FD with null record;

   procedure Free is
      new Ada.Unchecked_Deallocation (Socket_Hidden, Socket_Hidden_Access);

   procedure Raise_Exception
     (E       : in Exceptions.Exception_Occurrence;
      Routine : in String);
   pragma No_Return (Raise_Exception);
   --  Raise exception Socket_Error with E's message and a reference to the
   --  routine name.

   -------------------
   -- Accept_Socket --
   -------------------

   procedure Accept_Socket
     (Socket     : in     Socket_Type;
      New_Socket :    out Net.Socket_Type'Class)
   is
      pragma Warnings (Off, New_Socket);
   begin
      Sockets.Accept_Socket
        (SFD (Socket.S.all), SFD (Socket_Type (New_Socket).S.all));
   exception
      when E : others =>
         Raise_Exception (E, "Accept_Socket");
   end Accept_Socket;

   ------------
   -- Assign --
   ------------

   procedure Assign
     (Left  : in out Socket_Type;
      Right : in     Net.Socket_Type'Class) is
   begin
      Free (Left.S);
      Left.S := new Socket_Hidden'(Socket_Type (Right).S.all);
   end Assign;

   ----------
   -- Bind --
   ----------

   procedure Bind
     (Socket : in Socket_Type;
      Port   : in Natural;
      Host   : in String := "") is
   begin
      Sockets.Bind (SFD (Socket.S.all), Port, Host);
   exception
      when E : others =>
         Raise_Exception (E, "Bind");
   end Bind;

   -------------
   -- Connect --
   -------------

   procedure Connect
     (Socket   : in Socket_Type;
      Host     : in String;
      Port     : in Positive) is
   begin
      Sockets.Connect (SFD (Socket.S.all), Host, Port);
   exception
      when E : others =>
         Raise_Exception (E, "Connect");
   end Connect;

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
      return Integer (Sockets.Get_FD (SFD (Socket.S.all)));
   end Get_FD;

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
      Sockets.Listen (SFD (Socket.S.all), Queue_Size);
   exception
      when E : others =>
         Raise_Exception (E, "Listen");
   end Listen;

   ---------------
   -- Peer_Addr --
   ---------------

   function Peer_Addr (Socket : in Socket_Type) return String is
   begin
      return Sockets.Naming.Image
        (Sockets.Naming.Address'
           (Sockets.Naming.Get_Peer_Addr (SFD (Socket.S.all))));
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

   -------------
   -- Receive --
   -------------

   function Receive
     (Socket : in Socket_Type;
      Max    : in Stream_Element_Count := 4096)
      return Stream_Element_Array is
   begin
      return Sockets.Receive (SFD (Socket.S.all), Max);
   exception
      when E : others =>
         Raise_Exception (E, "Receive");
   end Receive;

   ----------
   -- Send --
   ----------

   procedure Send
     (Socket : in Socket_Type;
      Data   : in Stream_Element_Array) is
   begin
      Sockets.Send (SFD (Socket.S.all), Data);
   exception
      when E : others =>
         Raise_Exception (E, "Send");
   end Send;

   --------------
   -- Shutdown --
   --------------

   procedure Shutdown (Socket : in Socket_Type) is
   begin
      Sockets.Shutdown (SFD (Socket.S.all));
   exception
      when E : others =>
         Raise_Exception (E, "Shutdown");
   end Shutdown;

   ------------
   -- Socket --
   ------------

   function Socket return Socket_Access is
      Sock : Socket_Access;
   begin
      Sock                     := new Socket_Type;
      Socket_Type (Sock.all).S := new Socket_Hidden;
      Sockets.Socket (SFD (Socket_Type (Sock.all).S.all));

      return Sock;
   exception
      when E : others =>
         Raise_Exception (E, "Socket");
   end Socket;

end AWS.Net.Std;
