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

with GNAT.Sockets;

package body AWS.Net.Std is

   use Ada;
   use GNAT;

   subtype SFD is Sockets.Socket_Type;

   type Socket_Hidden is new Sockets.Socket_Type;

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
     (Socket     : in     Net.Socket_Type'Class;
      New_Socket :    out Socket_Type)
   is
      Sock_Addr : Sockets.Sock_Addr_Type;
   begin
      if New_Socket.S = null then
         New_Socket.S := new Socket_Hidden;
      end if;

      Sockets.Accept_Socket
        (SFD (Socket_Type (Socket).S.all),
         SFD (New_Socket.S.all), Sock_Addr);

      Set_Cache (New_Socket);
   exception
      when E : others =>
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
      Inet_Addr : Sockets.Inet_Addr_Type;
   begin
      if Host = "" then
         Inet_Addr := Sockets.Any_Inet_Addr;
      else
         Inet_Addr := Sockets.Addresses (Sockets.Get_Host_By_Name (Host));
      end if;

      if Socket.S = null then
         Socket.S := new Socket_Hidden;
         Sockets.Create_Socket (SFD (Socket.S.all));
      end if;

      Sockets.Bind_Socket
        (SFD (Socket.S.all),
         (Sockets.Family_Inet, Inet_Addr, Sockets.Port_Type (Port)));
   exception
      when E : others =>
         Raise_Exception (E, "Bind");
   end Bind;

   -------------
   -- Connect --
   -------------

   procedure Connect
     (Socket   :    out Socket_Type;
      Host     : in     String;
      Port     : in     Positive)
   is
      Sock_Addr : Sockets.Sock_Addr_Type;
   begin

      if Socket.S = null then
         Socket.S := new Socket_Hidden;
         Sockets.Create_Socket (SFD (Socket.S.all));
      end if;

      Sock_Addr := (Sockets.Family_Inet,
                    Sockets.Addresses (Sockets.Get_Host_By_Name (Host), 1),
                    Sockets.Port_Type (Port));
      Sockets.Connect_Socket (SFD (Socket.S.all), Sock_Addr);

      Set_Cache (Socket);
   exception
      when E : others =>
         Free (Socket.S);
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
      return Sockets.To_C (SFD (Socket.S.all));
   end Get_FD;

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
      Sockets.Listen_Socket (SFD (Socket.S.all), Queue_Size);
   exception
      when E : others =>
         Raise_Exception (E, "Listen");
   end Listen;

   ---------------
   -- Peer_Addr --
   ---------------

   function Peer_Addr (Socket : in Socket_Type) return String is
   begin
      return Sockets.Image
        (Sockets.Get_Peer_Name (SFD (Socket.S.all)));
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
      return Stream_Element_Array
   is
      Buffer : Stream_Element_Array (1 .. Max);
      Last   : Stream_Element_Count := 0;
   begin
      Sockets.Receive_Socket (SFD (Socket.S.all), Buffer, Last);

      --  Check if socket closed by peer.

      if Last = Buffer'First - 1 then
         Ada.Exceptions.Raise_Exception
           (Socket_Error'Identity,
            Message => "Reseive : Socket closed by peer.");
      end if;

      return Buffer (1 .. Last);
   exception
      when E : others =>
         Raise_Exception (E, "Receive");
   end Receive;

   ----------
   -- Send --
   ----------

   procedure Send
     (Socket : in Socket_Type;
      Data   : in Stream_Element_Array)
   is
      Last   : Stream_Element_Count;
   begin
      Sockets.Send_Socket (SFD (Socket.S.all), Data, Last);

      if Last = Data'First - 1 then
         raise Socket_Error;
      end if;
   exception
      when E : others =>
         Raise_Exception (E, "Send");
   end Send;

   --------------
   -- Shutdown --
   --------------

   procedure Shutdown (Socket : in Socket_Type) is
   begin
      if Socket.S /= null then
         begin
            --  We catch socket exceptions here as we do not want this call to
            --  fail. A shutdown will fail on non connected sockets.
            Sockets.Shutdown_Socket (SFD (Socket.S.all));
         exception
            when Sockets.Socket_Error =>
               null;
         end;

         Sockets.Close_Socket (SFD (Socket.S.all));
      end if;
   exception
      when E : others =>
         Raise_Exception (E, "Shutdown");
   end Shutdown;

begin
   Sockets.Initialize;
end AWS.Net.Std;
