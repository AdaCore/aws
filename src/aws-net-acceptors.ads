------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2005-2006                          --
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

--  Waiting on a group of sockets for reading and accept new connections

with Ada.Calendar;
with Ada.Exceptions;

with AWS.Net;
with AWS.Net.Generic_Sets;

with AWS.Utils;

package AWS.Net.Acceptors is

   type Acceptor_Type is limited private;

   procedure Listen
     (Acceptor            : in out Acceptor_Type;
      Host                : in     String;
      Port                : in     Positive;
      Queue_Size          : in     Positive;
      Timeout             : in     Duration := Forever;
      First_Timeout       : in     Duration := Forever;
      Force_Timeout       : in     Duration := Forever;
      Force_First_Timeout : in     Duration := Forever;
      Force_Length        : in     Positive := Positive'Last);
   --  Prepare Acceptor to accept sockets and wait for incoming data from the
   --  given Host and Port. Use Queue_Size for the Listen call.
   --  Timeout is to wait for the next data from the socket, should be longer
   --  than First_Timeout for HTTP protocol handlers. First_Timeout is the
   --  time to wait for data just after a socket is accepted. Force_Timeout
   --  used when the number of sockets exceed Force_Length (generally this
   --  timeout is shorter than the others).

   procedure Set_Socket_Constructor
     (Acceptor : in out Acceptor_Type; Constructor : in Socket_Constructor);

   procedure Get
     (Acceptor        : in out Acceptor_Type;
      Socket          : out    Socket_Access;
      On_Accept_Error : access procedure
        (E : in Ada.Exceptions.Exception_Occurrence) := null);
   --  Returns a socket from the internal socket set which has data to read.
   --  Should not be called simultaneously from different tasks.
   --  On_Accept_Error needs to be able to catch Socket_Error on Accept_Socket.
   --  Accept_Socket could fail if the server is processing too many keep-alive
   --  connections simultaneously. Acceptor swithed into Force timeouts in case
   --  of Accept_Socket fail. The server could also use the On_Accept_Error
   --  callback to decrease the number of simultaneous keep-alive connections.
   --  If On_Accept_Error is null, the exception from Accept_Socket is
   --  propagated.

   function Server_Socket
     (Acceptor : in Acceptor_Type) return Socket_Type'Class;
   pragma Inline (Server_Socket);
   --  return server accepting socket. Need only to show server socket FD in
   --  server status page.

   procedure Give_Back
     (Acceptor : in out Acceptor_Type;
      Socket   : in     Socket_Access);
   --  Give back socket which has been taken from Get routine above. Generally
   --  this is called from a different task while the Get routine is blocked
   --  waiting for a socket.

   procedure Shutdown (Acceptor : in out Acceptor_Type);
   --  Shutdown all internal sockets. Generally this is called from a
   --  different task while the Get routine is blocked waiting for a
   --  socket.

   function Length (Acceptor : in Acceptor_Type) return Natural;
   pragma Inline (Length);
   --  Return number of sockets in the internal socket set.
   --  Note that this number include server accepting socket
   --  and one service signaling socket.
   --  If the number of socket is 0, it mean that Acceptor either
   --  not initialized or already shutdowned.

private

   package Mailboxes is new Utils.Mailbox_G (Socket_Access);

   type Socket_Data_Type is record
      Time  : Ada.Calendar.Time;
      First : Boolean;
   end record;

   package Sets is new Generic_Sets (Socket_Data_Type);

   type Acceptor_Type is tagged limited record
      Set                 : Sets.Socket_Set_Type;
      W_Signal            : Socket_Access;
      R_Signal            : Socket_Access;
      Server              : Socket_Access;
      Box                 : Mailboxes.Mailbox (8);
      Index               : Sets.Socket_Count;
      Last                : Sets.Socket_Count;
      Timeout             : Duration;
      First_Timeout       : Duration;
      Force_Timeout       : Duration;
      Force_First_Timeout : Duration;
      Force_Length        : Sets.Socket_Count;
      Constructor         : Socket_Constructor := Socket'Access;
   end record;

end AWS.Net.Acceptors;
