------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2005-2017, AdaCore                     --
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

pragma Ada_2012;

--  Waiting on a group of sockets for reading and accept new connections

with Ada.Containers.Doubly_Linked_Lists;

with AWS.Net;
with AWS.Net.Generic_Sets;
with AWS.Utils;

private with Ada.Real_Time;

package AWS.Net.Acceptors is

   type Acceptor_Type is limited private;

   package Socket_Lists is new Containers.Doubly_Linked_Lists (Socket_Access);

   subtype Socket_List is Socket_Lists.List;

   procedure Listen
     (Acceptor            : in out Acceptor_Type;
      Host                : String;
      Port                : Natural;
      Queue_Size          : Positive;
      Family              : Family_Type := Family_Unspec;
      Timeout             : Duration    := Forever;
      First_Timeout       : Duration    := Forever;
      Force_Timeout       : Duration    := Forever;
      Force_First_Timeout : Duration    := Forever;
      Force_Length        : Positive    := Positive'Last;
      Close_Length        : Positive    := Positive'Last;
      Reuse_Address       : Boolean     := False;
      IPv6_Only           : Boolean     := False);
   --  Prepare Acceptor to accept sockets and wait for incoming data from the
   --  given Host and Port. Use Queue_Size for the Listen call.
   --  Timeout is to wait for the next data from the socket, should be longer
   --  than First_Timeout for HTTP protocol handlers. First_Timeout is the
   --  time to wait for data just after a socket is accepted. Force_Timeout
   --  used when the number of sockets exceed Force_Length (generally this
   --  timeout is shorter than the others).
   --  If number of sockets became more then Close_Length, closest to timeout
   --  socket would be closed without timeout condition.

   procedure Add_Listening
     (Acceptor      : in out Acceptor_Type;
      Host          : String;
      Port          : Natural;
      Family        : Family_Type := Family_Unspec;
      Reuse_Address : Boolean     := False;
      IPv6_Only     : Boolean     := False);
   --  Add the binded/listening socket on host, port and protocol family. To be
   --  able to connect web enabled application with others in the internal
   --  network, and then give access for external clients by listening on
   --  externally available address. Generally this is called from a different
   --  task while the Get routine is blocked waiting for a socket.

   procedure Set_Socket_Constructor
     (Acceptor : in out Acceptor_Type; Constructor : Socket_Constructor);

   procedure Get
     (Acceptor : in out Acceptor_Type;
      Socket   : out    Socket_Access;
      On_Error : access procedure (E : Exception_Occurrence) := null);
   --  Returns a socket from the internal socket set which has data to read.
   --  Should not be called simultaneously from different tasks.
   --  On_Error needs to be able to catch Socket_Error on Accept_Socket or
   --  on the Wait on the sockets. Accept_Socket and Wait on sockets could fail
   --  if the server is processing too many keep-alive connections
   --  simultaneously. Acceptor switched into Force timeouts in case of
   --  Accept_Socket or Wait fail. The server could also use the On_Error
   --  callback to decrease the number of simultaneous keep-alive connections.
   --  If On_Error is null, the exception on error is propagated.

   procedure Get
     (Acceptor : in out Acceptor_Type;
      Socket   : out    Socket_Access;
      To_Close : out    Socket_List;
      On_Error : access procedure (E : Exception_Occurrence) := null);
   --  Idem but with output socket list which have to be shutdowned and freed.
   --  It should be done out of critical section if any.

   procedure Shutdown_And_Free (Set : Socket_List);
   --  Use this routine to shutdown and free list of sockets returned from Get
   --  routine above.

   function Server_Socket (Acceptor : Acceptor_Type) return Socket_Type'Class
     with Inline;
   --  Returns main server accepting socket

   function Server_Sockets (Acceptor : Acceptor_Type) return Socket_List;
   --  Returns all listening server sockets

   procedure Give_Back
     (Acceptor : in out Acceptor_Type;
      Socket   : not null access Socket_Type'Class;
      Success  : out Boolean);
   --  Give back socket which has been taken from Get routine above. Generally
   --  this is called from a different task while the Get routine is blocked
   --  waiting for a socket. Socket would not be given back in case of socket
   --  queue size exceed Queue_Size Acceptor property and Success parameter
   --  would return False value in this case.

   procedure Give_Back
     (Acceptor : in out Acceptor_Type;
      Socket   : not null access Socket_Type'Class);
   --  Idem but do not check sockets queue length

   procedure Shutdown (Acceptor : in out Acceptor_Type);
   --  Shutdown all internal sockets. Generally this is called from a
   --  different task while the Get routine is blocked waiting for a
   --  socket.

   function Length (Acceptor : Acceptor_Type) return Natural with Inline;
   --  Return number of sockets in the internal socket set.
   --  Note that this number include server accepting socket
   --  and one service signaling socket.
   --  If the number of socket is 0, it mean that Acceptor either
   --  not initialized or already shutdowned.

private

   protected type Socket_Box (Acceptor : not null access Acceptor_Type) is

      procedure Add
        (S        : not null access Socket_Type'Class;
         Max_Size : Positive;
         Success  : out Boolean);

      entry Get (S : out Socket_Access);

      function Size return Natural;

      procedure Clear;

   private
      Buffer : Socket_List;
   end Socket_Box;

   protected type Server_Sockets_Set is

      procedure Add (S : not null access Socket_Type'Class);

      function Get return Socket_List;

      procedure Clear;

      entry Wait_Empty;

   private
      Sockets : Socket_List;
   end Server_Sockets_Set;

   type Socket_Data_Type is record
      Time  : Real_Time.Time;
      First : Boolean;
   end record;

   package Sets is new Generic_Sets (Socket_Data_Type);

   type Acceptor_Type is tagged limited record
      Set                 : Sets.Socket_Set_Type;
      W_Signal            : access Socket_Type'Class;
      R_Signal            : access Socket_Type'Class;
      Servers             : Server_Sockets_Set;
      Box                 : Socket_Box (Acceptor_Type'Access);
      Index               : Sets.Socket_Count;
      Last                : Sets.Socket_Count;
      Timeout             : Real_Time.Time_Span;
      First_Timeout       : Real_Time.Time_Span;
      Force_Timeout       : Real_Time.Time_Span;
      Force_First_Timeout : Real_Time.Time_Span;
      Force_Length        : Sets.Socket_Count;
      Close_Length        : Sets.Socket_Count;
      Back_Queue_Size     : Positive;
      Semaphore           : Utils.Semaphore;
      Constructor         : Socket_Constructor := Socket'Access;
   end record;

end AWS.Net.Acceptors;
