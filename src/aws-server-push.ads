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

--  Package to support Server Push feature. This is only supported by Netscape
--  browsers. It will not work with Microsoft Internet Explorer.
--  For Microsoft Internet Explorer complementary active components
--  should be used like java applets or ActiveX controls.

with AWS.Net.Stream_IO;

with Table_Of_Strings_And_Static_Values_G;

generic

   type Client_Output_Type (<>) is private;
   --  Data type client want to send through server push.

   type Stream_Output_Type (<>) is private;
   --  Data type to be sent through the socket stream.

   type Client_Environment is private;
   --  Data type to keep client context. This context will be passed to the
   --  convertion routine below.

   with function To_Stream_Output
     (Output : in Client_Output_Type;
      Client : in Client_Environment)
     return Stream_Output_Type;
   --  Function used for convert Client_Output_Type to Stream_Output_Type.
   --  This is used by the server to prepare the data to be sent to the
   --  clients.

package AWS.Server.Push is

   Client_Gone : exception;
   --  Raised when a client is not responding.

   Closed : exception;
   --  Raised when trying to register to a closed push server.

   Duplicate_Client_ID : exception;
   --  Raised in trying to register an already registered client.

   type Object is limited private;
   --  This is the push server object. A push server has two modes, either it
   --  is Open or Closed. When open it will send data to registered
   --  clients. No data will be sent to registered client if the server is
   --  Closed.

   type Mode is (Plain, Multipart, Chunked);
   --  Describeed the mode to communicate with the client.
   --  Plain     : no transformation is done, the data are sent as-is
   --  Multipart : data are MIME encoded.
   --  Chuncked  : data are chunked, a piece of data is sent in small pieces.

   subtype Client_Key is String;
   --  The Client ID key representation. In a server each client must have a
   --  uniq ID. This ID is used for registration and for sending data to
   --  specific client.

   procedure Register
     (Server            : in out Object;
      Client_ID         : in     Client_Key;
      Socket            : in     Net.Socket_Type'Class;
      Environment       : in     Client_Environment;
      Init_Data         : in     Client_Output_Type;
      Init_Content_Type : in     String             := "";
      Kind              : in     Mode               := Plain;
      Close_Duplicate   : in     Boolean            := False);
   --  Add client identified by Client_ID to the server subscription
   --  list and send the Init_Data (as a Data_Content_Type mime content) to
   --  him. After registering this client will be able to receive pushed data
   --  from the server in brodcasting mode. If Close_Duplicate is True and
   --  Client_ID is already registered into the list then old one will be
   --  unregistered first (no exception will be raised).

   procedure Register
     (Server          : in out Object;
      Client_ID       : in     Client_Key;
      Socket          : in     Net.Socket_Type'Class;
      Environment     : in     Client_Environment;
      Kind            : in     Mode               := Plain;
      Close_Duplicate : in     Boolean            := False);
   --  Same as above but without sending initial data.

   procedure Unregister
     (Server       : in out Object;
      Client_ID    : in     Client_Key;
      Close_Socket : in     Boolean    := True);
   --  Removes client Client_ID from server subscription list. The associated
   --  client's socket will be closed if Close_Socket is True. No exception is
   --  raised if Client_ID was not registered.

   procedure Unregister_Clients
     (Server        : in out Object;
      Close_Sockets : in     Boolean := True);
   --  Remove all registered clients from the server. Closes if Close_Sockets
   --  is set to True (default) otherwise the sockets remain open. After this
   --  call the sever will still in running mode. Does nothing if there is no
   --  client registered.

   procedure Send_To
     (Server       : in out Object;
      Client_ID    : in     Client_Key;
      Data         : in     Client_Output_Type;
      Content_Type : in     String             := "");
   --  Push data to a specified client identified by Client_ID.

   procedure Send
     (Server       : in out Object;
      Data         : in     Client_Output_Type;
      Content_Type : in     String             := "");
   --  Push data to every client (broadcast) subscribed to the server.

   generic
      with procedure Client_Gone (Client_ID : in String);
   procedure Send_G
     (Server       : in out Object;
      Data         : in     Client_Output_Type;
      Content_Type : in     String             := "");
   --  Push data to every client (broadcast) subscribed to the server.
   --  Call Client_Gone for each client with broken socket.

   function Count (Server : in Object) return Natural;
   --  Returns the number of registered clients in the server.

   function Is_Open (Server : in Object) return Boolean;
   --  Return True if the server is open, meaning server is still running,
   --  ready to accept client's registration and still sending data to
   --  clients.

   --  Shutdown routines put the server in a Closed mode. The routines below
   --  provides a way to eventually close the socket, to send some
   --  finalisation data.

   procedure Shutdown
     (Server        : in out Object;
      Close_Sockets : in     Boolean := True);
   --  Unregisted all clients and close all associated connections (socket) if
   --  Close_Socket is True. The server will be in Closed mode. After this
   --  call any client trying to register will get the Closed exception. It is
   --  possible to reactivate the server with Restart.

   procedure Shutdown
     (Server             : in out Object;
      Final_Data         : in     Client_Output_Type;
      Final_Content_Type : in     String             := "");
   --  Idem as above but it send Final_Data (as a Data_Content_Type mime
   --  content) before closing connections.

   procedure Shutdown_If_Empty
     (Server : in out Object;
      Open   :    out Boolean);
   --  Server will be shutdown (close mode) if there is no more active clients
   --  (Count = 0). Returns new server status in Open (Open will be True if
   --  server is in Open mode and False otherwise). After this call any client
   --  trying to register will get the Closed exception. It is possible to
   --  reactivate the server with Restart.

   procedure Restart (Server : in out Object);
   --  Set server to Open mode. Server will again send data to registered
   --  clients. It does nothing if server was already open.

private

   subtype Stream_Access is AWS.Net.Stream_IO.Socket_Stream_Access;

   type Client_Holder is record
      Stream      : Stream_Access;
      Kind        : Mode;
      Environment : Client_Environment;
   end record;

   package Table is new Table_Of_Strings_And_Static_Values_G
     (Character_Type => Character,
      String_Type    => String,
      Less           => "<",
      Equals         => "=",
      Value_Type     => Client_Holder);

   protected type Object is

      function Count return Natural;
      --  Returns the number of registered client.

      procedure Unregister_Clients (Close_Sockets : in Boolean);
      --  Unregister al clients, close associated socket if Close_Socket is
      --  set to True.

      procedure Shutdown_If_Empty (Open : out Boolean);
      --  See above.

      procedure Restart;
      --  See above.

      procedure Shutdown
        (Close_Sockets : in Boolean);
      --  See above.

      procedure Shutdown
        (Final_Data        : in Client_Output_Type;
         Final_Content_Type : in String);
      --  See above.

      procedure Register
        (Client_ID       : in Client_Key;
         Holder          : in Client_Holder;
         Close_Duplicate : in Boolean);
      --  See above.

      procedure Register
        (Client_ID         : in Client_Key;
         Holder            : in Client_Holder;
         Init_Data         : in Client_Output_Type;
         Init_Content_Type : in String;
         Close_Duplicate   : in Boolean);
      --  See above.

      procedure Send_To
        (Client_ID    : in Client_Key;
         Data         : in Client_Output_Type;
         Content_Type : in String);
      --  See above.

      procedure Send
        (Data         : in     Client_Output_Type;
         Content_Type : in     String;
         Unregistered : in out Table.Table_Type);
      --  Send Data to all clients registered. Unregistered will contain a
      --  list of clients that have not responded to the request. These
      --  clients have been removed from the list of registered client.

      procedure Unregister
        (Client_ID    : in Client_Key;
         Close_Socket : in Boolean);
      --  See above.

      function Is_Open return Boolean;
      --  See above.

   private
      Container : Table.Table_Type;
      Open      : Boolean := True;
   end Object;

end AWS.Server.Push;
