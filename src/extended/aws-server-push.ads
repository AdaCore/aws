------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2000-2017, AdaCore                     --
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

--  Package to support Server Push feature. This is only supported by Netscape
--  browsers. It will not work with Microsoft Internet Explorer.
--  For Microsoft Internet Explorer complementary active components
--  should be used like java applets or ActiveX controls.

with Ada.Calendar;
with Ada.Streams;
with Ada.Strings.Unbounded;

with AWS.Containers.Tables;
with AWS.Default;
with AWS.Net;

with System;

private with Ada.Containers.Indefinite_Hashed_Sets;
private with Ada.Containers.Indefinite_Hashed_Maps;
private with Ada.Containers.Indefinite_Doubly_Linked_Lists;
private with Ada.Strings.Hash;

generic

   type Client_Output_Type (<>) is private;
   --  Data type client want to send through server push

   type Client_Environment is private;
   --  Data type to keep client context. This context will be passed to the
   --  conversion routine below.

   with function To_Stream_Array
     (Output : Client_Output_Type;
      Client : Client_Environment) return Ada.Streams.Stream_Element_Array;
   --  Function used for convert Client_Output_Type to Stream_Output_Type.
   --  This is used by the server to prepare the data to be sent to the
   --  clients.

package AWS.Server.Push is

   use Ada;
   use Ada.Streams;
   use Ada.Strings.Unbounded;

   Client_Gone : exception;
   --  Raised when a client is not responding

   Closed : exception;
   --  Raised when trying to register to a closed push server

   Duplicate_Client_Id : exception;
   --  Raised in trying to register an already registered client

   type Object is limited private;
   --  This is the push server object. A push server has two modes, either it
   --  is Open or Closed. When open it will send data to registered
   --  clients. No data will be sent to registered client if the server is
   --  Closed.

   type Mode is (Plain, Multipart, Chunked);
   --  Described the mode to communicate with the client.
   --  Plain     : no transformation is done, the data are sent as-is
   --  Multipart : data are MIME encoded.
   --  Chuncked  : data are chunked, a piece of data is sent in small pieces.

   subtype Client_Key is String;
   --  The Client Id key representation. In a server each client must have a
   --  uniq ID. This Id is used for registration and for sending data to
   --  specific client.

   type Wait_Counter_Type is mod System.Max_Binary_Modulus;

   subtype Group_Set is Containers.Tables.VString_Array;

   Empty_Group : constant Group_Set := (1 .. 0 => Null_Unbounded_String);

   procedure Register
     (Server            : in out Object;
      Client_Id         : Client_Key;
      Socket            : Net.Socket_Access;
      Environment       : Client_Environment;
      Init_Data         : Client_Output_Type;
      Init_Content_Type : String             := "";
      Kind              : Mode               := Plain;
      Duplicated_Age    : Duration           := Duration'Last;
      Groups            : Group_Set          := Empty_Group;
      Timeout           : Duration           := Default.Send_Timeout);
   --  Add client identified by Client_Id to the server subscription
   --  list and send the Init_Data (as a Init_Content_Type mime content) to
   --  him. After registering this client will be able to receive pushed data
   --  from the server in broadcasting mode.
   --  If Duplicated_Age less than age of the already registered same Client_Id
   --  then old one will be unregistered first (no exception will be raised).
   --  The Timeout is not for socket send timeout, but for internal waiting for
   --  write availability timeout.

   procedure Register
     (Server            : in out Object;
      Client_Id         : Client_Key;
      Socket            : Net.Socket_Type'Class;
      Environment       : Client_Environment;
      Init_Data         : Client_Output_Type;
      Init_Content_Type : String             := "";
      Kind              : Mode               := Plain;
      Duplicated_Age    : Duration           := Duration'Last;
      Groups            : Group_Set          := Empty_Group;
      Timeout           : Duration           := Default.Send_Timeout);
   --  Same as above but with Socket_Type'Class parameter.
   --  Is not recommended, use above one with Socket_Access parameter.

   procedure Register
     (Server         : in out Object;
      Client_Id      : Client_Key;
      Socket         : Net.Socket_Type'Class;
      Environment    : Client_Environment;
      Content_Type   : String             := "";
      Kind           : Mode               := Plain;
      Duplicated_Age : Duration           := Duration'Last;
      Groups         : Group_Set          := Empty_Group;
      Timeout        : Duration           := Default.Send_Timeout);
   --  Same as above but without sending initial data.
   --  Content_Type applicable only when Kind parameter is Plain or Chunked,
   --  in Multipart server push mode each server push message would have own
   --  Content_Type defined.
   --  Is not recommended, use above one with Socket_Access parameter.

   procedure Unregister
     (Server       : in out Object;
      Client_Id    : Client_Key;
      Close_Socket : Boolean    := True);
   --  Removes client Client_Id from server subscription list. The associated
   --  client's socket will be closed if Close_Socket is True. No exception is
   --  raised if Client_Id was not registered.

   procedure Unregister_Clients
     (Server : in out Object; Close_Sockets : Boolean := True);
   --  Remove all registered clients from the server. Closes if Close_Sockets
   --  is set to True (default) otherwise the sockets remain open. After this
   --  call the sever will still in running mode. Does nothing if there is no
   --  client registered.

   procedure Subscribe
     (Server : in out Object; Client_Id : Client_Key; Group_Id : String);
   --  Subscribe client to the group

   procedure Subscribe_Copy
     (Server : in out Object; Source : String; Target : String);
   --  Subscribe everybody in the group Source to the group Target.
   --  If Source is empty then subscribe all clients to the group Target.

   procedure Unsubscribe
     (Server : in out Object; Client_Id : Client_Key; Group_Id : String);
   --  Remove group from client's group list

   procedure Unsubscribe_Copy
     (Server : in out Object; Source : String; Target : String);
   --  Unsubscribe everybody in the group Source from the group Target.
   --  If Source is empty then unsubscribe all clients from the group Target.

   procedure Send_To
     (Server       : in out Object;
      Client_Id    : Client_Key;
      Data         : Client_Output_Type;
      Content_Type : String             := "";
      Thin_Id      : String             := "");
   --  Push data to a specified client identified by Client_Id
   --  Thin_Id is to be able to replace messages in the send client queue
   --  with the newer one with the same Thin_Id.

   procedure Send
     (Server       : in out Object;
      Data         : Client_Output_Type;
      Group_Id     : String             := "";
      Content_Type : String             := "";
      Thin_Id      : String             := "";
      Client_Gone  : access procedure (Client_Id : String) := null);
   --  Push data to group of clients (broadcast) subscribed to the server.
   --  If Group_Id is empty, data transferred to each client.
   --  Call Client_Gone for each client with broken socket.
   --  Thin_Id is to be able to replace messages in the send client queue
   --  with the newer one with the same Thin_Id.

   generic
      with procedure Client_Gone (Client_Id : String);
   procedure Send_G
     (Server       : in out Object;
      Data         : Client_Output_Type;
      Group_Id     : String             := "";
      Content_Type : String             := "";
      Thin_Id      : String             := "");
   --  Same like before, but generic for back compatibility

   function Count (Server : Object) return Natural;
   --  Returns the number of registered clients in the server

   procedure Info
     (Server  : in out Object;
      Clients : out Natural;
      Groups  : out Natural;
      Process : access procedure
                  (Client_Id   : Client_Key;
                   Address     : String;
                   State       : String;
                   Environment : Client_Environment;
                   Kind        : Mode;
                   Groups      : Group_Set) := null);
   --  Returns the number of registered clients and groups in the server.
   --  Call Process routine for each client if defined.
   --  Test internal integrity.

   function Is_Open (Server : Object) return Boolean;
   --  Return True if the server is open, meaning server is still running,
   --  ready to accept client's registration and still sending data to
   --  clients.

   --  Shutdown routines put the server in a Closed mode. The routines below
   --  provides a way to eventually close the socket, to send some
   --  finalization data.

   procedure Shutdown
     (Server : in out Object; Close_Sockets : Boolean := True);
   --  Unregistered all clients and close all associated connections (socket)
   --  if Close_Socket is True. The server will be in Closed mode. After this
   --  call any client trying to register will get the Closed exception. It is
   --  possible to reactivate the server with Restart.

   procedure Shutdown
     (Server             : in out Object;
      Final_Data         : Client_Output_Type;
      Final_Content_Type : String             := "");
   --  Idem as above but it send Final_Data (as a Data_Content_Type mime
   --  content) before closing connections.

   procedure Shutdown_If_Empty (Server : in out Object; Open : out Boolean);
   --  Server will be shutdown (close mode) if there is no more active clients
   --  (Count = 0). Returns new server status in Open (Open will be True if
   --  server is in Open mode and False otherwise). After this call any client
   --  trying to register will get the Closed exception. It is possible to
   --  reactivate the server with Restart.

   procedure Restart (Server : in out Object);
   --  Set server to Open mode. Server will again send data to registered
   --  clients. It does nothing if server was already open.

   procedure Info
     (Size        : out Natural;
      Max_Size    : out Natural;
      Max_Size_DT : out Calendar.Time;
      Counter     : out Wait_Counter_Type);
   --  Size would return number of currently waiting sockets.
   --  Counter would return total number of waited sockets from start.

   function Wait_Send_Completion (Timeout : Duration) return Boolean;
   --  Wait for all data sending in all server_push objects of the current
   --  package instance.
   --  Return True if wait successful. False in timeout.

   type Error_Handler is not null access procedure (Message : String);

   procedure Set_Internal_Error_Handler (Handler : Error_Handler);
   --  Set the handler of the internal fatal errors

private

   package Group_Sets is new Ada.Containers.Indefinite_Hashed_Sets
     (String, Ada.Strings.Hash, Equivalent_Elements => "=");
   --  Package instance to keep each client subscribed groups

   type Message_Type
     (Size : Stream_Element_Count; Thin_Size : Natural) is
   record
      Data : Stream_Element_Array (1 .. Size);
      Thin : String (1 .. Thin_Size);
   end record;

   package Chunk_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists (Message_Type);

   package Thin_Indexes is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => String,
      Element_Type    => Chunk_Lists.Cursor,
      Hash            => Strings.Hash,
      Equivalent_Keys => "=",
      "="             => Chunk_Lists."=");

   type Client_Holder;

   type Client_Holder_Access is access all Client_Holder;

   package Tables is new Ada.Containers.Indefinite_Hashed_Maps
     (String, Client_Holder_Access, Strings.Hash, "=");

   type Map_Access is access all Tables.Map;

   package Group_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (String, Map_Access, Strings.Hash, "=");

   subtype Group_Map is Group_Maps.Map;

   protected type Object is

      function Count return Natural;
      --  Returns the number of registered client

      procedure Unregister_Clients (Queue : out Tables.Map; Open : Boolean);
      --  Unregister all clients, close associated sockets if Close_Socket is
      --  set to True.

      procedure Shutdown_If_Empty (Open : out Boolean);
      --  See above

      procedure Restart;
      --  See above

      procedure Shutdown
        (Final_Data         : Client_Output_Type;
         Final_Content_Type : String;
         Queue              : out Tables.Map);
      --  See above

      procedure Register
        (Client_Id      : Client_Key;
         Holder         : in out Client_Holder_Access;
         Duplicated     : out Client_Holder_Access;
         Duplicated_Age : Duration;
         Ext_Sock_Alloc : Boolean);
      --  See above.
      --  Holder would be released in case of registration failure.

      procedure Send_To
        (Client_Id    : Client_Key;
         Data         : Client_Output_Type;
         Content_Type : String;
         Thin_Id      : String;
         Holder       : out Client_Holder_Access);
      --  Holder out parameter not null mean that we have to convert Data into
      --  Stream_Element_Array, put it into socket and send the socket into
      --  waiter.

      procedure Send
        (Data         : Client_Output_Type;
         Group_Id     : String;
         Content_Type : String;
         Thin_Id      : String;
         Queue        : out Tables.Map);
      --  Send Data to all clients registered.
      --  Queue would contain client holders available to send data or those
      --  failed on the write waiting state.

      procedure Get_Data
        (Holder : Client_Holder_Access;
         Data   : out Stream_Element_Array;
         Last   : out Stream_Element_Offset);
      --  Return data for the Waiter task.
      --  Could be called only for the write busy client.
      --  If no data to send client become not write busy.

      procedure Unregister
        (Client_Id : Client_Key; Holder : out Client_Holder_Access);
      --  Unregister client and return its holder

      procedure Waiter_Error
        (Holder : Client_Holder_Access; Message : String);
      --  Waiter task would call it on socket error

      function Is_Open return Boolean;
      --  See above

      procedure Subscribe (Client_Id : Client_Key; Group_Id : String);
      --  See above

      procedure Subscribe_Copy (Source : String; Target : String);
      --  See above

      procedure Unsubscribe (Client_Id : Client_Key; Group_Id : String);
      --  See above

      procedure Unsubscribe_Copy (Source : String; Target : String);

      procedure Info
        (Client_Count : out Natural;
         Group_Count  : out Natural;
         Process      : access procedure
                          (Client_Id   : Client_Key;
                           Address     : String;
                           State       : String;
                           Environment : Client_Environment;
                           Kind        : Mode;
                           Groups      : Group_Set));

   private
      Container : Tables.Map;
      Groups    : Group_Map;
      Open      : Boolean := True;
   end Object;

end AWS.Server.Push;
