------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2012-2019, AdaCore                     --
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

--  This package is used to build and register the active WebSockets. Some
--  services to send or broadcast messages are also provided.

with AWS.Status;

private with GNAT.Regexp;

package AWS.Net.WebSocket.Registry is

   type Factory is not null access function
     (Socket  : Socket_Access;
      Request : AWS.Status.Data) return Object'Class;

   --  Creating and Registering WebSockets

   function Constructor (URI : String) return Registry.Factory
     with Pre => URI'Length > 0;
   --  Get the WebObject's constructor for a specific URI

   procedure Register (URI : String; Factory : Registry.Factory)
     with Pre => URI'Length > 0;
   --  Register a WebObject's constructor for a specific URI

   procedure Register_Pattern
     (Pattern : String;
      Factory : Registry.Factory)
     with Pre => Pattern'Length > 0;
   --  Register a WebObject's constructor for a specific URI and pattern

   --  Sending messages

   type Recipient is private;

   No_Recipient : constant Recipient;

   function Create (URI : String; Origin : String := "") return Recipient
     with Pre  => URI'Length >  0,
          Post => Create'Result /= No_Recipient;
   --  A recipient with only an URI is called a broadcast as it designate all
   --  registered WebSocket for this specific URI. If Origin is specified then
   --  it designates a single client.
   --
   --  Note that both URI and Origin can be regular expressions.

   function Create (Id : UID) return Recipient
     with Pre  => Id /= No_UID,
          Post => Create'Result /= No_Recipient;
   --  A recipient for a specific WebSocket

   type Action_Kind is (None, Close);

   procedure Send
     (To           : Recipient;
      Message      : String;
      Except_Peer  : String := "";
      Timeout      : Duration := Forever;
      Asynchronous : Boolean := False;
      Error        : access procedure (Socket : Object'Class;
                                       Action : out Action_Kind) := null)
     with Pre => To /= No_Recipient
                 and then (if Asynchronous then Error = null);
   --  Send a message to the WebSocket designated by Origin and URI. Do not
   --  send this message to the peer whose address is given by Except_Peer.
   --  Except_Peer must be the address as reported by AWS.Net.Peer_Addr. It is
   --  often needed to send a message to all registered sockets except the one
   --  which has sent the message triggering a response.

   procedure Send
     (To           : Recipient;
      Message      : Unbounded_String;
      Except_Peer  : String := "";
      Timeout      : Duration := Forever;
      Asynchronous : Boolean := False;
      Error        : access procedure (Socket : Object'Class;
                                       Action : out Action_Kind) := null)
     with Pre => To /= No_Recipient
                 and then (if Asynchronous then Error = null);
   --  As above but with an Unbounded_String

   procedure Send
     (To           : Recipient;
      Message      : String;
      Request      : AWS.Status.Data;
      Timeout      : Duration := Forever;
      Asynchronous : Boolean := False;
      Error        : access procedure (Socket : Object'Class;
                                       Action : out Action_Kind) := null)
     with Pre => To /= No_Recipient
                 and then (if Asynchronous then Error = null);
   --  As above but filter out the client having set the given request

   procedure Send
     (To           : Recipient;
      Message      : Unbounded_String;
      Request      : AWS.Status.Data;
      Timeout      : Duration := Forever;
      Asynchronous : Boolean := False;
      Error        : access procedure (Socket : Object'Class;
                                       Action : out Action_Kind) := null)
     with Pre => To /= No_Recipient
                 and then (if Asynchronous then Error = null);
   --  As above but with an Unbounded_String

   procedure Close
     (To          : Recipient;
      Message     : String;
      Except_Peer : String := "";
      Timeout     : Duration := Forever;
      Error       : Error_Type := Normal_Closure)
     with Pre => To /= No_Recipient;
   --  Close connections

   --  Targeting a single WebSocket, these routines are equivalent to the
   --  Net.WebSocket ones but are thread-safe. That is, they can be mixed
   --  with other WebSocket activity to and from the clients.

   procedure Send
     (Socket       : in out Object'Class;
      Message      : String;
      Is_Binary    : Boolean := False;
      Timeout      : Duration := Forever;
      Asynchronous : Boolean := False);
   --  This default implementation just send a message to the client. The
   --  message is sent in a single chunk (not fragmented).

   procedure Send
     (Socket       : in out Object'Class;
      Message      : Unbounded_String;
      Is_Binary    : Boolean := False;
      Timeout      : Duration := Forever;
      Asynchronous : Boolean := False);
   --  Same as above but can be used for large messages. The message is
   --  possibly sent fragmented.

   procedure Send
     (Socket       : in out Object'Class;
      Message      : Stream_Element_Array;
      Is_Binary    : Boolean := True;
      Timeout      : Duration := Forever;
      Asynchronous : Boolean := False);
   --  As above but for a Stream_Element_Array

   procedure Close
     (Socket  : in out Object'Class;
      Message : String;
      Timeout : Duration := Forever;
      Error   : Error_Type := Normal_Closure);

   function Is_Registered (Id : UID) return Boolean;
   --  Returns True if the WebSocket Id is registered and False otherwise

private

   use GNAT.Regexp;

   type Recipient_Kind is (K_UID, K_URI);

   type Recipient (Kind : Recipient_Kind := K_URI) is record
      case Kind is
         when K_UID =>
            WS_Id : UID;

         when K_URI =>
            URI_Set    : Boolean := False;
            URI        : Regexp  := Compile ("");
            Origin_Set : Boolean := False;
            Origin     : Regexp  := Compile ("");
      end case;
   end record;

   No_Recipient : constant Recipient := (K_UID, No_UID);

   procedure Start;
   --  Start the WebServer's servers

   procedure Shutdown;
   --  Stop the WebServer's servers

   function Register (WebSocket : Object'Class) return Object_Class;
   --  Register a new WebSocket, returns a reference to the registered
   --  WebSocket or null if it was impossible to register it.

   procedure Watch (WebSocket : in out Object_Class)
     with Pre => WebSocket /= null;
   --  Watched WebSocket for incoming data

end AWS.Net.WebSocket.Registry;
