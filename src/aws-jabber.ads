------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                            Copyright (C) 2002                            --
--                               ACT-Europe                                 --
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

with Ada.Strings.Unbounded;

with AWS.Containers.Key_Value;
with AWS.Net;

package AWS.Jabber is

   type Server is limited private;
   --  This is the Jabber server connection. This object is initialized by
   --  Connect below and is used with all services.

   Default_Port : constant := 5222;
   --  Standard Jabber Server default port is 5222. The SSL based connection
   --  port is 5223 but this is not supported by this API.

   Server_Error : exception;
   --  Raised by any routine below when an server or protocol error occurs. A
   --  message is attached to the exception, this correspond to the <error>
   --  XML protocol tag if present.

   procedure Connect
     (Server   : in out Jabber.Server;
      Host     : in     String;
      User     : in     String;
      Password : in     String;
      Port     : in     Positive      := Default_Port);
   --  Connect to a Jabber server Host:Port using User/Password account. It
   --  returns the Server object which can be used with services below.

   procedure Close (Server : in out Jabber.Server);
   --  Close the connection with the Jabber server.

   procedure Send_Message
     (Server  : in Jabber.Server;
      JID     : in String;
      Subject : in String;
      Content : in String);
   --  Send a message to user JID (Jabber ID) via the specified Server. The
   --  message is composed of Subject and a body (Content).

   type Presence_Status
      is (Offline, Available, Chat, Away, Extended_Away, Do_Not_Disturb);

   procedure Check_Presence
     (Server : in     Jabber.Server;
      JID    : in     String;
      Status :    out Presence_Status);
   --  Returns the presence status for JID.

private
   --  Jabber Client and Server open a stream and both communicate with each
   --  others via this channel. All messages exchanged are XML encoded. Both
   --  streams (client -> server and server -> client) have a common
   --  structure:
   --
   --  <?xml version=""1.0"" encoding=""UTF-8"" ?>"
   --  <stream:stream>
   --     <message> ... </message>
   --     <presence> ... </presence>
   --  </stream:stream>
   --
   --  As both streams are sent asynchronously, we use a Task to read the
   --  incoming stream. This task is responsible to accept incoming XML
   --  message, to parse them, create a Message object and add it into the
   --  Mailbox. The client services provided by this package just send the
   --  right XML stream and retrieve server's answer from the Mailbox.

   use Ada.Strings.Unbounded;

   subtype Message is AWS.Containers.Key_Value.Set;
   --  A message, this is just a set of key/value pair. Each key represent a
   --  tag and the associated value is the tag's value. Tag's attributes are
   --  encoded with a key which is the tag element name catenated with a '.'
   --  and the attribute name. For example with :
   --
   --     <presence from="toto"/>
   --
   --  We have :     Key            Value
   --                -------------  ------
   --                presence       ""
   --                presence.from  "toto"

   type Message_Access is access all Message;

   type Message_Set is array (Natural range <>) of Message_Access;

   -------------
   -- Mailbox --
   -------------

   protected type Mailbox (Max_Size : Positive) is

      entry Add (M : in Message_Access);
      --  Add a new message into the Mailbox, only possible if there is some
      --  free room on the Mailbox.

      entry Get (M : out Message_Access);
      --  Get a message from the Mailbox, only possible if there is some
      --  message in the Mailbox.

      function Size return Natural;
      --  Returns the current number of message waiting in the Mailbox.

      procedure Destroy;
      --  Removes and free memory associated with a messages in the Mailbox.

   private
      Buffer       : Message_Set (1 .. Max_Size);
      Current_Size : Natural := 0;
      Current      : Natural := 0;
      Last         : Natural := 0;
   end Mailbox;

   ---------------------
   -- Incoming_Stream --
   ---------------------

   --  Read incoming XML messages, parse them and add them to the
   --  Mailbox. This task will terminate with the connection socket will be
   --  closed by the client.

   task type Incoming_Stream (Server : access Jabber.Server);

   type Incoming_Stream_Access is access Incoming_Stream;

   ------------
   -- Server --
   ------------

   type Server_Access is access all Server;

   type Server is record
      Self    : Server_Access := Server'Unchecked_Access;
      Host    : Unbounded_String;
      Port    : Positive;
      Sock    : Net.Socket_Access;
      User    : Unbounded_String;
      Started : Boolean := False;
      SID     : Unbounded_String;

      MB     : Mailbox (25); -- Mailbox with a maximum of 25 mesages
      Stream : Incoming_Stream_Access;
   end record;

end AWS.Jabber;
