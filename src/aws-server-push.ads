------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2000-2001                          --
--                                ACT-Europe                                --
--                                                                          --
--  Authors: Dmitriy Anisimov - Pascal Obry                                 --
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

with Sockets;
with Ada.Streams;
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

   type Object is limited private;

   type Mode is (Plain, Multipart, Chunked);

   type Client_Holder is tagged private;

   subtype Socket_Type is Sockets.Socket_FD'Class;

   subtype Client_Key is String;

   procedure Register
     (Server      : in out Object;
      Client_ID   : in     Client_Key;
      Socket      : in     Socket_Type;
      Environment : in     Client_Environment;
      Kind        : in     Mode               := Plain);
   --  Add client identified by Client_ID to the server subscription
   --  list. After registering this client will be able to receive pushed data
   --  from the server in brodcasting mode.

   procedure Unregister
     (Server    : in out Object;
      Client_ID : in     Client_Key);
   --  Removes client from server subscription list.

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
   --  Push data to the every client (broadcast) subscribed to the server.

private

   type Stream_Access is access all Ada.Streams.Root_Stream_Type'Class;
   type Socket_Access is access all Socket_Type;

   type Client_Holder is tagged record
      Stream      : Stream_Access;
      Socket      : Socket_Access;
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

      procedure Register
        (Client_ID : in Client_Key;
         Holder    : in Client_Holder);

      procedure Send_To
        (Client_ID    : in Client_Key;
         Data         : in Client_Output_Type;
         Content_Type : in String);

      procedure Send
        (Data         : in Client_Output_Type;
         Content_Type : in String);

      procedure Unregister (Client_ID : in Client_Key);

   private
      Container : Table.Table_Type;
   end Object;

end AWS.Server.Push;
