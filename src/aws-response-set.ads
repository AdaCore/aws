------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                          Copyright (C) 2002-2004                         --
--                                ACT-Europe                                --
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

with AWS.Net;

package AWS.Response.Set is

   type Encoding_Direction is (Encode, Decode);
   --  Server side would do gzip or deflate encoding,
   --  Client side would do gzip or deflate decoding.

   ------------
   -- Header --
   ------------

   procedure Add_Header
     (D     : in out Data;
      Name  : in     String;
      Value : in     String);
   pragma Inline (Add_Header);
   --  Add header name/value to the header container.
   --  Should be used inside of server's callback when the user want
   --  to add its own header lines to the response.

   procedure Update_Header
     (D     : in out Data;
      Name  : in     String;
      Value : in     String;
      N     : in     Positive := 1);
   pragma Inline (Update_Header);
   --  Update N-th header name/value in the header container.
   --  Should be used inside of server's callback when the user want
   --  to add/modify its own header lines to the response.

   procedure Read_Header (Socket : in Net.Socket_Type'Class; D : in out Data);
   --  Read all header data from the socket and fill appropriate
   --  data's fields.

   procedure Status_Code
     (D     : in out Data;
      Value : in     Messages.Status_Code);
   pragma Inline (Status_Code);
   --  Set the status code

   procedure Content_Type
     (D     : in out Data;
      Value : in     String);
   pragma Inline (Content_Type);
   --  Set the MIME type for the message body

   procedure Cache_Control
     (D     : in out Data;
      Value : in     Messages.Cache_Option);
   pragma Inline (Cache_Control);
   --  Set the Cache_Control mode for the message

   procedure Location
     (D     : in out Data;
      Value : in     String);
   pragma Inline (Location);
   --  Set the location for the new page in the case of a moved
   --  message. Should be used with redirection 3xx status codes.

   procedure Authentication
     (D     : in out Data;
      Realm : in     String;
      Mode  : in     Authentication_Mode := Basic;
      Stale : in     Boolean             := False);
   pragma Inline (Authentication);
   --  Set the authentication mode requested by server. Set the status code to
   --  the 401.

   ----------
   -- Data --
   ----------

   procedure Clear (D : in out Data);
   --  Clear all internal data.

   procedure Mode
     (D     : in out Data;
      Value : in     Data_Mode);
   pragma Inline (Mode);
   --  Set the data mode:
   --  Header, Message, File, Stream, Socket_Taken or No_Data.

   procedure Filename
     (D     : in out Data;
      Value : in     String);
   pragma Inline (Filename);
   --  Set the filename which should be sent back.
   --  It also set the Mode field to File.

   procedure Stream
     (D        : in out Data;
      Handle   : access Resources.Streams.Stream_Type'Class;
      Encoding : in     Messages.Content_Encoding := Messages.Identity);
   pragma Inline (Stream);
   --  Set the user defined data stream

   procedure Close_Resource
     (D     : in out Data;
      State : in     Boolean);
   --  Set the server close state, if State if False the resource will not be
   --  closed. This is needed to build transient resources as the closing must
   --  be controlled by the transient task cleaner and not the server.

   procedure Data_Encoding
     (D         : in out Data;
      Encoding  : in     Messages.Content_Encoding;
      Direction : in     Encoding_Direction := Encode);
   --  Set data encoding, the encoding will be used for the Message_Body and
   --  Append_Body routines below.
   --  Direction Encode is for server side, Direction Decode is for client
   --  side. This routine have to be called before calling Message_Body or
   --  Append_Body routines to activate the encoding. Note that by default no
   --  encoding is done if Data_Encoding is not called (Encoding => Identity).

   procedure Message_Body
     (D     : in out Data;
      Value : in     Streams.Stream_Element_Array);
   pragma Inline (Message_Body);
   --  Set message body as a binary content. Set the Mode field to Message

   procedure Message_Body
     (D     : in out Data;
      Value : in     Strings.Unbounded.Unbounded_String);
   pragma Inline (Message_Body);
   --  Set the message body content as a unbounded_string. Set the Mode field
   --  to Message.

   procedure Message_Body
     (D     : in out Data;
      Value : in     String);
   pragma Inline (Message_Body);
   --  Set the message body content as a string. Set the Mode field to Message

   procedure Append_Body
     (D    : in out Data;
      Item : in     Streams.Stream_Element_Array);
   --  Add Item to the message

   procedure Append_Body (D : in out Data; Item : in String);
   --  Add Item to the message

   ---------------
   -- Other API --
   ---------------

   function Is_Valid (D : in Data) return Boolean;
   --  Checking validity of the HTTP response

end AWS.Response.Set;
