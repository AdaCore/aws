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

package AWS.Response.Set is

   procedure Mode
     (D     : in out Data;
      Value : in     Data_Mode);
   pragma Inline (Mode);
   --  Set the data mode.
   --  Header, Message, File, Stream, Socket_Taken or No_Data.

   procedure Status_Code
     (D     : in out Data;
      Value : in     Messages.Status_Code);
   pragma Inline (Status_Code);
   --  Set the status code.

   procedure Content_Type
     (D     : in out Data;
      Value : in     String);
   pragma Inline (Content_Type);
   --  Set the MIME type for the message body.

   procedure Filename
     (D     : in out Data;
      Value : in     String);
   pragma Inline (Filename);
   --  Set the filename which should be sent back.
   --  set the Mode field to File.

   procedure Location
     (D     : in out Data;
      Value : in     String);
   pragma Inline (Location);
   --  Set the location for the new page in the case of a moved
   --  message. Should be used with redirection 3xx status codes.

   procedure Authentication
     (D     : in out Data;
      Realm : in String;
      Mode  : in Authentication_Mode := Basic;
      Stale : in Boolean             := False);
   pragma Inline (Authentication);
   --  Set the authentication mode requested by server.
   --  Set the status code to the 401.

   procedure Stream
     (D              : in out Data;
      Handle         : in     Resources.Streams.Stream_Access;
      Content_Length : in     Content_Length_Type);
   pragma Inline (Stream);
   --  Set the user defined data stream.
   --  set the Mode field to Stream.

   procedure Message_Body
     (D     : in out Data;
      Value : in     Streams.Stream_Element_Array);
   pragma Inline (Message_Body);
   --  Set message body as a binary content.
   --  set the Mode field to Message.

   procedure Message_Body
     (D     : in out Data;
      Value : in     Strings.Unbounded.Unbounded_String);
   pragma Inline (Message_Body);
   --  Set the message body content as a unbounded_string.
   --  set the Mode field to Message.

   procedure Message_Body
     (D     : in out Data;
      Value : in     String);
   pragma Inline (Message_Body);
   --  Set the message body content as a string.
   --  set the Mode field to Message.

   function Is_Valid (D : in Data) return Boolean;
   --  Checking validity of the HTTP response.

end AWS.Response.Set;