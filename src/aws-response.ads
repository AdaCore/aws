------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                          Copyright (C) 2000-2001                         --
--                      Dmitriy Anisimkov & Pascal Obry                     --
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

--  This package is to be used to build answer to be sent to the client
--  browser.

with Ada.Strings.Unbounded;
with Ada.Streams;

with AWS.Status;
with AWS.Messages;
with AWS.MIME;

package AWS.Response is

   use Ada;

   type Data is private;

   type Data_Mode is (Header, Message, File);

   Default_Moved_Message : constant String :=
     "Page moved<br><a href=""_@_"">Click here</a>";
   --  This is a template message, _@_ will be replaced by the Location (see
   --  function Build with Location below).

   --
   --  Constructors
   --

   function Build
     (Content_Type : in String;
      Message_Body : in String;
      Status_Code  : in Messages.Status_Code := Messages.S200)
     return Data;

   function Build
     (Content_Type : in String;
      Message_Body : in Strings.Unbounded.Unbounded_String;
      Status_Code  : in Messages.Status_Code := Messages.S200)
     return Data;
   --  Return a message whose body is passed into Message_Body. The
   --  Content_Type parameter is the MIME type for the message
   --  body. Status_Code is the response status (see Messages.Status_Code
   --  definition).

   function Build
     (Content_Type : in String;
      Message_Body : in Streams.Stream_Element_Array;
      Status_Code  : in Messages.Status_Code := Messages.S200)
     return Data;
   --  Idem above, but the message body is a stream element array.

   function URL (Location : in String)
     return Data;
   --  This ask the server for a redirection to the specified URL.

   function Moved
     (Location : in String;
      Message  : in String := Default_Moved_Message)
     return Data;
   --  This send back a moved message (Messages.S301) with the specified
   --  message body.

   function Acknowledge
     (Status_Code  : in Messages.Status_Code;
      Message_Body : in String := "";
      Content_Type : in String := MIME.Text_HTML)
     return Data;
   --  Returns a message to the Web browser. This routine must be used to
   --  send back an error message to the Web browser. For example if a
   --  requested resource cannot be served a message with status code S404
   --  must be sent.

   function Authenticate (Realm : in String) return Data;
   --  Returns an authentification message (Messages.S401), the Web browser
   --  will then ask for an authentification. Realm string will be displayed
   --  by the Web Browser in the authentification dialog box.

   function File (Content_Type : in String;
                  Filename     : in String) return Data;
   --  Returns a message whose message body is the content of the file. The
   --  Content_Type must indicate the MIME type for the file.

   --
   --  Other API
   --

   function Mode           (D : in Data) return Data_Mode;
   --  Returns the data mode, either Header, Message or File.

   function Status_Code    (D : in Data) return Messages.Status_Code;
   --  Returns the status code.

   function Content_Length (D : in Data) return Natural;
   --  Returns the content length (i.e. the message body length). A value of 0
   --  indicate that there is no message body.

   function Content_Type   (D : in Data) return String;
   --  Returns the MIME type for the message body.

   function Location       (D : in Data) return String;
   --  Returns the location for the new page in the case of a moved
   --  message. See Moved constructor above.

   function Message_Body   (D : in Data) return String;
   --  Returns the message body content as a string.

   function Message_Body   (D : in Data)
       return Strings.Unbounded.Unbounded_String;
   --  Returns the message body content as a unbounded_string.

   function Realm          (D : in Data) return String;
   --  Returns the Realm for the current authentification request.

   function Binary         (D : in Data) return Streams.Stream_Element_Array;
   --  Returns the binary message body content.

   type Callback is access function (Request : in Status.Data) return Data;
   --  This is the Web Server Callback procedure. A client must declare and
   --  pass such procedure to the HTTP record.

private

   use Ada.Strings.Unbounded;

   type Stream_Element_Array_Access is access Streams.Stream_Element_Array;

   type Data is record
      Mode           : Data_Mode;
      Status_Code    : Messages.Status_Code;
      Content_Length : Natural;
      Content_Type   : Unbounded_String;
      Message_Body   : Unbounded_String;
      Location       : Unbounded_String;
      Realm          : Unbounded_String;
      Elements       : Stream_Element_Array_Access;
   end record;

end AWS.Response;
