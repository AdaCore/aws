------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2000-2003                          --
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

--  This package is to be used to build answer to be sent to the client
--  browser. It is also used as the object returned from the client API. So
--  it is either a response built on the server side or the response received
--  on the client side.

with Ada.Strings.Unbounded;
with Ada.Streams;
with Ada.Finalization;

with AWS.Headers;
with AWS.Status;
with AWS.Messages;
with AWS.MIME;
with AWS.Net;
with AWS.Resources.Streams;

package AWS.Response is

   use Ada;

   type Data is private;
   --  Note that this type use a reference counter which is not thread safe.

   type Callback is access function (Request : in Status.Data) return Data;
   --  This is the Web Server Callback procedure. A client must declare and
   --  pass such procedure to the HTTP record.

   type Data_Mode is (Header, Message, File, Stream, Socket_Taken, No_Data);

   type Authentication_Mode is (Any, Basic, Digest);
   --  The authentication mode.
   --  "Basic" and "Digest" mean that server must accept the requested
   --  authentication mode. "Any" mean that server could accept any
   --  authentication from client.
   --  Note the order here should not be changed as it is used in AWS.Client.

   subtype Content_Length_Type is Integer range -1 .. Integer'Last;

   Undefined_Length : constant Content_Length_Type;
   --  Undefined length could be used when we do not know the message length
   --  at the start of transfer. The end of message could be determined by the
   --  chunked transfer-encoding in the HTTP/1.1, or by the closing connection
   --  in the HTTP/1.0.

   Default_Moved_Message : constant String;
   --  This is a template message, _@_ will be replaced by the Location (see
   --  function Build with Location below).

   Default_Authenticate_Message : constant String;
   --  This is the message that will be displayed on the Web Browser if the
   --  authentication process fails or is cancelled.

   -----------------------
   -- Data Constructors --
   -----------------------

   function Build
     (Content_Type  : in String;
      Message_Body  : in String;
      Status_Code   : in Messages.Status_Code  := Messages.S200;
      Cache_Control : in Messages.Cache_Option := Messages.Unspecified)
      return Data;

   function Build
     (Content_Type    : in String;
      UString_Message : in Strings.Unbounded.Unbounded_String;
      Status_Code     : in Messages.Status_Code  := Messages.S200;
      Cache_Control   : in Messages.Cache_Option := Messages.Unspecified)
      return Data;
   --  Return a message whose body is passed into Message_Body. The
   --  Content_Type parameter is the MIME type for the message
   --  body. Status_Code is the response status (see Messages.Status_Code
   --  definition).

   function Build
     (Content_Type  : in String;
      Message_Body  : in Streams.Stream_Element_Array;
      Status_Code   : in Messages.Status_Code         := Messages.S200;
      Cache_Control : in Messages.Cache_Option        := Messages.Unspecified)
      return Data;
   --  Idem above, but the message body is a stream element array.

   function File
     (Content_Type  : in String;
      Filename      : in String;
      Status_Code   : in Messages.Status_Code  := Messages.S200;
      Cache_Control : in Messages.Cache_Option := Messages.Unspecified)
      return Data;
   --  Returns a message whose message body is the content of the file. The
   --  Content_Type must indicate the MIME type for the file.

   function Stream
     (Content_Type  : in     String;
      Handle        : access Resources.Streams.Stream_Type'Class;
      Status_Code   : in     Messages.Status_Code   := Messages.S200;
      Cache_Control : in     Messages.Cache_Option  := Messages.No_Cache)
      return Data;
   --  Returns a message whose message body is the content of the user
   --  defined stream. The Content_Type must indicate the MIME type for
   --  the data stream, Stream_Size the total number of bytes and Status_Code
   --  the header status code which should be send back to client's browser.

   procedure Create_Resource
     (D    : in     Data;
      File :    out AWS.Resources.File_Type);
   pragma Inline (Create_Resource);
   --  Creates the resource object (either a file or in-memory object) for
   --  the data to be sent to the client. The resource should be closed after
   --  use.

   ------------------------------
   -- Redirection Constructors --
   ------------------------------

   function URL (Location : in String) return Data;
   --  This ask the server for a redirection to the specified URL.

   function Moved
     (Location : in String;
      Message  : in String := Default_Moved_Message)
      return Data;
   --  This send back a moved message (Messages.S301) with the specified
   --  message body.

   ------------------------
   -- Other Constructors --
   ------------------------

   function Acknowledge
     (Status_Code  : in Messages.Status_Code;
      Message_Body : in String := "";
      Content_Type : in String := MIME.Text_HTML)
      return Data;
   --  Returns a message to the Web browser. This routine must be used to
   --  send back an error message to the Web browser. For example if a
   --  requested resource cannot be served a message with status code S404
   --  must be sent.

   function Authenticate
     (Realm   : in String;
      Mode    : in Authentication_Mode := Basic;
      Stale   : in Boolean             := False;
      Message : in String              := Default_Authenticate_Message)
      return Data;
   --  Returns an authentification message (Messages.S401), the Web browser
   --  will then ask for an authentification. Realm string will be displayed
   --  by the Web Browser in the authentification dialog box.

   function Socket_Taken return Data;
   --  Must be used to say that the connection socket has been taken by user
   --  inside of user callback. No operations should be performed on this
   --  socket, and associated slot should be released for further operations.

   function Empty return Data;
   --  Returns an empty message (Data_Mode = No_Data and Status_Code is 204).
   --  It is used to say that user's handlers were not able to do something
   --  with the request. This is used by the callback's chain in the
   --  dispatcher and should not be used by users.

   --
   --  API to retrieve response data
   --

   ------------
   -- Header --
   ------------

   function Header
     (D    : in Data;
      Name : in String;
      N    : in Positive)
      return String;
   pragma Inline (Header);
   --  Return the N-th value for header Name.

   function Header
     (D    : in Data;
      Name : in String)
      return String;
   pragma Inline (Header);
   --  Return all values as a comma-separated string for header Name.
   --  See [RFC 2616 - 4.2] last paragraph.

   procedure Send_Header (Socket : in Net.Socket_Type'Class; D : in Data);
   pragma Inline (Send_Header);
   --  Send all header lines to the socket.

   function Status_Code (D : in Data) return Messages.Status_Code;
   pragma Inline (Status_Code);
   --  Returns the status code.

   function Content_Length (D : in Data) return Content_Length_Type;
   pragma Inline (Content_Length);
   --  Returns the content length (i.e. the message body length). A value of 0
   --  indicate that there is no message body.

   function Content_Type (D : in Data) return String;
   pragma Inline (Content_Type);
   --  Returns the MIME type for the message body.

   function Cache_Control (D : in Data) return Messages.Cache_Option;
   pragma Inline (Cache_Control);
   --  Returns the cache control specified for the response

   function Location (D : in Data) return String;
   pragma Inline (Location);
   --  Returns the location for the new page in the case of a moved
   --  message. See Moved constructor above.

   ----------
   -- Data --
   ----------

   function Mode (D : in Data) return Data_Mode;
   pragma Inline (Mode);
   --  Returns the data mode, either Header, Message or File.

   function Message_Body (D : in Data) return String;
   pragma Inline (Message_Body);
   --  Returns the message body content as a string.
   --  Message_Body routines could not be used with user defined streams
   --  (see. Stream routine in this package). Constraint_Error would be raised
   --  on try to get data by the Message_Body from the user defined streams.
   --  For get data from user defined streams routine Create_Resource should
   --  be used.

   function Message_Body
     (D      : in Data)
      return Strings.Unbounded.Unbounded_String;
   pragma Inline (Message_Body);
   --  Returns the message body content as a unbounded_string.

   function Message_Body (D : in Data) return Streams.Stream_Element_Array;
   pragma Inline (Message_Body);
   --  Returns message body as a binary content.

   function Filename (D : in Data) return String;
   pragma Inline (Filename);
   --  Returns the filename which should be sent back.

   --------------------
   -- Authentication --
   --------------------

   function Realm (D : in Data) return String;
   pragma Inline (Realm);
   --  Returns the Realm for the current authentification request.

   function Authentication (D : in Data) return Authentication_Mode;
   pragma Inline (Authentication);
   --  Returns the authentication mode requested by server.

   function Authentication_Stale (D : in Data) return Boolean;
   pragma Inline (Authentication_Stale);
   --  Returns the stale parameter for authentication.

private

   use Ada.Strings.Unbounded;

   Default_Moved_Message : constant String
     := "Page moved<br><a href=""_@_"">Click here</a>";

   CRLF : constant String := ASCII.CR & ASCII.LF;

   Default_Authenticate_Message : constant String
     := "<HTML><HEAD>" & CRLF
     & "<TITLE>401 Authorization Required</TITLE>" & CRLF
     & "</HEAD><BODY>" & CRLF
     & "<H1>Authorization Required</H1>" & CRLF
     & "This server could not verify that you" & CRLF
     & "are authorized to access the document you" & CRLF
     & "requested.  Either you supplied the wrong" & CRLF
     & "credentials (e.g., bad password), or your" & CRLF
     & "browser doesn't understand how to supply" & CRLF
     & "the credentials required.<P>" & CRLF
     & "</BODY></HTML>" & CRLF;

   Undefined_Length : constant Content_Length_Type
     := Content_Length_Type (Resources.Undefined_Length);

   type Release_Controller is record
      Counter        : Natural := 1;
      --  Counter of the reference to Data object.

      Stream_Taken : Boolean   := False;
      --  Flag would be set to true after Create_Resource routine call
      --  to do not free stream on finalize.
   end record;

   type Release_Controller_Access is access all Release_Controller;

   type Data is new Ada.Finalization.Controlled with record
      Ref_Counter  : Release_Controller_Access;
      Mode         : Data_Mode            := No_Data;
      Status_Code  : Messages.Status_Code := Messages.S200;
      Filename     : Unbounded_String;
      Stream       : Resources.Streams.Stream_Access;
      Header       : AWS.Headers.List;
   end record;

   procedure Initialize (Object : in out Data);
   procedure Adjust     (Object : in out Data);
   procedure Finalize   (Object : in out Data);

end AWS.Response;
