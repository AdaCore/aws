------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2000-2008, AdaCore                     --
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

--  This package is used to keep the HTTP protocol status. Client can then
--  request the status for various values like the requested URI, the
--  Content_Length and the Session ID for example.

with Ada.Calendar;
with Ada.Streams;
with Ada.Strings.Unbounded;

with AWS.Attachments;
with AWS.Containers.Memory_Streams;
with AWS.Headers;
with AWS.Messages;
with AWS.Net;
with AWS.Parameters;
with AWS.Session;
with AWS.URL;

package AWS.Status is

   type Data is private;

   type Request_Method is
     (OPTIONS, GET, HEAD, POST, PUT, DELETE, TRACE, CONNECT, EXTENSION_METHOD);
   --  EXTENSION_METHOD indicates that a method is an extension-method,
   --  ie none of the eight method tokens predefined in the RFC 2616.

   type Authorization_Type is (None, Basic, Digest);

   ------------------
   -- Request-Line --
   ------------------

   function Method                 (D : in Data) return Request_Method;
   pragma Inline (Method);
   --  Returns the request method

   function Method                 (D : in Data) return String;
   pragma Inline (Method);
   --  Returns the request method as a String. Useful to get the method String
   --  for an extension-method, ie a method that is not already predefined
   --  in the RFC 2616.

   function URI                    (D : in Data) return String;
   pragma Inline (URI);
   --  Returns the requested resource

   function URI                    (D : in Data) return URL.Object;
   pragma Inline (URI);
   --  As above but return an URL object

   function Parameters             (D : in Data) return Parameters.List;
   pragma Inline (Parameters);
   --  Returns the list of parameters for the request. This list can be empty
   --  if there was no form or URL parameters.

   function Parameter
     (D : in Data; Name : in String; N : in Positive := 1) return String;
   pragma Inline (Parameter);

   function HTTP_Version           (D : in Data) return String;
   pragma Inline (HTTP_Version);
   --  Returns the HTTP version used by the client

   function Request_Time           (D : in Data) return Ada.Calendar.Time;
   pragma Inline (Request_Time);
   --  Returns the time of the request

   ------------
   -- Header --
   ------------

   function Header                 (D : in Data) return Headers.List;
   pragma Inline (Header);
   --  Returns the list of header lines for the request

   function Accept_Encoding        (D : in Data) return String;
   pragma Inline (Accept_Encoding);
   --  Get the value for "Accept-Encoding:" header

   function Connection             (D : in Data) return String;
   pragma Inline (Connection);
   --  Get the value for "Connection:" parameter

   function Content_Length         (D : in Data) return Natural;
   pragma Inline (Content_Length);
   --  Get the value for "Content-Length:" header, this is the number of
   --  bytes in the message body.

   function Content_Type           (D : in Data) return String;
   pragma Inline (Content_Type);
   --  Get value for "Content-Type:" header

   function Host                   (D : in Data) return String;
   pragma Inline (Host);
   --  Get value for "Host:" header

   function If_Modified_Since      (D : in Data) return String;
   pragma Inline (If_Modified_Since);
   --  Get value for "If-Modified-Since:" header

   function Keep_Alive             (D : in Data) return Boolean;
   pragma Inline (Keep_Alive);
   --  Returns the flag if the current HTTP connection is keep-alive

   function User_Agent             (D : in Data) return String;
   pragma Inline (User_Agent);
   --  Get value for "User-Agent:" header

   function Referer                (D : in Data) return String;
   pragma Inline (Referer);
   --  Get value for "Referer:" header

   function Is_Supported
     (D        : in Data;
      Encoding : in Messages.Content_Encoding) return Boolean;
   --  Returns True if the content encoding scheme is sported by the client

   function Preferred_Coding (D : in Data) return Messages.Content_Encoding;
   --  Returns supported by AWS coding preferred by client from the
   --  Accept-Coding header.

   ----------------
   -- Connection --
   ----------------

   function Peername               (D : in Data) return String;
   pragma Inline (Peername);
   --  Returns the name of the peer (the name of the client computer)

   function Socket                 (D : in Data) return Net.Socket_Type'Class;
   pragma Inline (Socket);
   --  Returns the socket used to transfert data between the client and
   --  server.

   function Socket                 (D : in Data) return Net.Socket_Access;
   pragma Inline (Socket);
   --  Returns the socket used to transfert data between the client and
   --  server. Use Socket_Access to avoid memory allocation if we would need
   --  socket access further.

   ----------
   -- Data --
   ----------

   function Multipart_Boundary     (D : in Data) return String;
   pragma Inline (Multipart_Boundary);
   --  Get value for the boundary part in "Content-Type: ...; boundary=..."
   --  parameter. This is a string that will be used to separate each chunk of
   --  data in a multipart message.

   subtype Stream_Element_Array is Ada.Streams.Stream_Element_Array;
   subtype Stream_Element_Offset is Ada.Streams.Stream_Element_Offset;

   function Binary_Data (D : in Data) return Stream_Element_Array;
   pragma Inline (Binary_Data);
   --  Returns the binary data message content.
   --  Note that only the root part of a multipart/related message is returned.

   procedure Reset_Body_Index (D : in out Data);
   --  Reset message body read position to the start

   procedure Read_Body
     (D      : in     Data;
      Buffer :    out Stream_Element_Array;
      Last   :    out Stream_Element_Offset);
   pragma Inline (Read_Body);
   --  Read a chunk of data from message body and put them into Buffer.
   --  Last is the index of the last item returned in Buffer.

   function End_Of_Body (D : in Data) return Boolean;
   pragma Inline (End_Of_Body);
   --  Returns true if there is no more data to read from the message body

   -----------------
   -- Attachments --
   -----------------

   function Attachments            (D : in Data) return AWS.Attachments.List;
   pragma Inline (Attachments);
   --  Returns the list of Attachments for the request

   -------------
   -- Session --
   -------------

   function Has_Session            (D : in Data) return Boolean;
   pragma Inline (Has_Session);
   --  Returns true if a session ID has been received

   function Session                (D : in Data) return Session.Id;
   pragma Inline (Session);
   --  Returns the Session ID for the request. Raises Constraint_Error if
   --  server's session support not activated.

   function Session_Created        (D : in Data) return Boolean;
   --  Returns True if session was just created and is going to be sent to
   --  client.

   function Session_Timed_Out      (D : in Data) return Boolean;
   --  Returns True if a previous session was timeout (even if a new session
   --  has been created).

   ----------
   -- SOAP --
   ----------

   function Is_SOAP                (D : in Data) return Boolean;
   pragma Inline (Is_SOAP);
   --  Returns True if it is a SOAP request. In this case SOAPAction return
   --  the SOAPAction header and Payload returns the XML SOAP Payload message.

   function SOAPAction             (D : in Data) return String;
   pragma Inline (SOAPAction);
   --  Get value for "SOAPAction:" parameter. This is a standard header to
   --  support SOAP over HTTP protocol.

   function Payload                (D : in Data) return String;
   pragma Inline (Payload);
   --  Returns the XML Payload message. XML payload is the actual SOAP
   --  request. This is the root part of multipart/related SOAP message.

   -----------
   -- HTTPS --
   -----------

   function Check_Digest
     (D : in Data; Password : in String) return Messages.Status_Code;
   --  This function is used by the digest authentication to check if the
   --  client password and authentication parameters are correct.
   --  The password is not transferred between the client and the server,
   --  the server check that the client knows the right password using the
   --  MD5 checksum.
   --  Returns Messages.S200 in case of successful authentication,
   --  Messages.S400 in case of wrong authentication request
   --  (RFC 2617 3.2.2, 3.2.2.5),
   --  and Messages.S401 in case of authentication error.

   function Check_Digest (D : in Data; Password : in String) return Boolean;
   --  The same as above, but do not distinguish wrong requests and
   --  authentication errors.

   function Authorization_Mode     (D : in Data) return Authorization_Type;
   pragma Inline (Authorization_Mode);
   --  Returns the type of the "Authorization:" parameter

   function Authorization_Name     (D : in Data) return String;
   pragma Inline (Authorization_Name);
   --  Returns "username" value in the "Authorization:" parameter

   function Authorization_URI      (D : in Data) return String;
   pragma Inline (Authorization_URI);
   --  Returns "uri" value in the "Authorization:" parameter
   --  Note, it could differ from HTTP URI field, for example Mozilla browser
   --  places http parameters to the authorization uri field.

   function Authorization_Password (D : in Data) return String;
   pragma Inline (Authorization_Password);
   --  Returns "password" value in the "Authorization:" parameter

   function Authorization_Realm    (D : in Data) return String;
   pragma Inline (Authorization_Realm);
   --  Returns "realm" value in the "Authorization:" parameter

   function Authorization_Nonce    (D : in Data) return String;
   pragma Inline (Authorization_Nonce);
   --  Returns "nonce" value in the "Authorization:" parameter

   function Authorization_NC       (D : in Data) return String;
   pragma Inline (Authorization_NC);
   --  Returns "nc" value in the "Authorization:" parameter

   function Authorization_CNonce   (D : in Data) return String;
   pragma Inline (Authorization_CNonce);
   --  Returns "cnonce" value in the "Authorization:" parameter

   function Authorization_QOP      (D : in Data) return String;
   pragma Inline (Authorization_QOP);
   --  Retruns "qop" value in the "Authorization:" parameter

   function Authorization_Response (D : in Data) return String;
   pragma Inline (Authorization_Response);
   --  Returns "response" value in the "Authorization:" parameter

   function Authorization_Tail     (D : in Data) return String;
   pragma Inline (Authorization_Tail);
   --  Returns precalculated part of digest composed of
   --  Nonce, NC, CNonce, QOP, Method, URI authorization fields.
   --  To build a full authorization response you can use:
   --
   --  MD5.Digest
   --    (MD5.Digest (Username & ':' & Realm & ':' & Password)
   --      & Authorization_Tail);
   --
   --  This method can be used to avoid sending a password over the network.

private

   use Ada.Strings.Unbounded;

   type Memory_Stream_Access is access Containers.Memory_Streams.Stream_Type;

   type Data is record
      --  Connection info
      Socket            : Net.Socket_Access;
      Peername          : Unbounded_String;

      --  Request
      Header            : Headers.List;
      Method            : Request_Method        := GET;
      Method_String     : Unbounded_String;
      HTTP_Version      : Unbounded_String;
      URI               : aliased URL.Object;
      Request_Time      : Ada.Calendar.Time;
      Binary_Data       : Memory_Stream_Access;
      Content_Length    : Natural               := 0;
      Keep_Alive        : Boolean;
      File_Up_To_Date   : Boolean               := False;
      Attachments       : AWS.Attachments.List;

      --  SOAP
      SOAP_Action       : Boolean               := False;
      --  True if SOAPAction is set in the header list

      --  Authentication
      Auth_Mode         : Authorization_Type    := None;
      Auth_Name         : Unbounded_String; -- for Basic and Digest
      Auth_Password     : Unbounded_String; -- for Basic
      Auth_Realm        : Unbounded_String; -- for Digest
      Auth_Nonce        : Unbounded_String; -- for Digest
      Auth_NC           : Unbounded_String; -- for Digest
      Auth_CNonce       : Unbounded_String; -- for Digest
      Auth_QOP          : Unbounded_String; -- for Digest
      Auth_URI          : Unbounded_String; -- for Digest
      Auth_Response     : Unbounded_String; -- for Digest

      --  Session
      Session_Id        : AWS.Session.Id        := AWS.Session.No_Session;
      Session_Created   : Boolean               := False;
      Session_Timed_Out : Boolean               := False;
   end record;

end AWS.Status;
