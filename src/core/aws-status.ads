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

pragma Ada_2012;

--  This package is used to keep the HTTP protocol status. Client can then
--  request the status for various values like the requested URI, the
--  Content_Length and the Session ID for example.

with Ada.Calendar;
with Ada.Real_Time;
with Ada.Streams;
with Ada.Strings.Unbounded;

with AWS.Attachments;
with AWS.Headers;
with AWS.Messages;
with AWS.Net;
with AWS.Parameters;
with AWS.Session;
with AWS.URL;

private with AWS.Resources.Streams.Memory;
private with GNAT.SHA256;

package AWS.Status is

   use Ada;
   use Ada.Streams;
   use Ada.Strings.Unbounded;

   type Data is private;

   type Request_Method is
     (OPTIONS, GET, HEAD, POST, PUT, DELETE, TRACE, CONNECT, EXTENSION_METHOD);
   --  EXTENSION_METHOD indicates that a method is an extension-method,
   --  ie none of the eight method tokens predefined in the RFC 2616.

   type Authorization_Type is (None, Basic, Digest);

   ------------------
   -- Request-Line --
   ------------------

   function Method       (D : Data) return Request_Method with Inline;
   --  Returns the request method

   function Method       (D : Data) return String with Inline;
   --  Returns the request method as a String. Useful to get the method String
   --  for an extension-method, ie a method that is not already predefined
   --  in the RFC 2616.

   function URI          (D : Data) return String with Inline;
   --  Returns the requested resource

   function URI          (D : Data) return URL.Object with Inline;
   --  As above but return an URL object

   function URL          (D : Data) return String with Inline;
   --  Returns the requested URL

   function Parameters   (D : Data) return Parameters.List with Inline;
   --  Returns the list of parameters for the request. This list can be empty
   --  if there was no form or URL parameters.

   function Parameter
     (D : Data; Name : String; N : Positive := 1) return String with Inline;

   function HTTP_Version (D : Data) return String with Inline;
   --  Returns the HTTP version used by the client

   function Request_Time (D : Data) return Calendar.Time with Inline;
   --  Returns the time of the request

   function Request_Time (D : Data) return Real_Time.Time with Inline;

   ------------
   -- Header --
   ------------

   function Header          (D : Data) return Headers.List with Inline;
   --  Returns the list of header lines for the request

   function Accept_Encoding (D : Data) return String with Inline;
   --  Get the value for "Accept-Encoding:" header

   function Connection      (D : Data) return String with Inline;
   --  Get the value for "Connection:" header

   function Content_Length  (D : Data) return Stream_Element_Count with Inline;
   --  Get the value for "Content-Length:" header, this is the number of
   --  bytes in the message body.

   function Content_Type    (D : Data) return String with Inline;
   --  Get value for "Content-Type:" header

   function Transfer_Encoding (D : Data) return String with Inline;
   --  Get value for "Transfer-Encoding:" header

   function Expect            (D : Data) return String with Inline;
   --  Get value for "Expect:" header

   function Host              (D : Data) return String with Inline;
   --  Get value for "Host:" header

   function If_Modified_Since (D : Data) return String with Inline;
   --  Get value for "If-Modified-Since:" header

   function Keep_Alive        (D : Data) return Boolean with Inline;
   --  Returns the flag if the current HTTP connection is keep-alive

   function User_Agent        (D : Data) return String with Inline;
   --  Get value for "User-Agent:" header

   function Referer           (D : Data) return String with Inline;
   --  Get value for "Referer:" header

   function Cache_Control     (D : Data) return Messages.Cache_Option
     with Inline;
   --  Get value for "Cache-Control:" header

   function Cache_Control     (D : Data) return Messages.Cache_Data
     with Inline;
   --  Returns the cache control data specified for the request

   function Is_Supported
     (D        : Data;
      Encoding : Messages.Content_Encoding) return Boolean;
   --  Returns True if the content encoding scheme is supported by the client

   function Preferred_Coding  (D : Data) return Messages.Content_Encoding;
   --  Returns supported by AWS coding preferred by client from the
   --  Accept-Coding header.

   function Upgrade           (D : Data) return String with Inline;
   --  Get value for "Upgrade:" header

   function Sec_WebSocket_Key (D : Data) return String with Inline;
   --  Get value for "Sec-WebSocket-Key:" header

   -------------------------------------------
   -- Cross-Origin Resource Sharing Headers --
   -------------------------------------------

   function Origin (D : Data) return String with Inline;
   --  Get value for "Origin:" header

   function Access_Control_Request_Headers (D : Data) return String
     with Inline;
   --  Get value for "Access-Control-Request-Headers:" header

   function Access_Control_Request_Method (D : Data) return String with Inline;
   --  Get value for "Access-Control-Request-Method:" header

   ----------------
   -- Connection --
   ----------------

   function Peername (D : Data) return String with Inline;
   --  Returns the address of the peer (the IP address of the client computer)

   function Socket   (D : Data) return Net.Socket_Type'Class with Inline;
   --  Returns the socket used to transfer data between the client and
   --  server.

   function Socket   (D : Data) return Net.Socket_Access with Inline;
   --  Returns the socket used to transfer data between the client and
   --  server. Use Socket_Access to avoid memory allocation if we would need
   --  socket access further.

   ----------
   -- Data --
   ----------

   function Is_Body_Uploaded       (D : Data) return Boolean with Inline;
   --  Returns True if the message body has been uploaded and False if not.
   --  The reason being that the body size is above Upload_Size_Limit.
   --  User can upload the file using AWS.Server.Get_Message_Body, the size
   --  being returned by Content_Length.

   function Multipart_Boundary     (D : Data) return String with Inline;
   --  Get value for the boundary part in "Content-Type: ...; boundary=..."
   --  parameter. This is a string that will be used to separate each chunk of
   --  data in a multipart message.

   function Binary_Data (D : Data) return Stream_Element_Array with Inline;
   --  Returns the binary data message content.
   --  Note that only the root part of a multipart/related message is returned.

   function Binary_Data (D : Data) return Unbounded_String;
   --  Returns the binary data message content in a Unbounded_String
   --  Note that only the root part of a multipart/related message is returned.

   function Binary_Size (D : Data) return Stream_Element_Offset with Inline;
   --  Returns size of the binary data message content

   procedure Reset_Body_Index (D : Data) with Inline;
   --  Reset message body read position to the start

   procedure Read_Body
     (D      : Data;
      Buffer : out Stream_Element_Array;
      Last   : out Stream_Element_Offset)
   with Inline;
   --  Read a chunk of data from message body and put them into Buffer.
   --  Last is the index of the last item returned in Buffer.

   function End_Of_Body (D : Data) return Boolean with Inline;
   --  Returns true if there is no more data to read from the message body

   -----------------
   -- Attachments --
   -----------------

   function Attachments (D : Data) return AWS.Attachments.List with Inline;
   --  Returns the list of Attachments for the request

   -------------
   -- Session --
   -------------

   function Has_Session            (D : Data) return Boolean with Inline;
   --  Returns true if a session ID has been received

   function Session_Private        (D : Data) return String with Inline;
   --  Returns the private Session ID for the request. Raises Constraint_Error
   --  if server's session support not activated.

   function Session                (D : Data) return Session.Id with Inline;
   --  Returns the Session ID for the request. Raises Constraint_Error if
   --  server's session support not activated.

   function Session_Created        (D : Data) return Boolean;
   --  Returns True if session was just created and is going to be sent to
   --  client.

   function Session_Timed_Out      (D : Data) return Boolean;
   --  Returns True if a previous session was timeout (even if a new session
   --  has been created).

   ----------
   -- SOAP --
   ----------

   function Is_SOAP    (D : Data) return Boolean with Inline;
   --  Returns True if it is a SOAP request. In this case SOAPAction return
   --  the SOAPAction header and Payload returns the XML SOAP Payload message.

   function SOAPAction (D : Data) return String with Inline;
   --  Get value for "SOAPAction:" parameter. This is a standard header to
   --  support SOAP over HTTP protocol.

   function Payload    (D : Data) return String with Inline;
   --  Returns the XML Payload message. XML payload is the actual SOAP
   --  request. This is the root part of multipart/related SOAP message.

   function Payload    (D : Data) return Unbounded_String;
   --  Returns the XML Payload message. XML payload is the actual SOAP
   --  request. This is the root part of multipart/related SOAP message.

   -----------
   -- HTTPS --
   -----------

   function Check_Digest
     (D : Data; Password : String) return Messages.Status_Code;
   --  This function is used by the digest authentication to check if the
   --  client password and authentication parameters are correct.
   --  The password is not transferred between the client and the server,
   --  the server check that the client knows the right password using the
   --  MD5 checksum.
   --  Returns Messages.S200 in case of successful authentication,
   --  Messages.S400 in case of wrong authentication request
   --  (RFC 2617 3.2.2, 3.2.2.5),
   --  and Messages.S401 in case of authentication error.

   function Check_Digest (D : Data; Password : String) return Boolean;
   --  The same as above, but do not distinguish wrong requests and
   --  authentication errors.

   function Authorization_Mode     (D : Data) return Authorization_Type
     with Inline;
   --  Returns the type of the "Authorization:" parameter

   function Authorization_Name     (D : Data) return String with Inline;
   --  Returns "username" value in the "Authorization:" parameter

   function Authorization_URI      (D : Data) return String with Inline;
   --  Returns "uri" value in the "Authorization:" parameter
   --  Note, it could differ from HTTP URI field, for example Mozilla browser
   --  places http parameters to the authorization uri field.

   function Authorization_Password (D : Data) return String with Inline;
   --  Returns "password" value in the "Authorization:" parameter

   function Authorization_Realm    (D : Data) return String with Inline;
   --  Returns "realm" value in the "Authorization:" parameter

   function Authorization_Nonce    (D : Data) return String with Inline;
   --  Returns "nonce" value in the "Authorization:" parameter

   function Authorization_NC       (D : Data) return String with Inline;
   --  Returns "nc" value in the "Authorization:" parameter

   function Authorization_CNonce   (D : Data) return String with Inline;
   --  Returns "cnonce" value in the "Authorization:" parameter

   function Authorization_QOP      (D : Data) return String with Inline;
   --  Retruns "qop" value in the "Authorization:" parameter

   function Authorization_Response (D : Data) return String with Inline;
   --  Returns "response" value in the "Authorization:" parameter

   function Authorization_Tail     (D : Data) return String with Inline;
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

   use GNAT;

   type Memory_Stream_Access is
     access Resources.Streams.Memory.Stream_Type'Class;

   No_Session_Private : constant SHA256.Message_Digest :=
                          (others => ASCII.NUL);

   type Data is record
      --  Connection info
      Socket            : Net.Socket_Access;
      Peername          : Unbounded_String;

      --  Request
      Header            : Headers.List;
      Query             : Unbounded_String;
      Method            : Request_Method        := GET;
      Method_String     : Unbounded_String;
      HTTP_Version      : Unbounded_String;
      URI               : aliased AWS.URL.Object;
      Calendar_Time     : Calendar.Time;
      Monotonic_Time    : Real_Time.Time;
      Binary_Data       : Memory_Stream_Access;
      Uploaded          : Boolean               := False;
      Content_Length    : Stream_Element_Count  := 0;
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
      Session_Private   : SHA256.Message_Digest := No_Session_Private;
      Session_Created   : Boolean               := False;
      Session_Timed_Out : Boolean               := False;
   end record;

end AWS.Status;
