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

--  This package is used to keep the HTTP protocol status. Client can then
--  request the status for various values like the requested URI, the
--  Content_Length and the Session ID for example.

with Ada.Strings.Unbounded;
with Ada.Streams;

with AWS.Net;
with AWS.Session;
with AWS.Parameters;
with AWS.URL;

package AWS.Status is

   type Data is private;

   type Request_Method is (GET, HEAD, POST, PUT);

   type Authorization_Type is (None, Basic, Digest);

   function Check_Digest (D : in Data; Password : in String) return Boolean;
   --  This function is used by the digest authentication to check if the
   --  client password is correct.
   --  The password is not transferred between the client and the server,
   --  the server check that the client knows the right password using the
   --  MD5 checksum.

   function Authorization_Mode     (D : in Data) return Authorization_Type;
   pragma Inline (Authorization_Mode);
   --  Get the type of the "Authorization:" parameter

   function Authorization_Name     (D : in Data) return String;
   pragma Inline (Authorization_Name);
   --  Get the value for the name in the "Authorization:" parameter

   function Authorization_Password (D : in Data) return String;
   pragma Inline (Authorization_Password);
   --  Get the value for the password in the "Authorization:" parameter

   function Authorization_Realm    (D : in Data) return String;
   pragma Inline (Authorization_Realm);
   --  Get the value for the "realm" in the "Authorization:" parameter

   function Authorization_Nonce    (D : in Data) return String;
   pragma Inline (Authorization_Nonce);
   --  Get the value for the "nonce" in the "Authorization:" parameter

   function Authorization_NC       (D : in Data) return String;
   pragma Inline (Authorization_NC);
   --  Get the value for the "nc" in the "Authorization:" parameter

   function Authorization_CNonce   (D : in Data) return String;
   pragma Inline (Authorization_CNonce);
   --  Get the value for the "cnonce" in the "Authorization:" parameter

   function Authorization_QOP      (D : in Data) return String;
   pragma Inline (Authorization_QOP);
   --  Get the value for the "qop" in the "Authorization:" parameter

   function Authorization_Response (D : in Data) return String;
   pragma Inline (Authorization_Response);
   --  Get the value for the "responce" in the "Authorization:" parameter

   function Connection             (D : in Data) return String;
   pragma Inline (Connection);
   --  Get the value for "Connection:" parameter

   function Content_Length         (D : in Data) return Natural;
   pragma Inline (Content_Length);
   --  Get the value for "Content-Length:" parameter, this is the number of
   --  bytes in the message body.

   function Content_Type           (D : in Data) return String;
   pragma Inline (Content_Type);
   --  Get value for "Content-Type:" parameter

   function Has_Session            (D : in Data) return Boolean;
   pragma Inline (Has_Session);
   --  Returns true if a session ID has been received.

   function Host                   (D : in Data) return String;
   pragma Inline (Host);
   --  Get value for "Host:" parameter

   function HTTP_Version           (D : in Data) return String;
   pragma Inline (HTTP_Version);
   --  Returns the HTTP version used by the client.

   function If_Modified_Since      (D : in Data) return String;
   pragma Inline (If_Modified_Since);
   --  Get value for "If-Modified-Since:" parameter

   function Keep_Alive             (D : in Data) return Boolean;
   pragma Inline (Keep_Alive);
   --  Returns the flag if the current HTTP connection is keep-alive.

   function Method                 (D : in Data) return Request_Method;
   pragma Inline (Method);
   --  Returns the request method.

   function Multipart_Boundary     (D : in Data) return String;
   pragma Inline (Multipart_Boundary);
   --  Get value for the boundary part in "Content-Type: ...; boundary=..."
   --  parameter. This is a string that will be used to separate each chunk of
   --  data in a multipart message.

   function Parameters             (D : in Data) return Parameters.List;
   pragma Inline (Parameters);
   --  Returns the list of parameters for the request. This list can be empty
   --  if there was no form or URL parameters.

   function Peername               (D : in Data) return String;
   pragma Inline (Peername);
   --  Returns the name of the peer (the name of the client computer)

   function Session                (D : in Data) return AWS.Session.ID;
   pragma Inline (Session);
   --  Returns the Session ID for the request.

   function Session_Created        (D : in Data) return Boolean;
   --  Returns True if session was just created and is going to be sent to
   --  client.

   function Socket                 (D : in Data) return Net.Socket_Type'Class;
   pragma Inline (Socket);
   --  Returns the socket used to transfert data between the client and
   --  server.

   function URI                    (D : in Data) return String;
   pragma Inline (URI);
   --  Returns the requested resource.

   function URI                    (D : in Data) return URL.Object;
   pragma Inline (URI);
   --  As above but return an URL object.

   function User_Agent             (D : in Data) return String;
   pragma Inline (User_Agent);
   --  Get value for "User-Agent:" parameter

   function Referer                (D : in Data) return String;
   pragma Inline (Referer);
   --  Get value for "Referer:" parameter

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
   --  Returns the XML Payload message. XML payload is the actual SOAP request

   subtype Stream_Element_Array is Ada.Streams.Stream_Element_Array;

   function Binary_Data (D : in Data) return Stream_Element_Array;
   pragma Inline (Binary_Data);
   --  Returns the binary data message content.

private

   use Ada.Strings.Unbounded;

   type Stream_Element_Array_Access is access Stream_Element_Array;

   type Data is record
      Connection        : Unbounded_String;
      Host              : Unbounded_String;
      Peername          : Unbounded_String;
      Method            : Request_Method     := GET;
      URI               : AWS.URL.Object;
      Parameters        : AWS.Parameters.List;
      Binary_Data       : Stream_Element_Array_Access := null;
      HTTP_Version      : Unbounded_String;
      Content_Type      : Unbounded_String;
      Boundary          : Unbounded_String;
      Content_Length    : Natural            := 0;
      Keep_Alive        : Boolean;
      If_Modified_Since : Unbounded_String;
      File_Up_To_Date   : Boolean            := False;
      Socket            : Net.Socket_Access;
      Auth_Mode         : Authorization_Type := None;
      Auth_Name         : Unbounded_String; -- for Basic and Digest
      Auth_Password     : Unbounded_String; -- for Basic
      Auth_Realm        : Unbounded_String; -- for Digest
      Auth_Nonce        : Unbounded_String; -- for Digest
      Auth_NC           : Unbounded_String; -- for Digest
      Auth_CNonce       : Unbounded_String; -- for Digest
      Auth_QOP          : Unbounded_String; -- for Digest
      Auth_Response     : Unbounded_String; -- for Digest
      Session_ID        : AWS.Session.ID     := AWS.Session.No_Session;
      Session_Created   : Boolean            := False;
      SOAPAction        : Unbounded_String;
      Payload           : Unbounded_String;
      User_Agent        : Unbounded_String;
      Referer           : Unbounded_String;
   end record;

end AWS.Status;
