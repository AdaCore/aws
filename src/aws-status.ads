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

--  This package is used to keep the HTTP protocol status. Client can then
--  request the status for various values like the requested URI, the
--  Content_Length and the Session ID for example.

with Ada.Strings.Unbounded;
with Ada.Streams;

with AWS.Session;
with AWS.Parameters;
with Sockets;

package AWS.Status is

   type Data is private;

   type Request_Method is (GET, HEAD, POST, PUT);

   subtype Socket_Type is Sockets.Socket_FD'Class;

   type Socket_Access is access all Socket_Type;

   function Authorization_Name     (D : in Data) return String;
   --  Get the value for the name in the "Authorization:" parameter

   function Authorization_Password (D : in Data) return String;
   --  Get the value for the password in the "Authorization:" parameter

   function Connection             (D : in Data) return String;
   --  Get the value for "Connection:" parameter

   function Content_Length         (D : in Data) return Natural;
   --  Get the value for "Content-Length:" parameter, this is the number of
   --  bytes in the message body.

   function Content_Type           (D : in Data) return String;
   --  Get value for "Content-Type:" parameter

   function File_Up_To_Date        (D : in Data) return Boolean;
   --  Returns true if the file to be transfered is already up-to-date on
   --  the client side. In this case there is not need to send the file again,
   --  a message 304 will be sent back to the client.

   function Has_Session            (D : in Data) return Boolean;
   --  Returns true if a session ID has been received.

   function Host                   (D : in Data) return String;
   --  Get value for "Host:" parameter

   function HTTP_Version           (D : in Data) return String;
   --  Returns the HTTP version used by the client.

   function If_Modified_Since      (D : in Data) return String;
   --  Get value for "If-Modified-Since:" parameter

   function Method                 (D : in Data) return Request_Method;
   --  Returns the request method.

   function Multipart_Boundary     (D : in Data) return String;
   --  Get value for the boundary part in "Content-Type: ...; boundary=..."
   --  parameter. This is a string that will be used to separate each chunk of
   --  data in a multipart message.

   function Parameters             (D : in Data) return Parameters.List;
   --  Returns the list of parameters for the request. This list can be empty
   --  if there was no form or URL parameters.

   function Peername               (D : in Data) return String;
   --  Returns the name of the peer (the name of the client computer)

   function Session                (D : in Data) return AWS.Session.ID;
   --  Returns the Session ID for the request.

   function Socket                 (D : in Data) return Socket_Type;
   --  Returns the socket used to transfert data between the client and
   --  server.

   function URI                    (D : in Data) return String;
   --  Returns the requested resource

   function User_Agent             (D : in Data) return String;
   --  Get value for "User-Agent:" parameter

   function Referer                (D : in Data) return String;
   --  Get value for "Referer:" parameter

   function Is_SOAP                (D : in Data) return Boolean;
   --  Returns True if it is a SOAP request. In this case SOAPAction return
   --  the SOAPAction header and Payload returns the XML SOAP Payload message.

   function SOAPAction             (D : in Data) return String;
   --  Get value for "SOAPAction:" parameter. This is a standard header to
   --  support SOAP over HTTP protocol.

   function Payload                (D : in Data) return String;
   --  Returns the XML Payload message. XML payload is the actual SOAP request

   subtype Stream_Element_Array is Ada.Streams.Stream_Element_Array;

   function Binary_Data (D : in Data) return Stream_Element_Array;
   --  Returns the binary data message content.

private

   pragma Inline (Authorization_Name);
   pragma Inline (Authorization_Password);
   pragma Inline (Host);
   pragma Inline (Method);
   pragma Inline (URI);
   pragma Inline (HTTP_Version);
   pragma Inline (Socket);
   pragma Inline (Is_SOAP);

   use Ada.Strings.Unbounded;

   type Stream_Element_Array_Access is access Stream_Element_Array;

   type Data is record
      Connection        : Unbounded_String;
      Host              : Unbounded_String;
      Peername          : Unbounded_String;
      Method            : Request_Method     := GET;
      URI               : Unbounded_String;
      Parameters        : AWS.Parameters.List;
      Binary_Data       : Stream_Element_Array_Access := null;
      HTTP_Version      : Unbounded_String;
      Content_Type      : Unbounded_String;
      Boundary          : Unbounded_String;
      Content_Length    : Natural            := 0;
      If_Modified_Since : Unbounded_String;
      File_Up_To_Date   : Boolean            := False;
      Socket            : Socket_Access;
      Auth_Name         : Unbounded_String;
      Auth_Password     : Unbounded_String;
      Session_ID        : AWS.Session.ID     := AWS.Session.No_Session;
      SOAPAction        : Unbounded_String;
      Payload           : Unbounded_String;
      User_Agent        : Unbounded_String;
      Referer           : Unbounded_String;
   end record;

end AWS.Status;
