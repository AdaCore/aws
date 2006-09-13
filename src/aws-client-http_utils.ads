------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                            Copyright (C) 2005                            --
--                                 AdaCore                                  --
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

with Ada.Streams;

with AWS.Attachments;
with AWS.Client;
with AWS.Response;

package AWS.Client.HTTP_Utils is

   use AWS.Client;
   use Ada.Streams;
   use Ada.Strings.Unbounded;

   Debug_On : Boolean := False;

   procedure Decrement_Authentication_Attempt
     (Connection : in out HTTP_Connection;
      Counter    : in out Auth_Attempts_Count;
      Over       :    out Boolean);
   --  Counts the authentication attempts. Over is set to True when
   --  authentication attempts are over.

   procedure Set_Authentication
     (Auth :    out Authentication_Type;
      User : in     String;
      Pwd  : in     String;
      Mode : in     Authentication_Mode);
   --  Internal procedure to set authentication parameters

   procedure Parse_Header
     (Connection : in out HTTP_Connection;
      Answer     :    out Response.Data;
      Keep_Alive :    out Boolean);
   --  Read server answer and set corresponding variable with the value
   --  read. Most of the fields are ignored right now.

   procedure Connect (Connection : in out HTTP_Connection);
   --  Open the connection. Raises Connection_Error if it is not possible to
   --  establish the connection. In this case all resources are released and
   --  Connection.Opened is set to False.

   procedure Disconnect (Connection : in out HTTP_Connection);
   --  Close connection. Further use is not possible

   procedure Get_Response
     (Connection : in out HTTP_Connection;
      Result     :    out Response.Data;
      Get_Body   : in     Boolean         := True);
   --  Receives response from server for GET and POST and HEAD commands.
   --  If Get_Body is set then the message body will be read.

   procedure Open_Send_Common_Header
     (Connection : in out HTTP_Connection;
      Method     : in     String;
      URI        : in     String);
   --  Open the the Connection if it is not open. Send the common HTTP headers
   --  for all requests like the proxy, authentication, user agent, host.

   function Port_Not_Default (Port : in Positive) return String;
   --  Returns the port image (preceded by character ':') if it is not the
   --  default port.

   procedure Send_Authentication_Header
     (Connection : in out HTTP_Connection;
      Token      : in     String;
      Data       : in out Authentication_Type;
      URI        : in     String;
      Method     : in     String);
   --  Send the authentication header for proxy or for server

   procedure Internal_Post
     (Connection   : in out HTTP_Connection;
      Result       :    out Response.Data;
      Data         : in     Ada.Streams.Stream_Element_Array;
      URI          : in     String;
      SOAPAction   : in     String;
      Content_Type : in     String;
      Attachments  : in     AWS.Attachments.List);
   --  Common base routine for Post and SOAP_Post routines

   procedure Internal_Post_Without_Attachment
     (Connection   : in out HTTP_Connection;
      Result       :    out Response.Data;
      Data         : in     Ada.Streams.Stream_Element_Array;
      URI          : in     String;
      SOAPAction   : in     String;
      Content_Type : in     String);
   --  Only used by Internal_Post

   procedure Internal_Post_With_Attachment
     (Connection   : in out HTTP_Connection;
      Result       :    out Response.Data;
      Data         : in     Ada.Streams.Stream_Element_Array;
      URI          : in     String;
      SOAPAction   : in     String;
      Content_Type : in     String;
      Attachments  : in     AWS.Attachments.List);
   --  Only used by Internal_Post

   procedure Send_Common_Post
     (Connection   : in out HTTP_Connection;
      Data         : in     Ada.Streams.Stream_Element_Array;
      URI          : in     String;
      SOAPAction   : in     String;
      Content_Type : in     String);
   --  Send to the server only a POST request with Data
   --  and common headers, using a Connection.

   procedure Send_Header
     (Sock : in AWS.Net.Socket_Type'Class;
      Data : in String);
   pragma Inline (Send_Header);
   --  Send header Data to socket and call Debug_Message.

   procedure Set_HTTP_Connection
     (HTTP_Client : in out HTTP_Connection;
      Sock_Ptr    : in     AWS.Net.Socket_Access);
   --  Initialize HTTP_Client by positioning the socket used as Sock_Ptr

   function Value (V : in String) return Unbounded_String;
   --  Returns V as an Unbounded_String if V is not the empty string
   --  otherwise it returns Null_Unbounded_String.

end AWS.Client.HTTP_Utils;
