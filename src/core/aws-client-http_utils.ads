------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2005-2009, AdaCore                     --
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

with AWS.Client;
with AWS.Response;

package AWS.Client.HTTP_Utils is

   use AWS.Client;
   use Ada.Strings.Unbounded;

   Connection_Error : exception renames Client.Connection_Error;
   Protocol_Error   : exception renames Client.Protocol_Error;

   Debug_On : Boolean := False;

   procedure Decrement_Authentication_Attempt
     (Connection : in out HTTP_Connection;
      Counter    : in out Auth_Attempts_Count;
      Over       : out Boolean);
   --  Counts the authentication attempts. Over is set to True when
   --  authentication attempts are over.

   procedure Set_Authentication
     (Auth : out Authentication_Type;
      User : String;
      Pwd  : String;
      Mode : Authentication_Mode);
   --  Internal procedure to set authentication parameters

   procedure Parse_Header
     (Connection : in out HTTP_Connection;
      Answer     : out Response.Data;
      Keep_Alive : out Boolean);
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
      Result     : out Response.Data;
      Get_Body   : Boolean         := True);
   --  Receives response from server for GET and POST and HEAD commands.
   --  If Get_Body is set then the message body will be read.

   procedure Read_Body
     (Connection : in out HTTP_Connection;
      Result     : out Response.Data;
      Store      : Boolean);
   --  Read message body and store it into Result if Store is True otherwise
   --  the content is discarded.

   procedure Open_Send_Common_Header
     (Connection : in out HTTP_Connection;
      Method     : String;
      URI        : String;
      Headers    : Header_List := Empty_Header_List);
   --  Open the the Connection if it is not open. Send the common HTTP headers
   --  for all requests like the proxy, authentication, user agent, host.

   procedure Send_Authentication_Header
     (Connection : in out HTTP_Connection;
      Token      : String;
      Data       : in out Authentication_Type;
      URI        : String;
      Method     : String);
   --  Send the authentication header for proxy or for server

   procedure Internal_Post
     (Connection   : in out HTTP_Connection;
      Result       : out Response.Data;
      Data         : Stream_Element_Array;
      URI          : String;
      SOAPAction   : String;
      Content_Type : String;
      Attachments  : Attachment_List;
      Headers      : Header_List           := Empty_Header_List);
   --  Common base routine for Post and SOAP_Post routines

   procedure Internal_Post_Without_Attachment
     (Connection   : in out HTTP_Connection;
      Result       : out Response.Data;
      Data         : Stream_Element_Array;
      URI          : String;
      SOAPAction   : String;
      Content_Type : String;
      Headers      : Header_List := Empty_Header_List);
   --  Only used by Internal_Post

   procedure Internal_Post_With_Attachment
     (Connection   : in out HTTP_Connection;
      Result       : out Response.Data;
      Data         : Stream_Element_Array;
      URI          : String;
      SOAPAction   : String;
      Content_Type : String;
      Attachments  : Attachment_List;
      Headers      : Header_List           := Empty_Header_List);
   --  Only used by Internal_Post

   procedure Send_Common_Post
     (Connection   : in out HTTP_Connection;
      Data         : Stream_Element_Array;
      URI          : String;
      SOAPAction   : String;
      Content_Type : String;
      Headers      : Header_List := Empty_Header_List);
   --  Send to the server only a POST request with Data
   --  and common headers, using a Connection.

   procedure Send_Header
     (Sock : AWS.Net.Socket_Type'Class;
      Data : String);
   pragma Inline (Send_Header);
   --  Send header Data to socket and call Debug_Message

   procedure Send_Header
     (Sock        : AWS.Net.Socket_Type'Class;
      Header      : String;
      Constructor : not null access function (Value : String) return String;
      Value       : String;
      Headers     : Header_List);
   pragma Inline (Send_Header);
   --  Send header to socket if this header is not present in Headers. The
   --  actual header data is given by the constructor. Call Debug_Message if
   --  header is sent.

   procedure Set_HTTP_Connection
     (HTTP_Client : in out HTTP_Connection;
      Sock_Ptr    : AWS.Net.Socket_Access);
   --  Initialize HTTP_Client by positioning the socket used as Sock_Ptr

   function Value (V : String) return Unbounded_String;
   --  Returns V as an Unbounded_String if V is not the empty string
   --  otherwise it returns Null_Unbounded_String.

end AWS.Client.HTTP_Utils;
