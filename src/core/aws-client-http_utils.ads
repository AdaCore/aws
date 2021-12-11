------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2005-2021, AdaCore                     --
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

with AWS.Config;
with AWS.HTTP2.Frame.Settings;
with AWS.Response;
with AWS.Status;

package AWS.Client.HTTP_Utils is

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

   procedure Read_Parse_Header
     (Connection : in out HTTP_Connection;
      Answer     : in out Response.Data;
      Keep_Alive : out Boolean);
   --  Read server answer (in HTTP/1.x only) and set corresponding variable
   --  with the value read. Most of the fields are ignored right now. For
   --  HTTP/2 mode Answer's headers are supposed to have been added already.

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
      Store      : Boolean)
     with Pre => HTTP_Version (Connection) = HTTPv1;
   --  Read message body and store it into Result if Store is True otherwise
   --  the content is discarded.

   procedure Open_Set_Common_Header
     (Connection : in out HTTP_Connection;
      Method     : String;
      URI        : String;
      Headers    : Header_List := Empty_Header_List);
   --  Open the the Connection if it is not open. Send the common HTTP headers
   --  for all requests like the proxy, authentication, user agent, host.

   procedure Set_Authentication_Header
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

   procedure Internal_Upload
     (Connection : in out HTTP_Connection;
      Result     : out Response.Data;
      Filename   : String;
      URI        : String;
      Headers    : Header_List := Empty_Header_List;
      Progress   : access procedure
                     (Total, Sent : Stream_Element_Offset) := null);
   --  Upload file

   procedure Set_Common_Post
     (Connection   : in out HTTP_Connection;
      Data         : Stream_Element_Array;
      URI          : String;
      SOAPAction   : String;
      Content_Type : String;
      Headers      : Header_List := Empty_Header_List);
   --  Send to the server only a POST request with Data
   --  and common headers, using a Connection.

   type Method_Kind is new Status.Request_Method
     with Dynamic_Predicate => Method_Kind in GET | HEAD | PUT | DELETE;

   No_Data : constant Stream_Element_Array := (1 .. 0 => 0);

   procedure Send_Request
     (Connection : in out HTTP_Connection;
      Kind       : Method_Kind;
      Result     : out Response.Data;
      URI        : String;
      Data       : Stream_Element_Array := No_Data;
      Headers    : Header_List := Empty_Header_List);
   --  Send to the server only a POST request with Data
   --  and common headers, using a Connection.

   procedure Set_Header
     (Headers : in out Header_List;
      Header  : String;
      Value   : String := "")
     with Inline;
   --  Append header to Headers if this header is not already present. The
   --  actual header data is given by the constructor. Call Debug_Message if
   --  header is sent.

   procedure Set_HTTP_Connection
     (HTTP_Client : in out HTTP_Connection;
      Sock_Ptr    : Net.Socket_Access);
   --  Initialize HTTP_Client by positioning the socket used as Sock_Ptr

   function Value (V : String) return Unbounded_String;
   --  Returns V as an Unbounded_String if V is not the empty string
   --  otherwise it returns Null_Unbounded_String.

   function Get_Settings
     (Config : AWS.Config.Object) return HTTP2.Frame.Settings.Set;
   --  Returns the config set from Config

end AWS.Client.HTTP_Utils;
