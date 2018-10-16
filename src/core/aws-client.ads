------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2000-2018, AdaCore                     --
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

with Ada.Streams;
with Ada.Strings.Unbounded;

with AWS.Attachments;
with AWS.Default;
with AWS.Headers;
with AWS.Net.SSL.Certificate;
with AWS.Response;

private with Ada.Exceptions;
private with Ada.Finalization;
private with Ada.Real_Time;
private with ZLib;

private with AWS.URL;
private with AWS.Utils;

package AWS.Client is

   use Ada.Streams;
   use Ada.Strings.Unbounded;

   Connection_Error : exception;
   --  Raised if the connection with the server cannot be established

   Protocol_Error   : exception;
   --  Raised if the client receives wrong HTTP protocol data

   No_Data       : constant String;
   --  Used as the default parameter when no data specified for a specific
   --  parameter.

   Retry_Default : constant := 0;
   --  Number of time a data is requested from the Server if the first
   --  time fails.

   --------------
   -- Timeouts --
   --------------

   type Timeouts_Values is private;
   --  Defined the duration for the connect, send, receive and complete
   --  response receive timeouts.

   No_Timeout : constant Timeouts_Values;
   --  No timeout, allow infinite time to send or retrieve data

   function Timeouts
     (Connect  : Duration := Net.Forever;
      Send     : Duration := Net.Forever;
      Receive  : Duration := Net.Forever;
      Response : Duration := Net.Forever) return Timeouts_Values;
   --  Constructor for the timeouts values

   function Timeouts (Each : Duration) return Timeouts_Values;
   --  Constructor for the timeouts values, sets all timeouts values (see
   --  Contructor above) to Each.

   function Connect_Timeout (T : Timeouts_Values) return Duration with Inline;
   --  Returns the corresponding timeout value

   function Send_Timeout (T : Timeouts_Values) return Duration with Inline;
   --  Returns the corresponding timeout value

   function Receive_Timeout (T : Timeouts_Values) return Duration with Inline;
   --  Returns the corresponding timeout value

   function Response_Timeout (T : Timeouts_Values) return Duration with Inline;
   --  Returns the corresponding timeout value

   --------------
   -- Messages --
   --------------

   type Content_Bound is new Integer range -1 .. Integer'Last;

   Undefined : constant Content_Bound := -1;

   type Content_Range is record
      First, Last : Content_Bound := Undefined;
   end record;
   --  Range for partial download

   No_Range : constant Content_Range := (Undefined, Undefined);

   type Authentication_Mode is new AWS.Response.Authentication_Mode;

   type Authentication_Level is private;

   type Authentication_Type is private;

   type Auth_Attempts_Count is private;

   subtype Header_List is Headers.List;
   Empty_Header_List : constant Header_List := Headers.Empty_List;

   subtype Attachment_List is Attachments.List;
   Empty_Attachment_List : constant Attachment_List := Attachments.Empty_List;

   function Get
     (URL                : String;
      User               : String          := No_Data;
      Pwd                : String          := No_Data;
      Proxy              : String          := No_Data;
      Proxy_User         : String          := No_Data;
      Proxy_Pwd          : String          := No_Data;
      Timeouts           : Timeouts_Values := No_Timeout;
      Data_Range         : Content_Range   := No_Range;
      Follow_Redirection : Boolean         := False;
      Certificate        : String          := Default.Client_Certificate;
      Headers            : Header_List     := Empty_Header_List;
      User_Agent         : String          := Default.User_Agent)
      return Response.Data;
   --  Retrieve the message data given a specific URL. It open a connection
   --  with the server and ask for the resource specified in the URL it then
   --  return it in the Response.Data structure.
   --  If User/Pwd are given then it uses it to access the URL.
   --
   --  Optionally it connects through a PROXY using if necessary the Proxy
   --  authentication Proxy_User:Proxy_Pwd.
   --
   --  Only Basic authentication is supported (i.e. Digest is not). Digest
   --  authentication is supported with the keep-alive client API, see below.
   --
   --  If Follow_Redirection is set to True, Get will follow the redirection
   --  information for 301 status code response. Note that this is not
   --  supported for keep-alive connections as the redirection could point to
   --  another server.
   --
   --  Get will retry one time if it fails.

   function Head
     (URL        : String;
      User       : String          := No_Data;
      Pwd        : String          := No_Data;
      Proxy      : String          := No_Data;
      Proxy_User : String          := No_Data;
      Proxy_Pwd  : String          := No_Data;
      Timeouts   : Timeouts_Values := No_Timeout;
      Headers    : Header_List     := Empty_Header_List;
      User_Agent : String          := Default.User_Agent) return Response.Data;
   --  Idem as above but we do not get the message body.
   --  Head will retry one time if it fails.

   function Put
     (URL        : String;
      Data       : String;
      User       : String          := No_Data;
      Pwd        : String          := No_Data;
      Proxy      : String          := No_Data;
      Proxy_User : String          := No_Data;
      Proxy_Pwd  : String          := No_Data;
      Timeouts   : Timeouts_Values := No_Timeout;
      Headers    : Header_List     := Empty_Header_List;
      User_Agent : String          := Default.User_Agent) return Response.Data;
   --  Send to the server URL a PUT request with Data
   --  Put will retry one time if it fails.

   function Delete
     (URL        : String;
      Data       : String;
      User       : String          := No_Data;
      Pwd        : String          := No_Data;
      Proxy      : String          := No_Data;
      Proxy_User : String          := No_Data;
      Proxy_Pwd  : String          := No_Data;
      Timeouts   : Timeouts_Values := No_Timeout;
      Headers    : Header_List     := Empty_Header_List;
      User_Agent : String          := Default.User_Agent) return Response.Data;
   --  Send to the server URL a DELETE request with Data
   --  Delete will retry one time if it fails.

   function Delete
     (URL        : String;
      Data       : Stream_Element_Array;
      User       : String          := No_Data;
      Pwd        : String          := No_Data;
      Proxy      : String          := No_Data;
      Proxy_User : String          := No_Data;
      Proxy_Pwd  : String          := No_Data;
      Timeouts   : Timeouts_Values := No_Timeout;
      Headers    : Header_List     := Empty_Header_List;
      User_Agent : String          := Default.User_Agent) return Response.Data;
   --  Send to the server URL a DELETE request with Data
   --  Delete will retry one time if it fails.

   function Post
     (URL          : String;
      Data         : String;
      Content_Type : String          := No_Data;
      User         : String          := No_Data;
      Pwd          : String          := No_Data;
      Proxy        : String          := No_Data;
      Proxy_User   : String          := No_Data;
      Proxy_Pwd    : String          := No_Data;
      Timeouts     : Timeouts_Values := No_Timeout;
      Attachments  : Attachment_List := Empty_Attachment_List;
      Headers      : Header_List     := Empty_Header_List;
      User_Agent   : String          := Default.User_Agent)
      return Response.Data;
   --  Send to the server URL a POST request with Data
   --  Post will retry one time if it fails.

   function Post
     (URL          : String;
      Data         : Stream_Element_Array;
      Content_Type : String          := No_Data;
      User         : String          := No_Data;
      Pwd          : String          := No_Data;
      Proxy        : String          := No_Data;
      Proxy_User   : String          := No_Data;
      Proxy_Pwd    : String          := No_Data;
      Timeouts     : Timeouts_Values := No_Timeout;
      Attachments  : Attachment_List := Empty_Attachment_List;
      Headers      : Header_List     := Empty_Header_List;
      User_Agent   : String          := Default.User_Agent)
      return Response.Data;
   --  Idem as above but with binary data

   function SOAP_Post
     (URL         : String;
      Data        : String;
      SOAPAction  : String;
      User        : String          := No_Data;
      Pwd         : String          := No_Data;
      Proxy       : String          := No_Data;
      Proxy_User  : String          := No_Data;
      Proxy_Pwd   : String          := No_Data;
      Timeouts    : Timeouts_Values := No_Timeout;
      Attachments : Attachment_List := Empty_Attachment_List;
      Headers     : Header_List     := Empty_Header_List;
      User_Agent  : String          := Default.User_Agent)
      return Response.Data;
   --  Send to the server URL a POST request with Data
   --  Post will retry one time if it fails.

   function Upload
     (URL        : String;
      Filename   : String;
      User       : String          := No_Data;
      Pwd        : String          := No_Data;
      Proxy      : String          := No_Data;
      Proxy_User : String          := No_Data;
      Proxy_Pwd  : String          := No_Data;
      Timeouts   : Timeouts_Values := No_Timeout;
      Headers    : Header_List     := Empty_Header_List;
      Progress   : access procedure
        (Total, Sent : Stream_Element_Offset) := null;
      User_Agent : String          := Default.User_Agent)
      return Response.Data;
   --  This is a file upload request. Filename file's content will be send to
   --  the server at address URL.

   ---------------------------------------
   --  Keep-Alive client implementation --
   ---------------------------------------

   type HTTP_Connection is limited private;
   type HTTP_Connection_Access is access all HTTP_Connection;

   function Create
     (Host        : String;
      User        : String          := No_Data;
      Pwd         : String          := No_Data;
      Proxy       : String          := No_Data;
      Proxy_User  : String          := No_Data;
      Proxy_Pwd   : String          := No_Data;
      Retry       : Natural         := Retry_Default;
      Persistent  : Boolean         := True;
      Timeouts    : Timeouts_Values := No_Timeout;
      Server_Push : Boolean         := False;
      Certificate : String          := Default.Client_Certificate;
      User_Agent  : String          := Default.User_Agent)
      return HTTP_Connection;

   procedure Create
     (Connection  : in out HTTP_Connection;
      Host        : String;
      User        : String          := No_Data;
      Pwd         : String          := No_Data;
      Proxy       : String          := No_Data;
      Proxy_User  : String          := No_Data;
      Proxy_Pwd   : String          := No_Data;
      Retry       : Natural         := Retry_Default;
      Persistent  : Boolean         := True;
      Timeouts    : Timeouts_Values := No_Timeout;
      Server_Push : Boolean         := False;
      SSL_Config  : Net.SSL.Config  := Net.SSL.Null_Config;
      Certificate : String          := Default.Client_Certificate;
      User_Agent  : String          := Default.User_Agent);
   --  Create a new connection. This is to be used with Keep-Alive client API
   --  below. The connection will be tried Retry times if it fails. If
   --  persistent is True the connection will remain open otherwise it will be
   --  closed after each request. User/Pwd are the server authentication info,
   --  Proxy is the name of the proxy server to use, Proxy_User/Proxy_Pwd are
   --  the proxy authentication data. Only Basic authentication is supported
   --  from this routine, for Digest authentication see below. Timeouts are
   --  the send/receive timeouts for each request. If Server_Push is True the
   --  connection will be used to push information to the client.
   --  SSL_Config is to define secure connection configuration. Othewhise
   --  Certificate can be set to specify the certificate filename to use for
   --  the secure connection. User_Agent can be overridden to whatever you want
   --  the client interface to present itself to the server.

   function Get_Certificate
     (Connection : HTTP_Connection) return Net.SSL.Certificate.Object;
   --  Return the certificate used for the secure connection. If this is not a
   --  secure connection, returns Net.SSL.Certificate.Undefined.

   function Host (Connection : HTTP_Connection) return String;
   --  Returns the host as recorded into the connection

   procedure Set_Headers
     (Connection : in out HTTP_Connection; Headers : Header_List) with Inline;
   --  Set additional headers for connection

   procedure Set_WWW_Authentication
     (Connection : in out HTTP_Connection;
      User       : String;
      Pwd        : String;
      Mode       : Authentication_Mode);
   --  Sets the username password and authentication mode for the Web
   --  authentication.
   --
   --  "Any" mean that user want to use Digest server authentication mode but
   --  could use Basic if the server does not support Digest authentication.
   --
   --  "Basic" mean that client will send basic authentication. "Basic"
   --  authentication is send with the first request and is a fast
   --  authentication protocol.
   --
   --  "Digest" mean that the client ask for Digest authentication, it
   --  requires that a first unauthorized request be sent to the server. The
   --  server will answer "nonce" for the authentication protocol to continue.

   procedure Set_Proxy_Authentication
     (Connection : in out HTTP_Connection;
      User       : String;
      Pwd        : String;
      Mode       : Authentication_Mode);
   --  Sets the username, password and authentication mode for the proxy
   --  authentication.

   procedure Set_Persistent
     (Connection : in out HTTP_Connection; Value : Boolean) with Inline;
   --  Change Persistent flag of the connection. If persistent is True the
   --  connection will remain open, otherwise it will be closed after each
   --  request, next request and further would be with "Connection: Close"
   --  header line.

   procedure Clear_SSL_Session (Connection : in out HTTP_Connection);
   --  Avoid reuse SSL session data after reconnect

   procedure Copy_Cookie
     (Source      : HTTP_Connection;
      Destination : in out HTTP_Connection);
   --  Copy a session Id from connection Source to connection Destination.
   --  Allow both connections to share the same user environment. Note that
   --  user's environment are thread-safe.

   function Get_Cookie (Connection : HTTP_Connection) return String
     with Inline;
   --  Get the connection cookie

   procedure Set_Cookie
     (Connection : in out HTTP_Connection; Cookie : String) with Inline;
   --  Set the connection cookie

   function Cipher_Description (Connection : HTTP_Connection) return String;

   function SSL_Session_Id (Connection : HTTP_Connection) return String;
   --  Returns base64 encoded SSL session identifier.
   --  Returns empty string for plain HTTP connections and for not connected
   --  SSL HTTP connections.

   function Read_Until
     (Connection : HTTP_Connection;
      Delimiter  : String;
      Wait       : Boolean := True) return String;
   --  Read data on the Connection until the delimiter (including the
   --  delimiter). It can be used to retrieve the next piece of data from a
   --  push server. If Wait is False the routine is looking for delimiter only
   --  in the internal socket buffer and return empty string if no delimiter
   --  found. If Wait is True and returned data is empty or does not termintate
   --  with the delimiter the server push connection is closed.

   procedure Read_Until
     (Connection : in out HTTP_Connection;
      Delimiter  : String;
      Result     : in out Unbounded_String;
      Wait       : Boolean := True);
   --  Idem as above but returns the result as an Unbounded_String

   procedure Read_Some
     (Connection : in out HTTP_Connection;
      Data       : out Stream_Element_Array;
      Last       : out Stream_Element_Offset);
   --  Reads any available data from the client's connection.
   --  If no data available, it will wait for some data to become available or
   --  until it timeouts. Returns Last < Data'First when there is no data
   --  available in the HTTP response. Connection have to be created with
   --  parameter Server_Push => True.

   procedure Read
     (Connection : in out HTTP_Connection;
      Data       : out Stream_Element_Array;
      Last       : out Stream_Element_Offset);
   --  Reads data from the client's connection until Data buffer if filled
   --  or it reached the end of the response. Returns Last < Data'Last if
   --  there is no more data available in HTTP response. Connection have
   --  to be created with parameter Server_Push => True.

   procedure Get
     (Connection : in out HTTP_Connection;
      Result     : out Response.Data;
      URI        : String          := No_Data;
      Data_Range : Content_Range   := No_Range;
      Headers    : Header_List     := Empty_Header_List);
   --  Same as Get above but using a Connection

   procedure Head
     (Connection : in out HTTP_Connection;
      Result     : out Response.Data;
      URI        : String          := No_Data;
      Headers    : Header_List     := Empty_Header_List);
   --  Same as Head above but using a Connection

   procedure Delete
     (Connection : in out HTTP_Connection;
      Result     : out Response.Data;
      Data       : String;
      URI        : String          := No_Data;
      Headers    : Header_List     := Empty_Header_List);
   --  Same as Delete above but using a Connection

   procedure Delete
     (Connection : in out HTTP_Connection;
      Result     : out Response.Data;
      Data       : Stream_Element_Array;
      URI        : String          := No_Data;
      Headers    : Header_List     := Empty_Header_List);
   --  Same as Delete above but using a Connection

   procedure Put
     (Connection : in out HTTP_Connection;
      Result     : out Response.Data;
      Data       : String;
      URI        : String      := No_Data;
      Headers    : Header_List := Empty_Header_List);
   --  Same as Put above but using a Connection

   procedure Put
     (Connection : in out HTTP_Connection;
      Result     : out Response.Data;
      Data       : Stream_Element_Array;
      URI        : String      := No_Data;
      Headers    : Header_List := Empty_Header_List);

   procedure Post
     (Connection   : in out HTTP_Connection;
      Result       : out Response.Data;
      Data         : String;
      Content_Type : String          := No_Data;
      URI          : String          := No_Data;
      Attachments  : Attachment_List := Empty_Attachment_List;
      Headers      : Header_List     := Empty_Header_List);
   --  Same as Post above but using a Connection

   procedure Post
     (Connection   : in out HTTP_Connection;
      Result       : out Response.Data;
      Data         : Stream_Element_Array;
      Content_Type : String          := No_Data;
      URI          : String          := No_Data;
      Attachments  : Attachment_List := Empty_Attachment_List;
      Headers      : Header_List     := Empty_Header_List);
   --  Same as Post above but using a Connection

   procedure Upload
     (Connection : in out HTTP_Connection;
      Result     : out Response.Data;
      Filename   : String;
      URI        : String          := No_Data;
      Headers    : Header_List     := Empty_Header_List;
      Progress   : access procedure
        (Total, Sent : Stream_Element_Offset) := null);
   --  Same as Upload above but using a Connection

   procedure SOAP_Post
     (Connection  : HTTP_Connection;
      Result      : out Response.Data;
      SOAPAction  : String;
      Data        : String;
      Streaming   : Boolean         := False;
      Attachments : Attachment_List := Empty_Attachment_List;
      Headers     : Header_List     := Empty_Header_List);
   --  Same as SOAP_Post above but using a Connection
   --  Streaming is to be able to parse response XML on the fly,
   --  without intermediate buffer.

   procedure Close (Connection : in out HTTP_Connection);
   --  Close connection, it releases all associated resources

   procedure Set_Streaming_Output
     (Connection : in out HTTP_Connection;
      Value      : Boolean)
   with Inline;
   --  Call this routine with Value => True to be able to read data as a
   --  stream by using Read and/or Read_Some routines above. Note that
   --  Connection is already in Streaming mode if it has been created
   --  with Server_Push => True.

   procedure Set_Debug (On : Boolean);
   --  Set debug mode on/off. If debug is activated the request header and the
   --  server response header will be displayed.

   function Get_Socket (Connection : HTTP_Connection) return Net.Socket_Access;
   --  Retrieve the socket used for the connection

private

   use Ada;
   use Ada.Exceptions;

   Forever : constant Real_Time.Time_Span := Real_Time.Time_Span_Last;

   type Timeouts_Values is record
      Connect  : Duration            := Net.Forever;
      Send     : Duration            := Net.Forever;
      Receive  : Duration            := Net.Forever;
      Response : Real_Time.Time_Span := Forever;
   end record;

   No_Timeout : constant Timeouts_Values :=
                  (Response => Forever, others => Net.Forever);
   No_Data    : constant String := "";

   Undefined_Length : Response.Content_Length_Type
     renames Response.Undefined_Length;

   type Authentication_Level is (WWW, Proxy);

   type Authentication_Type is record
      User      : Unbounded_String;
      Pwd       : Unbounded_String;
      --  Mode the user want to use
      Init_Mode : Authentication_Mode := Any;

      --  "Any" mean without authentication
      Work_Mode : Authentication_Mode := Any;
      Requested : Boolean := False;

      --  Fields below are for digest authentication only
      Realm     : Unbounded_String;
      Nonce     : Unbounded_String;
      QOP       : Unbounded_String;
      NC        : Natural := 0;
   end record;

   type Authentication_Set is
     array (Authentication_Level) of Authentication_Type;

   type Auth_Attempts_Count is
     array (Authentication_Level) of Natural range 0 .. 2;

   type Transfer_Type is
     (None,           -- Connection is not in transfer state
      Chunked,        -- Transfer-encoding chunked
      Content_Length, -- Content-Length defined
      Until_Close,    -- Document end on close socket
      End_Response);  -- Document is over

   type HTTP_Connection is new Finalization.Limited_Controlled with record
      Self : not null access HTTP_Connection :=
               HTTP_Connection'Unchecked_Access;

      Connect_URL        : AWS.URL.Object;
      Host               : Unbounded_String;
      Host_URL           : AWS.URL.Object;
      Proxy              : Unbounded_String;
      Proxy_URL          : AWS.URL.Object;
      Headers            : Header_List;
      Auth               : Authentication_Set;
      Opened             : Boolean                      := False;
      Persistent         : Boolean;
      Streaming          : Boolean;
      Cookie             : Unbounded_String;
      Socket             : Net.Socket_Access;
      SSL_Session        : Net.SSL.Session_Type;
      Retry              : Natural;
      Timeouts           : Timeouts_Values;
      Data_Range         : Content_Range;
      Certificate        : Unbounded_String;
      User_Agent         : Unbounded_String;
      SSL_Config         : AWS.Net.SSL.Config;
      Default_SSL_Config : Boolean                      := False;
      Length             : Response.Content_Length_Type := Undefined_Length;
      Transfer           : Transfer_Type                := None;
      Decode_Filter      : ZLib.Filter_Type;
      Decode_Buffer      : Utils.Stream_Element_Array_Access;
      Decode_First       : Stream_Element_Offset;
      Decode_Last        : Stream_Element_Offset;
   end record;

   overriding procedure Finalize (Connection : in out HTTP_Connection);

   procedure Debug_Message (Prefix, Message : String) with Inline;
   --  Output Message prefixed with Prefix if Debug_On is True and does
   --  nothing otherwise.

   procedure Debug_Exception (E : Exception_Occurrence)
     with Inline;
   --  Output E exception if Debug_On is True and does nothing otherwise

   procedure Error_Processing
     (Connection : in out HTTP_Connection;
      Try_Count  : in out Natural;
      Result     : out Response.Data;
      Context    : String;
      E          : Exception_Occurrence;
      Stamp      : Ada.Real_Time.Time);
   --  Check timeout and Try_Count and set error responce into Result
   --  if necessary.

   function Get_Socket (Connection : HTTP_Connection) return Net.Socket_Access
      is (Connection.Socket);

end AWS.Client;
