------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2000-2002                          --
--                               ACT-Europe                                 --
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

with Ada.Streams;
with Ada.Strings.Unbounded;

with AWS.Net;
with AWS.Response;
with AWS.URL;

package AWS.Client is

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

   type Authentication_Mode is new AWS.Response.Authentication_Mode;

   type Timeouts_Values is record
      Send    : Natural;
      Receive : Natural;
   end record;
   --  Defined the number of seconds for the send and receive timeout

   No_Timeout : constant Timeouts_Values;
   --  No timeout, allow infinite time to send or retreive data

   function Get
     (URL                : in String;
      User               : in String          := No_Data;
      Pwd                : in String          := No_Data;
      Proxy              : in String          := No_Data;
      Proxy_User         : in String          := No_Data;
      Proxy_Pwd          : in String          := No_Data;
      Timeouts           : in Timeouts_Values := No_Timeout;
      Follow_Redirection : in Boolean         := False)
      return Response.Data;
   --  retreive the message data given a specific URL. It open a connection
   --  with the server and ask for the resource specified in the URL it then
   --  return it in the Response.Data structure.
   --  If User/Pwd are given then it uses it to access the URL.
   --
   --  Eventually it connect through a PROXY using if necessary the Proxy
   --  authentification Proxy_User:Proxy_Pwd.
   --
   --  Only Basic authetification is supported (i.e. Digest is not). Digest
   --  authentication is supported with the keep-alive client API, see below.
   --
   --  If Follow_Redirection is set to True, Get will follow the redirection
   --  information for 301 status code response. Note that this is not
   --  supported for keep-alive connections as the redirection could point to
   --  another server.
   --
   --  Get will retry one time if it fails.

   function Head
     (URL        : in String;
      User       : in String          := No_Data;
      Pwd        : in String          := No_Data;
      Proxy      : in String          := No_Data;
      Proxy_User : in String          := No_Data;
      Proxy_Pwd  : in String          := No_Data;
      Timeouts   : in Timeouts_Values := No_Timeout)
      return Response.Data;
   --  Idem as above but we do not get the message body.
   --  Head will retry one time if it fails.

   function Put
     (URL        : in String;
      Data       : in String;
      User       : in String          := No_Data;
      Pwd        : in String          := No_Data;
      Proxy      : in String          := No_Data;
      Proxy_User : in String          := No_Data;
      Proxy_Pwd  : in String          := No_Data;
      Timeouts   : in Timeouts_Values := No_Timeout)
      return Response.Data;
   --  Send to the server URL a PUT request with Data
   --  Put will retry one time if it fails.

   function Post
     (URL        : in String;
      Data       : in String;
      User       : in String          := No_Data;
      Pwd        : in String          := No_Data;
      Proxy      : in String          := No_Data;
      Proxy_User : in String          := No_Data;
      Proxy_Pwd  : in String          := No_Data;
      Timeouts   : in Timeouts_Values := No_Timeout)
      return Response.Data;
   --  Send to the server URL a POST request with Data
   --  Post will retry one time if it fails.

   function Post
     (URL        : in String;
      Data       : in Ada.Streams.Stream_Element_Array;
      User       : in String          := No_Data;
      Pwd        : in String          := No_Data;
      Proxy      : in String          := No_Data;
      Proxy_User : in String          := No_Data;
      Proxy_Pwd  : in String          := No_Data;
      Timeouts   : in Timeouts_Values := No_Timeout)
      return Response.Data;
   --  Idem as above but with binary data.

   function SOAP_Post
     (URL        : in String;
      Data       : in String;
      SOAPAction : in String;
      User       : in String          := No_Data;
      Pwd        : in String          := No_Data;
      Proxy      : in String          := No_Data;
      Proxy_User : in String          := No_Data;
      Proxy_Pwd  : in String          := No_Data;
      Timeouts   : in Timeouts_Values := No_Timeout)
     return Response.Data;
   --  Send to the server URL a POST request with Data
   --  Post will retry one time if it fails.

   function Upload
     (URL        : in String;
      Filename   : in String;
      User       : in String          := No_Data;
      Pwd        : in String          := No_Data;
      Proxy      : in String          := No_Data;
      Proxy_User : in String          := No_Data;
      Proxy_Pwd  : in String          := No_Data;
      Timeouts   : in Timeouts_Values := No_Timeout)
      return Response.Data;
   --  This is a file upload request. Filename file's content will be send to
   --  the server at address URL.

   ---------------------------------------
   --  Keep-Alive client implementation --
   ---------------------------------------

   type HTTP_Connection is limited private;

   procedure Create
     (Connection  : in out HTTP_Connection;
      Host        : in     String;
      User        : in     String          := No_Data;
      Pwd         : in     String          := No_Data;
      Proxy       : in     String          := No_Data;
      Proxy_User  : in     String          := No_Data;
      Proxy_Pwd   : in     String          := No_Data;
      Retry       : in     Natural         := Retry_Default;
      SOAPAction  : in     String          := No_Data;
      Persistent  : in     Boolean         := True;
      Timeouts    : in     Timeouts_Values := No_Timeout;
      Server_Push : in     Boolean         := False);
   --  Create a new connection. This is to be used with Keep-Alive client API
   --  below. The request will be tried Retry time if it fails.

   procedure Set_WWW_Authentication
     (Connection : in out HTTP_Connection;
      User       : in     String;
      Pwd        : in     String;
      Mode       : in     Authentication_Mode);
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
      User       : in     String;
      Pwd        : in     String;
      Mode       : in     Authentication_Mode);
   --  Sets the username, password and authentication mode for the proxy
   --  authentication.

   procedure Copy_Cookie
     (Source      : in     HTTP_Connection;
      Destination : in out HTTP_Connection);
   --  Copy a session ID from connection Source to connection Destination.
   --  Allow both connections to share the same user environment. Note that
   --  user's environemnt are thread-safe.

   function Read_Until
     (Connection : in HTTP_Connection;
      Delimiter  : in String)
      return String;
   --  Read data on the Connection until the delimiter (including the
   --  delimiter). It can be used to retreive the next piece of data from a
   --  push server. If returned data is empty or does not termintate with the
   --  delimiter the server push connection is closed.

   procedure Read_Until
     (Connection : in out HTTP_Connection;
      Delimiter  : in     String;
      Result     : in out Ada.Strings.Unbounded.Unbounded_String);
   --  Idem as above but returns the result as an Unbounded_String.

   procedure Get
     (Connection : in out HTTP_Connection;
      Result     :    out Response.Data;
      URI        : in     String          := No_Data);
   --  Same as Get above but using a Connection.

   procedure Head
     (Connection : in out HTTP_Connection;
      Result     :    out Response.Data;
      URI        : in     String          := No_Data);
   --  Same as Head above but using a Connection.

   procedure Put
     (Connection : in out HTTP_Connection;
      Result     :    out Response.Data;
      Data       : in     String;
      URI        : in     String          := No_Data);
   --  Same as Put above but using a Connection.

   procedure Post
     (Connection : in out HTTP_Connection;
      Result     :    out Response.Data;
      Data       : in     String;
      URI        : in     String          := No_Data);
   --  Same as Post above but using a Connection.

   procedure Post
     (Connection : in out HTTP_Connection;
      Result     :    out Response.Data;
      Data       : in     Ada.Streams.Stream_Element_Array;
      URI        : in     String          := No_Data);
   --  Same as Post above but using a Connection.

   procedure Upload
     (Connection : in out HTTP_Connection;
      Result     :    out Response.Data;
      Filename   : in     String;
      URI        : in     String          := No_Data);
   --  Same as Upload above but using a Connection.

   function SOAP_Post
     (Connection  : access HTTP_Connection;
      Data        : in     String)
      return Response.Data;
   --  Same as SOAP_Post above but using a Connection.

   procedure Close (Connection : in out HTTP_Connection);
   --  Close connection, it releases all associated resources.

   procedure Set_Debug (On : in Boolean);
   --  Set debug mode on/off. If debug is activated the request header and the
   --  server response header will be displayed.

private

   use Ada.Strings.Unbounded;

   No_Timeout : constant Timeouts_Values := (0, 0);
   No_Data    : constant String := "";

   type Client_Phase is (Not_Monitored, Send, Receive, Stopped);

   type HTTP_Connection_Access is access all HTTP_Connection;

   --  ??? Cleaner_Task is used to monitor the timeouts during the Send and
   --  Receive phase. This is the current implementation and should be fixed
   --  at some point. Right now there is no cross-platforms implementation of
   --  a Socket timeout.

   task type Cleaner_Task is

      entry Start (Connection : in HTTP_Connection_Access);
      --  Task initialization, pass the HTTP_Connection to monitor.

      entry Next_Phase;
      --  Change the client phase.

   end Cleaner_Task;

   type Cleaner_Access is access Cleaner_Task;

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

   type HTTP_Connection is limited record
      Self : HTTP_Connection_Access := HTTP_Connection'Unchecked_Access;

      Connect_URL   : AWS.URL.Object;
      Host          : Unbounded_String;
      Host_URL      : AWS.URL.Object;
      Proxy         : Unbounded_String;
      Proxy_URL     : AWS.URL.Object;
      Auth          : Authentication_Set;
      Opened        : Boolean;
      Persistent    : Boolean;
      Server_Push   : Boolean;
      SOAPAction    : Unbounded_String;
      Cookie        : Unbounded_String;
      Socket        : Net.Socket_Access;
      Retry         : Natural;
      Current_Phase : Client_Phase;
      Timeouts      : Timeouts_Values;
      Cleaner       : Cleaner_Access;
   end record;

end AWS.Client;
