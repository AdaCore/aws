------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2000-2004                          --
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

with Ada.Calendar;
with Ada.Characters.Handling;
with Ada.Exceptions;
with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Ada.Strings.Fixed;
with Ada.Streams.Stream_IO;

with GNAT.Calendar.Time_IO;

with AWS.Digest;
with AWS.Headers.Set;
with AWS.Headers.Values;
with AWS.Messages;
with AWS.MIME;
with AWS.Net.Buffered;
with AWS.OS_Lib;
with AWS.Response.Set;
with AWS.Translator;
with AWS.Utils;
with AWS.Net.SSL;

package body AWS.Client is

   use Ada;
   use Ada.Strings.Unbounded;

   type Auth_Attempts_Count is
     array (Authentication_Level) of Natural range 0 .. 2;

   Debug_On : Boolean := False;

   procedure Debug_Message (Prefix, Message : in String);
   pragma Inline (Debug_Message);
   --  Output Message prefixed with Prefix if Debug_On is True and does
   --  nothing otherwise.

   procedure Get_Response
     (Connection : in out HTTP_Connection;
      Result     :    out Response.Data;
      Get_Body   : in     Boolean         := True);
   --  Receives response from server for GET and POST and HEAD commands.
   --  If Get_Body is set then the message body will be read.

   procedure Decrement_Authentication_Attempt
     (Connection : in out HTTP_Connection;
      Counter    : in out Auth_Attempts_Count;
      Over       :    out Boolean);
   --  Counts the authentication attempts. Over is set to True when
   --  authentication attempts are over.

   procedure Set_Authentication
     (Auth       :    out Authentication_Type;
      User       : in     String;
      Pwd        : in     String;
      Mode       : in     Authentication_Mode);
   --  Internal procedure to set authentication parameters.

   procedure Parse_Header
     (Connection        : in out HTTP_Connection;
      Answer            :    out Response.Data;
      Keep_Alive        :    out Boolean);
   --  Read server answer and set corresponding variable with the value
   --  read. Most of the fields are ignored right now.

   procedure Connect (Connection : in out HTTP_Connection);
   --  Open the connection. Raises Connection_Error if it is not possible to
   --  establish the connection. In this case all resources are released and
   --  Connection.Opened is set to False.

   procedure Disconnect (Connection : in out HTTP_Connection);
   --  Close connection. Further use is not possible.

   procedure Open_Send_Common_Header
     (Connection : in out HTTP_Connection;
      Method     : in     String;
      URI        : in     String);
   --  Open the the Connection if it is not open. Send the common HTTP headers
   --  for all requests like the proxy, authentification, user agent, host.

   procedure Internal_Post
     (Connection   : in out HTTP_Connection;
      Result       :    out Response.Data;
      Data         : in     Streams.Stream_Element_Array;
      URI          : in     String;
      SOAPAction   : in     String;
      Content_Type : in     String);
   --  Common base routine for Post and SOAP_Post routines.

   procedure Send_Header
     (Sock : in Net.Socket_Type'Class;
      Data : in String);
   pragma Inline (Send_Header);
   --  Send header Data to socket and call Debug_Message.

   function Value (V : in String) return Unbounded_String;
   --  Returns V as an Unbounded_String if V is not the empty string
   --  otherwise it returns Null_Unbounded_String.

   -----------
   -- Close --
   -----------

   procedure Close (Connection : in out HTTP_Connection) is
   begin
      Net.SSL.Release (Connection.SSL_Config);

      Disconnect (Connection);
      Net.Free (Connection.Socket);

      if ZLib.Is_Open (Connection.Decode_Filter) then
         ZLib.Close (Connection.Decode_Filter, Ignore_Error => True);
      end if;

      Utils.Free (Connection.Decode_Buffer);
   end Close;

   -------------
   -- Connect --
   -------------

   procedure Connect (Connection : in out HTTP_Connection) is
      use type Net.Socket_Access;
      Connect_URL : AWS.URL.Object renames Connection.Connect_URL;
      Security    : constant Boolean := AWS.URL.Security (Connect_URL);
   begin
      pragma Assert (not Connection.Opened);
      --  This should never be called with an open connection.

      --  Keep-alive reconnection will be with old socket. We cannot reuse it,
      --  and have to free it.

      if Connection.Socket /= null then
         Net.Free (Connection.Socket);
      end if;

      Connection.Socket := Net.Socket (Security);

      if Security then
         --  This is a secure connection, set the SSL config for this socket
         Net.SSL.Set_Config
           (Net.SSL.Socket_Type (Connection.Socket.all),
            Connection.SSL_Config);
      end if;

      Net.Connect
        (Connection.Socket.all,
         AWS.URL.Host (Connect_URL), AWS.URL.Port (Connect_URL));

      Connection.Opened := True;
   exception
      when E : Net.Socket_Error =>
         Connection.Opened := False;

         Exceptions.Raise_Exception
           (Connection_Error'Identity,
            "can't connect to " & AWS.URL.URL (Connect_URL)
              & " -> " & Exceptions.Exception_Information (E));
   end Connect;

   -----------------
   -- Copy_Cookie --
   -----------------

   procedure Copy_Cookie
     (Source      : in     HTTP_Connection;
      Destination : in out HTTP_Connection) is
   begin
      Destination.Cookie := Source.Cookie;
   end Copy_Cookie;

   ------------
   -- Create --
   ------------

   procedure Create
     (Connection  : in out HTTP_Connection;
      Host        : in     String;
      User        : in     String          := No_Data;
      Pwd         : in     String          := No_Data;
      Proxy       : in     String          := No_Data;
      Proxy_User  : in     String          := No_Data;
      Proxy_Pwd   : in     String          := No_Data;
      Retry       : in     Natural         := Retry_Default;
      Persistent  : in     Boolean         := True;
      Timeouts    : in     Timeouts_Values := No_Timeout;
      Server_Push : in     Boolean         := False;
      Certificate : in     String          := Default.Client_Certificate;
      User_Agent  : in     String          := Default.User_Agent)
   is
      Connect_URL : AWS.URL.Object;
      Host_URL    : AWS.URL.Object := AWS.URL.Parse (Host);
      Proxy_URL   : AWS.URL.Object := AWS.URL.Parse (Proxy);

   begin
      --  If there is a proxy, the host to connect to is the proxy otherwise
      --  we connect to the Web server.

      if Proxy = No_Data then
         Connect_URL := Host_URL;
      else
         Connect_URL := Proxy_URL;
      end if;

      Connection.Host                     := To_Unbounded_String (Host);
      Connection.Host_URL                 := Host_URL;
      Connection.Connect_URL              := Connect_URL;
      Connection.Auth (WWW).User          := Value (User);
      Connection.Auth (WWW).Pwd           := Value (Pwd);
      Connection.Proxy                    := Value (Proxy);
      Connection.Proxy_URL                := Proxy_URL;
      Connection.Auth (Client.Proxy).User := Value (Proxy_User);
      Connection.Auth (Client.Proxy).Pwd  := Value (Proxy_Pwd);
      Connection.Retry                    := Create.Retry;
      Connection.Cookie                   := Null_Unbounded_String;
      Connection.Persistent               := Persistent;
      Connection.Streaming                := Server_Push;
      Connection.Certificate              := To_Unbounded_String (Certificate);

      Connection.User_Agent := To_Unbounded_String (User_Agent);

      if Timeouts.Receive > 0 then
         Connection.Read_Timeout := Duration (Timeouts.Receive);
      else
         Connection.Read_Timeout := Net.Forever;
      end if;

      if Timeouts.Send > 0 then
         Connection.Write_Timeout := Duration (Timeouts.Send);
      else
         Connection.Write_Timeout := Net.Forever;
      end if;

      --  If we have set the proxy or standard authentication we must set the
      --  authentication mode to Basic.

      if Proxy_User /= No_Data then
         Connection.Auth (Client.Proxy).Work_Mode := Basic;
      end if;

      if User /= No_Data then
         Connection.Auth (WWW).Work_Mode := Basic;
      end if;

      if URL.Security (Host_URL) then
         --  This is a secure connection, initialize the SSL layer

         Net.SSL.Initialize
           (Connection.SSL_Config, Certificate, Net.SSL.SSLv23_Client);
      end if;

      --  Establish the connection now

      Connect (Connection);

      if Persistent and then Connection.Retry = 0 then
         --  In this case the connection termination can be initiated by the
         --  server or the client after a period. So the connection could be
         --  closed while trying to get some data from the server. To be nicer
         --  from user's point of view just make sure we retry at least one
         --  time before reporting an error.
         Connection.Retry := 1;
      end if;
   end Create;

   -------------------
   -- Debug_Message --
   -------------------

   procedure Debug_Message (Prefix, Message : in String) is
   begin
      if Debug_On then
         Text_IO.Put_Line (Prefix & Message);
      end if;
   end Debug_Message;

   --------------------------------------
   -- Decrement_Authentication_Attempt --
   --------------------------------------

   procedure Decrement_Authentication_Attempt
     (Connection : in out HTTP_Connection;
      Counter    : in out Auth_Attempts_Count;
      Over       :    out Boolean)
   is
      type Over_Data is array (Authentication_Level) of Boolean;

      Is_Over    : constant Over_Data := (others => True);
      Over_Level : Over_Data          := (others => True);
   begin
      for Level in Authentication_Level'Range loop
         if Connection.Auth (Level).Requested then
            Counter (Level)    := Counter (Level) - 1;
            Over_Level (Level) := Counter (Level) = 0;
         end if;
      end loop;

      Over := Over_Level = Is_Over;
   end Decrement_Authentication_Attempt;

   ----------------
   -- Disconnect --
   ----------------

   procedure Disconnect (Connection : in out HTTP_Connection) is
      use type Net.Socket_Access;
   begin
      if Connection.Opened then
         Connection.Opened := False;

         if Connection.Socket /= null then
            Net.Shutdown (Connection.Socket.all);
         end if;
      end if;
   end Disconnect;

   ---------
   -- Get --
   ---------

   function Get
     (URL                : in String;
      User               : in String          := No_Data;
      Pwd                : in String          := No_Data;
      Proxy              : in String          := No_Data;
      Proxy_User         : in String          := No_Data;
      Proxy_Pwd          : in String          := No_Data;
      Timeouts           : in Timeouts_Values := No_Timeout;
      Follow_Redirection : in Boolean         := False)
      return Response.Data
   is
      use type Messages.Status_Code;

      Result : Response.Data;

   begin
      declare
         Connection : HTTP_Connection;
      begin
         Create (Connection,
                 URL, User, Pwd, Proxy, Proxy_User, Proxy_Pwd,
                 Persistent => False,
                 Timeouts   => Timeouts);

         Get (Connection, Result);

         Close (Connection);
      exception
         when others =>
            Close (Connection);
            raise;
      end;

      if Follow_Redirection
        and then Response.Status_Code (Result) = Messages.S305
      then
         --  This is "Use Proxy" message, Location point to the proxy to use.
         --  We do not have the login/password for the proxy.
         return Get
           (URL, User, Pwd, Response.Location (Result),
            Timeouts => Timeouts, Follow_Redirection => Follow_Redirection);

      elsif Follow_Redirection
          and then
        Response.Status_Code (Result) in Messages.S301 .. Messages.S307
          and then
        Response.Status_Code (Result) /= Messages.S304
      then
         --  All other redirections, 304 is not one of them.
         return Get
           (Response.Location (Result), User, Pwd,
            Proxy, Proxy_User, Proxy_Pwd, Timeouts, Follow_Redirection);
      else
         return Result;
      end if;
   end Get;

   ---------
   -- Get --
   ---------

   procedure Get
     (Connection : in out HTTP_Connection;
      Result     :    out Response.Data;
      URI        : in     String          := No_Data)
   is
      Try_Count     : Natural := Connection.Retry;

      Auth_Attempts : Auth_Attempts_Count := (others => 2);
      Auth_Is_Over  : Boolean;

   begin
      Retry : loop
         begin
            Open_Send_Common_Header (Connection, "GET", URI);

            Net.Buffered.New_Line (Connection.Socket.all);

            Get_Response (Connection, Result, not Connection.Streaming);

            Decrement_Authentication_Attempt
              (Connection, Auth_Attempts, Auth_Is_Over);

            if Auth_Is_Over then
               return;
            end if;

         exception
            when Net.Socket_Error =>

               Disconnect (Connection);

               if Try_Count = 0 then
                  Result := Response.Build
                    (MIME.Text_HTML, "Get Timeout", Messages.S408);

                  exit Retry;
               end if;

               Try_Count := Try_Count - 1;
         end;
      end loop Retry;
   end Get;

   ---------------------
   -- Get_Certificate --
   ---------------------

   function Get_Certificate
     (Connection : in HTTP_Connection)
      return Net.SSL.Certificate.Object
   is
      use type Net.Socket_Access;
   begin
      if Connection.Socket = null
        or else Connection.Socket.all not in AWS.Net.SSL.Socket_Type'Class
      then
         return Net.SSL.Certificate.Undefined;
      else
         return Net.SSL.Certificate.Get
           (Net.SSL.Socket_Type (Connection.Socket.all));
      end if;
   end Get_Certificate;

   ------------------
   -- Get_Response --
   ------------------

   procedure Get_Response
     (Connection : in out HTTP_Connection;
      Result     :    out Response.Data;
      Get_Body   : in     Boolean         := True)
   is
      procedure Disconnect;
      --  close connection socket.

      Sock       : Net.Socket_Type'Class renames Connection.Socket.all;

      Keep_Alive : Boolean;

      ----------------
      -- Disconnect --
      ----------------

      procedure Disconnect is
      begin
         if not Keep_Alive and not Connection.Streaming then
            Disconnect (Connection);
         end if;
      end Disconnect;

   begin
      Net.Set_Timeout (Sock, Connection.Read_Timeout);

      --  Clear the data in the response

      Response.Set.Clear (Result);

      Parse_Header (Connection, Result, Keep_Alive);

      declare
         TE     : constant String
           := Response.Header (Result, Messages.Transfer_Encoding_Token);
         CT_Len : constant Integer := Response.Content_Length (Result);
      begin
         if TE = "chunked" then

            --  A chuncked message is written on the stream as list of data
            --  chunk. Each chunk has the following format:
            --
            --  <N : the chunk size in hexadecimal> CRLF
            --  <N * BYTES : the data> CRLF
            --
            --  The termination chunk is:
            --
            --  0 CRLF
            --  CRLF
            --

            Connection.Transfer := Chunked;
            Connection.Length   := 0;

         elsif CT_Len = Response.Undefined_Length then
            Connection.Transfer := Until_Close;
         else
            Connection.Transfer := Content_Length;
            Connection.Length   := CT_Len;
         end if;
      end;

      if not Get_Body then
         Disconnect;
         return;
      end if;

      --  Read the message body

      loop
         declare
            use Ada.Streams;
            Buffer : Stream_Element_Array (1 .. 8096);
            Last   : Stream_Element_Offset;
         begin
            Read_Some (Connection, Buffer, Last);
            exit when Last < Buffer'First;
            Response.Set.Append_Body (Result, Buffer (Buffer'First .. Last));
         end;
      end loop;

      Connection.Transfer := None;

      Disconnect;
   end Get_Response;

   ----------
   -- Head --
   ----------

   function Head
     (URL        : in String;
      User       : in String          := No_Data;
      Pwd        : in String          := No_Data;
      Proxy      : in String          := No_Data;
      Proxy_User : in String          := No_Data;
      Proxy_Pwd  : in String          := No_Data;
      Timeouts   : in Timeouts_Values := No_Timeout)
      return Response.Data
   is
      Connection : HTTP_Connection;
      Result     : Response.Data;

   begin
      Create (Connection,
              URL, User, Pwd, Proxy, Proxy_User, Proxy_Pwd,
              Persistent => False,
              Timeouts   => Timeouts);

      Head (Connection, Result);
      Close (Connection);

      return Result;

   exception
      when others =>
         Close (Connection);
         raise;
   end Head;

   ----------
   -- Head --
   ----------

   procedure Head
     (Connection : in out HTTP_Connection;
      Result     :    out Response.Data;
      URI        : in     String := No_Data)
   is
      Try_Count : Natural := Connection.Retry;

      Auth_Attempts : Auth_Attempts_Count := (others => 2);
      Auth_Is_Over  : Boolean;
   begin
      Retry : loop
         begin
            Open_Send_Common_Header (Connection, "HEAD", URI);

            Net.Buffered.New_Line (Connection.Socket.all);

            Get_Response (Connection, Result, Get_Body => False);

            Decrement_Authentication_Attempt
              (Connection, Auth_Attempts, Auth_Is_Over);

            if Auth_Is_Over then
               return;
            end if;

         exception
            when Net.Socket_Error =>

               Disconnect (Connection);

               if Try_Count = 0 then
                  Result := Response.Build
                    (MIME.Text_HTML, "Head Timeout", Messages.S408);
                  exit Retry;
               end if;

               Try_Count := Try_Count - 1;
         end;
      end loop Retry;
   end Head;

   -------------------
   -- Internal_Post --
   -------------------

   procedure Internal_Post
     (Connection   : in out HTTP_Connection;
      Result       :    out Response.Data;
      Data         : in     Streams.Stream_Element_Array;
      URI          : in     String;
      SOAPAction   : in     String;
      Content_Type : in     String)
   is
      Try_Count : Natural := Connection.Retry;

      Auth_Attempts : Auth_Attempts_Count := (others => 2);
      Auth_Is_Over  : Boolean;

   begin
      Retry : loop
         begin
            Open_Send_Common_Header (Connection, "POST", URI);

            declare
               Sock : Net.Socket_Type'Class renames Connection.Socket.all;
            begin

               if Content_Type /= No_Data then
                  Send_Header (Sock, Messages.Content_Type (Content_Type));
               end if;

               if SOAPAction /= No_Data then
                  --  SOAP header

                  Send_Header (Sock, Messages.SOAPAction (SOAPAction));
               end if;

               --  Send message Content_Length

               Send_Header (Sock, Messages.Content_Length (Data'Length));

               Net.Buffered.New_Line (Sock);

               --  Send message body

               Net.Buffered.Write (Sock, Data);
            end;

            --  Get answer from server

            Get_Response (Connection, Result, not Connection.Streaming);

            Decrement_Authentication_Attempt
              (Connection, Auth_Attempts, Auth_Is_Over);

            if Auth_Is_Over then
               return;
            end if;

         exception

            when Net.Socket_Error =>

               Disconnect (Connection);

               if Try_Count = 0 then
                  Result := Response.Build
                    (MIME.Text_HTML, "Post Timeout", Messages.S408);
                  exit Retry;
               end if;

               Try_Count := Try_Count - 1;
         end;
      end loop Retry;
   end Internal_Post;

   -----------------------------
   -- Open_Send_Common_Header --
   -----------------------------

   procedure Open_Send_Common_Header
     (Connection : in out HTTP_Connection;
      Method     : in     String;
      URI        : in     String)
   is
      Sock    : Net.Socket_Access renames Connection.Socket;
      No_Data : Unbounded_String renames Null_Unbounded_String;

      procedure Send_Authentication_Header
        (Token       : in     String;
         Data        : in out Authentication_Type);
      --  Send the authentication header for proxy or for server

      function HTTP_Prefix (Security : in Boolean) return String;
      --  Returns "http://" or "https://" if Security is set to True

      function Persistence return String;
      --  Returns "Keep-Alive" is we have a persistent connection and "Close"
      --  otherwise.

      function Port_Not_Default (Port : in Positive) return String;
      --  Returns the port image (preceded by character ':') if it is not the
      --  default port.

      -----------------
      -- HTTP_Prefix --
      -----------------

      function HTTP_Prefix (Security : in Boolean) return String is
      begin
         if Security then
            return "https://";
         else
            return "http://";
         end if;
      end HTTP_Prefix;

      -----------------
      -- Persistence --
      -----------------

      function Persistence return String is
      begin
         if Connection.Persistent then
            return "Keep-Alive";
         else
            return "Close";
         end if;
      end Persistence;

      ----------------------
      -- Port_Not_Default --
      ----------------------

      function Port_Not_Default (Port : in Positive) return String is
      begin
         if Port = 80 then
            return "";
         else
            declare
               Port_Image : constant String := Positive'Image (Port);
            begin
               return ':' & Port_Image (2 .. Port_Image'Last);
            end;
         end if;
      end Port_Not_Default;

      --------------------------------
      -- Send_Authentication_Header --
      --------------------------------

      procedure Send_Authentication_Header
        (Token : in     String;
         Data  : in out Authentication_Type)
      is
         User : constant String := To_String (Data.User);
         Pwd  : constant String := To_String (Data.Pwd);

      begin
         if User /= No_Data and then Pwd /= No_Data then

            if Data.Work_Mode = Basic then
               Send_Header
                 (Sock.all,
                  Token & ": Basic "
                    & AWS.Translator.Base64_Encode (User & ':' & Pwd));

            elsif Data.Work_Mode = Digest then

               declare
                  Nonce : constant String := To_String (Data.Nonce);
                  Realm : constant String := To_String (Data.Realm);
                  QOP   : constant String := To_String (Data.QOP);

                  function Get_URI return String;
                  --  Returns the real URI where the request is going to be
                  --  sent. It is either Open_Send_Common_Header.URI parameter
                  --  if it exists (without the HTTP parameters part), or URI
                  --  part of the Connection.Connect_URL field.

                  function QOP_Data return String;
                  --  Returns string with qop, cnonce and nc parameters
                  --  if qop parameter exists in the server auth request,
                  --  or empty string if not [RFC 2617 - 3.2.2].

                  Response : AWS.Digest.Digest_String;

                  -------------
                  -- Get_URI --
                  -------------

                  function Get_URI return String is
                     URI_Last : Natural;
                  begin
                     if URI = "" then
                        return URL.Path (Connection.Connect_URL)
                          & URL.File (Connection.Connect_URL);
                     else
                        URI_Last := Strings.Fixed.Index (URI, "?");

                        if URI_Last = 0 then
                           URI_Last := URI'Last;
                        else
                           URI_Last := URI_Last - 1;
                        end if;

                        return URI (URI'First .. URI_Last);
                     end if;
                  end Get_URI;

                  URI : constant String := Get_URI;

                  --------------
                  -- QOP_Data --
                  --------------

                  function QOP_Data return String is
                     CNonce : constant AWS.Digest.Nonce
                       := AWS.Digest.Create_Nonce;
                  begin
                     if QOP = No_Data then
                        Response := AWS.Digest.Create
                          (Username => User,
                           Realm    => Realm,
                           Password => Pwd,
                           Nonce    => Nonce,
                           Method   => Method,
                           URI      => URI);
                        return "";

                     else
                        Data.NC := Data.NC + 1;

                        declare
                           NC : constant String := Utils.Hex (Data.NC, 8);
                        begin
                           Response := AWS.Digest.Create
                             (Username => User,
                              Realm    => Realm,
                              Password => Pwd,
                              Nonce    => Nonce,
                              CNonce   => String (CNonce),
                              NC       => NC,
                              QOP      => QOP,
                              Method   => Method,
                              URI      => URI);

                           return "qop=""" & QOP
                             & """, cnonce=""" & String (CNonce)
                             & """, nc=" & NC
                             & ", ";
                        end;
                     end if;
                  end QOP_Data;

               begin
                  Send_Header
                    (Sock.all,
                     Token & ": Digest "
                       & QOP_Data
                       & "nonce=""" & Nonce
                       & """, username=""" & User
                       & """, realm=""" & Realm
                       & """, uri=""" & URI
                       & """, response=""" & Response
                       & """");
               end;

            end if;
         end if;
      end Send_Authentication_Header;

      Host_Address : constant String
        := AWS.URL.Host (Connection.Host_URL)
             & Port_Not_Default (AWS.URL.Port (Connection.Host_URL));

   begin
      --  Open connection if needed

      if not Connection.Opened then
         Connect (Connection);
      end if;

      Net.Set_Timeout (Connection.Socket.all, Connection.Write_Timeout);

      --  Header command

      if Connection.Proxy = No_Data then

         if URI = "" then
            Send_Header
              (Sock.all,
               Method & ' '
                 & AWS.URL.Pathname_And_Parameters (Connection.Host_URL, False)
                 & ' ' & HTTP_Version);
         else
            --  URI should already be encoded, but to help a bit Windows
            --  systems who tend to have spaces into URL we encode them here.

            declare
               E_URI : String := URI;
            begin
               for K in E_URI'Range loop
                  if E_URI (K) = ' ' then
                     E_URI (K) := '+';
                  end if;
               end loop;

               Send_Header
                 (Sock.all,
                  Method & ' ' & E_URI & ' ' & HTTP_Version);
            end;
         end if;

         Send_Header
           (Sock.all, Messages.Connection (Persistence));

      else
         if URI = "" then
            Send_Header (Sock.all,
                         Method & ' '
                           & To_String (Connection.Host)
                           & ' ' & HTTP_Version);
         else
            Send_Header
              (Sock.all,
               Method & ' '
                 & HTTP_Prefix (AWS.URL.Security (Connection.Host_URL))
                 & Host_Address & URI
                 & ' ' & HTTP_Version);
         end if;

         Send_Header
           (Sock.all, Messages.Proxy_Connection (Persistence));

      end if;

      --  Cookie

      if Connection.Cookie /= No_Data then
         Send_Header
           (Sock.all, Messages.Cookie (To_String (Connection.Cookie)));
      end if;

      Send_Header (Sock.all,
                   Messages.Host (Host_Address));

      Send_Header (Sock.all,
                   Messages.Accept_Type ("text/html, */*"));

      Send_Header (Sock.all,
                   Messages.Accept_Encoding_Token & ": deflate, gzip");

      Send_Header (Sock.all,
                   Messages.Accept_Language ("fr, ru, us"));

      Send_Header (Sock.all,
                   Messages.User_Agent (To_String (Connection.User_Agent)));

      --  User Authentification

      Send_Authentication_Header
        (Messages.Authorization_Token, Connection.Auth (WWW));

      --  Proxy Authentification

      Send_Authentication_Header
        (Messages.Proxy_Authorization_Token, Connection.Auth (Proxy));
   end Open_Send_Common_Header;

   ------------------
   -- Parse_Header --
   ------------------

   procedure Parse_Header
     (Connection : in out HTTP_Connection;
      Answer     :    out Response.Data;
      Keep_Alive :    out Boolean)
   is
      Sock : Net.Socket_Type'Class renames Connection.Socket.all;

      Status : Messages.Status_Code;

      Request_Auth_Mode : array (Authentication_Level) of Authentication_Mode
        := (others => Any);

      procedure Parse_Authenticate_Line
        (Level     : in Authentication_Level;
         Auth_Line : in String);
      --  Parses Authentication request line and fill Connection.Auth (Level)
      --  field with the information read on the line. Handle WWW and Proxy
      --  authentication.

      procedure Read_Status_Line;
      --  Read the status line

      procedure Set_Keep_Alive (Data : in String);
      --  Set the Parse_Header.Keep_Alive depending on data from the
      --  Proxy-Connection or Connection header line.

      function "+" (S : in String) return Unbounded_String
             renames To_Unbounded_String;

      -----------------------------
      -- Parse_Authenticate_Line --
      -----------------------------

      procedure Parse_Authenticate_Line
        (Level     : in Authentication_Level;
         Auth_Line : in     String)
      is
         use Ada.Characters.Handling;

         Basic_Token  : constant String := "BASIC";
         Digest_Token : constant String := "DIGEST";

         Auth         : Authentication_Type renames Connection.Auth (Level);

         Request_Mode : Authentication_Mode;

         Read_Params  : Boolean := False;
         --  Set it to true when the authentication mode is stronger
         --  then before.

         procedure Value
           (Item : in     String;
            Quit : in out Boolean);
         --  Routine receiving unnamed value during parsing of
         --  authentication line.

         procedure Named_Value
           (Name  : in     String;
            Value : in     String;
            Quit  : in out Boolean);
         --  Routine receiving name/value pairs during parsing of
         --  authentication line.

         -----------------
         -- Named_Value --
         -----------------

         procedure Named_Value
           (Name  : in     String;
            Value : in     String;
            Quit  : in out Boolean)
         is
            pragma Warnings (Off, Quit);
            U_Name : constant String := To_Upper (Name);
         begin
            if not Read_Params then
               return;
            end if;

            if U_Name = "REALM" then
               Auth.Realm := +Value;

            elsif U_Name = "NONCE" then
               Auth.Nonce := +Value;

            elsif U_Name = "QOP" then
               Auth.QOP   := +Value;

            elsif U_Name = "ALGORITHM" then
               if Value /= "MD5" then
                  Ada.Exceptions.Raise_Exception
                    (Constraint_Error'Identity,
                     "Only MD5 algorithm is supported.");
               end if;

            --  The parameter Stale is true when the Digest value is correct
            --  but the nonce value is too old or incorrect.
            --
            --  This mean that an interactive HTTP client should not ask
            --  name/password from the user, and try to use name/password from
            --  the previous successful authentication attempt.
            --  We do not need to check Stale authentication parameter
            --  for now, because our client is not interactive, so we are not
            --  going to ask user to input the name/password anyway. We could
            --  uncomment it later, when we would provide some interactive
            --  behavior to AWS.Client or interface to the interactive
            --  programs by callback to the AWS.Client.
            --
            --  elsif U_Name = "STALE" then
            --     null;
            end if;
         end Named_Value;

         -----------
         -- Value --
         -----------

         procedure Value
           (Item : in     String;
            Quit : in out Boolean)
         is
            pragma Warnings (Off, Quit);
            Mode_Image : constant String := To_Upper (Item);
         begin
            if Mode_Image = Digest_Token then
               Request_Mode := Digest;
            elsif Mode_Image = Basic_Token then
               Request_Mode := Basic;
            end if;

            Read_Params := Request_Mode > Request_Auth_Mode (Level);

            if Read_Params then
               Request_Auth_Mode (Level) := Request_Mode;
               Auth.Requested := True;
               Auth.Work_Mode := Request_Mode;
               Auth.NC        := 0;
            end if;
         end Value;

         -----------
         -- Parse --
         -----------

         procedure Parse is new Headers.Values.Parse (Value, Named_Value);

      begin
         Parse (Auth_Line);
      end Parse_Authenticate_Line;

      -----------------------
      --  Read_Status_Line --
      -----------------------

      procedure Read_Status_Line is

         function Get_Full_Line return String;
         --  Returns a full HTTP line (handle continuation line)
         --
         --  ??? This is non-standard and as been implemented because some
         --  Lotus Domino servers do send a Reason-Phrase with continuation
         --  line. This is clearly not valid see [RFC 2616 - 6.1].

         -------------------
         -- Get_Full_Line --
         -------------------

         function Get_Full_Line return String is
            Line   : constant String    := Net.Buffered.Get_Line (Sock);
            N_Char : constant Character := Net.Buffered.Peek_Char (Sock);
         begin
            if N_Char = ' ' or else N_Char = ASCII.HT then
               --  Next line is a continuation line [RFC 2616 - 2.2], but
               --  again this is non standard here, see comment above.
               return Line & Get_Full_Line;
            else
               return Line;
            end if;
         end Get_Full_Line;

         Line : constant String := Get_Full_Line;

      begin
         Debug_Message ("< ", Line);

         --  Checking the first line in the HTTP header.
         --  It must match Messages.HTTP_Token.

         if Messages.Match (Line, Messages.HTTP_Token) then
            Status := Messages.Status_Code'Value
                 ('S' & Line (Messages.HTTP_Token'Last + 5
                                .. Messages.HTTP_Token'Last + 7));
            Response.Set.Status_Code (Answer, Status);

            --  By default HTTP/1.0 connection is not keep-alive but
            --  HTTP/1.1 is keep-alive.

            Keep_Alive
              := Line (Messages.HTTP_Token'Last + 1
                         .. Messages.HTTP_Token'Last + 3) >= "1.1";
         else
            --  or else it is wrong answer from server
            Ada.Exceptions.Raise_Exception (Protocol_Error'Identity, Line);
         end if;
      end Read_Status_Line;

      --------------------
      -- Set_Keep_Alive --
      --------------------

      procedure Set_Keep_Alive (Data : in String) is
      begin
         if Messages.Match (Data, "Close") then
            Keep_Alive := False;

         elsif Messages.Match (Data, "Keep-Alive") then
            Keep_Alive := True;
         end if;
      end Set_Keep_Alive;

      use type Messages.Status_Code;

   begin
      for Level in Authentication_Level'Range loop
         Connection.Auth (Level).Requested := False;
      end loop;

      Read_Status_Line;
      Response.Set.Read_Header (Sock, Answer);

      declare
         use AWS.Response;

         Content_Encoding : constant String
           := Ada.Characters.Handling.To_Lower
                (Header (Answer, Messages.Content_Encoding_Token));

         procedure Decode_Init (Header : ZLib.Header_Type);

         procedure Decode_Init (Header : ZLib.Header_Type) is
            use type Utils.Stream_Element_Array_Access;
         begin
            ZLib.Inflate_Init (Connection.Decode_Filter, Header => Header);

            if Connection.Decode_Buffer = null then
               Connection.Decode_Buffer
                 := new Stream_Element_Array (1 .. 8096);
            end if;

            Connection.Decode_First := Connection.Decode_Buffer'Last + 1;
            Connection.Decode_Last  := Connection.Decode_Buffer'Last;
         end Decode_Init;

      begin
         if ZLib.Is_Open (Connection.Decode_Filter) then
            ZLib.Close (Connection.Decode_Filter, Ignore_Error => True);
         end if;

         if Content_Encoding = "gzip" then
            Decode_Init (ZLib.GZip);

         elsif Content_Encoding = "deflate" then
            Decode_Init (ZLib.Default);

         end if;
      end;

      --  ??? we should not expect 100 response message after the body sent.
      --  This code needs to be fixed.
      --  We should expect 100 status line only before sending the message
      --  body to server.
      --  And we should send Expect: header line in the header if we could
      --  deal with 100 status code.
      --  See [RFC 2616 - 8.2.3] use of the 100 (Continue) Status.

      if Status = Messages.S100 then
         Read_Status_Line;
         Response.Set.Read_Header (Sock, Answer);
      end if;

      Set_Keep_Alive (Response.Header (Answer, Messages.Connection_Token));

      Set_Keep_Alive (Response.Header
        (Answer, Messages.Proxy_Connection_Token));

      --  ??? We handle a single cookie on the client side. This must be
      --  fixed. Every cookie received should be stored and sent back to the
      --  server.

      declare
         Set_Cookie : constant String
           := Response.Header (Answer, Messages.Set_Cookie_Token);
      begin
         --  Set the new cookie, only if the server sent Set-Cookie
         --  header line.

         if Set_Cookie /= "" then
            Connection.Cookie := +Set_Cookie;
         end if;
      end;

      Parse_Authenticate_Line
        (WWW,
         Response.Header (Answer, Messages.WWW_Authenticate_Token));

      Parse_Authenticate_Line
        (Proxy,
         Response.Header (Answer, Messages.Proxy_Authenticate_Token));
   end Parse_Header;

   ----------
   -- Post --
   ----------

   function Post
     (URL        : in String;
      Data       : in String;
      User       : in String          := No_Data;
      Pwd        : in String          := No_Data;
      Proxy      : in String          := No_Data;
      Proxy_User : in String          := No_Data;
      Proxy_Pwd  : in String          := No_Data;
      Timeouts   : in Timeouts_Values := No_Timeout)
      return Response.Data
   is
      Connection : HTTP_Connection;
      Result     : Response.Data;

   begin
      Create (Connection,
              URL, User, Pwd, Proxy, Proxy_User, Proxy_Pwd,
              Persistent => False,
              Timeouts   => Timeouts);

      Post (Connection, Result, Data);
      Close (Connection);
      return Result;

   exception
      when others =>
         Close (Connection);
         raise;
   end Post;

   ----------
   -- Post --
   ----------

   function Post
     (URL        : in String;
      Data       : in Streams.Stream_Element_Array;
      User       : in String          := No_Data;
      Pwd        : in String          := No_Data;
      Proxy      : in String          := No_Data;
      Proxy_User : in String          := No_Data;
      Proxy_Pwd  : in String          := No_Data;
      Timeouts   : in Timeouts_Values := No_Timeout)
      return Response.Data
   is
      Connection : HTTP_Connection;
      Result     : Response.Data;

   begin
      Create (Connection,
              URL, User, Pwd, Proxy, Proxy_User, Proxy_Pwd,
              Persistent => False,
              Timeouts   => Timeouts);

      Post (Connection, Result, Data);
      Close (Connection);
      return Result;

   exception
      when others =>
         Close (Connection);
         raise;
   end Post;

   ----------
   -- Post --
   ----------

   procedure Post
     (Connection : in out HTTP_Connection;
      Result     :    out Response.Data;
      Data       : in     Streams.Stream_Element_Array;
      URI        : in     String := No_Data) is
   begin
      Internal_Post
        (Connection,
         Result,
         Data,
         URI,
         SOAPAction   => No_Data,
         Content_Type => MIME.Application_Octet_Stream);
   end Post;

   ----------
   -- Post --
   ----------

   procedure Post
     (Connection : in out HTTP_Connection;
      Result     :    out Response.Data;
      Data       : in     String;
      URI        : in     String := No_Data) is
   begin
      Internal_Post
        (Connection,
         Result,
         Translator.To_Stream_Element_Array (Data),
         URI,
         SOAPAction   => No_Data,
         Content_Type => MIME.Application_Form_Data);
   end Post;

   ---------
   -- Put --
   ---------

   function Put
     (URL        : in String;
      Data       : in String;
      User       : in String          := No_Data;
      Pwd        : in String          := No_Data;
      Proxy      : in String          := No_Data;
      Proxy_User : in String          := No_Data;
      Proxy_Pwd  : in String          := No_Data;
      Timeouts   : in Timeouts_Values := No_Timeout)
      return Response.Data
   is
      Connection : HTTP_Connection;
      Result     : Response.Data;

   begin
      Create (Connection,
              URL, User, Pwd, Proxy, Proxy_User, Proxy_Pwd,
              Persistent => False,
              Timeouts   => Timeouts);

      Put (Connection, Result, Data);
      Close (Connection);
      return Result;

   exception
      when others =>
         Close (Connection);
         raise;
   end Put;

   ---------
   -- Put --
   ---------

   procedure Put
     (Connection : in out HTTP_Connection;
      Result     :    out Response.Data;
      Data       : in     String;
      URI        : in     String          := No_Data)
   is
      Keep_Alive : Boolean;

      Try_Count  : Natural := Connection.Retry;

      Auth_Attempts : Auth_Attempts_Count := (others => 2);
      Auth_Is_Over  : Boolean;

   begin
      Retry : loop

         begin
            Open_Send_Common_Header (Connection, "PUT", URI);

            --  Send message Content_Length

            Send_Header
              (Connection.Socket.all, Messages.Content_Length (Data'Length));

            Net.Buffered.New_Line (Connection.Socket.all);

            --  Send message body

            Net.Buffered.Put_Line (Connection.Socket.all, Data);

            --  Get answer from server

            Parse_Header
              (Connection, Result, Keep_Alive);

            if not Keep_Alive then
               Disconnect (Connection);
            end if;

            Decrement_Authentication_Attempt
              (Connection, Auth_Attempts, Auth_Is_Over);

            if Auth_Is_Over then
               return;
            end if;

         exception
            when Net.Socket_Error =>

               Disconnect (Connection);

               if Try_Count = 0 then
                  Result := Response.Build
                    (MIME.Text_HTML, "Put Timeout", Messages.S408);
                  exit Retry;
               end if;

               Try_Count := Try_Count - 1;
         end;
      end loop Retry;
   end Put;

   ----------
   -- Read --
   ----------

   procedure Read
     (Connection : in out HTTP_Connection;
      Data       :    out Ada.Streams.Stream_Element_Array;
      Last       :    out Ada.Streams.Stream_Element_Offset)
   is
      use Ada.Streams;
      First : Stream_Element_Offset := Data'First;
   begin
      loop
         Read_Some (Connection, Data (First .. Data'Last), Last);
         exit when Last = Data'Last or else Last < First;
         First := Last + 1;
      end loop;
   end Read;

   ---------------
   -- Read_Some --
   ---------------

   procedure Read_Some
     (Connection : in out HTTP_Connection;
      Data       :    out Ada.Streams.Stream_Element_Array;
      Last       :    out Ada.Streams.Stream_Element_Offset)
   is
      procedure Read_Internal
        (Data       :    out Ada.Streams.Stream_Element_Array;
         Last       :    out Ada.Streams.Stream_Element_Offset);
      --  Read the encoded data as is from HTTP connection.

      -------------------
      -- Read_Internal --
      -------------------

      procedure Read_Internal
        (Data       :    out Ada.Streams.Stream_Element_Array;
         Last       :    out Ada.Streams.Stream_Element_Offset)
      is
         use Ada.Streams;
         Sock  : Net.Socket_Type'Class renames Connection.Socket.all;

         procedure Skip_Line;
         --  Skip a line in the sock stream

         procedure Read_Limited;
         --  Read Connection.Length characters if it can be contained in Data
         --  buffer otherwise just fill the remaining space in Data.

         ------------------
         -- Read_Limited --
         ------------------

         procedure Read_Limited is
            Limit : constant Stream_Element_Offset
              := Stream_Element_Offset'Min
                (Data'Last,
                 Data'First + Stream_Element_Offset (Connection.Length) - 1);
         begin
            Net.Buffered.Read (Sock, Data (Data'First .. Limit), Last);

            Connection.Length
              := Connection.Length - Integer (Last - Data'First + 1);
         end Read_Limited;

         ---------------
         -- Skip_Line --
         ---------------

         procedure Skip_Line is
            D : constant String := Net.Buffered.Get_Line (Sock);
            pragma Warnings (Off, D);
         begin
            null;
         end Skip_Line;

      begin
         case Connection.Transfer is
            when End_Response =>
               Last := Data'First - 1;

            when Until_Close  =>
               begin
                  Net.Buffered.Read (Sock, Data, Last);
               exception
                  when Net.Socket_Error =>
                     Connection.Transfer := End_Response;
                     Last := Data'First - 1;
               end;

            when Content_Length =>
               if Connection.Length = 0 then
                  Connection.Transfer := End_Response;
                  Last := Data'First - 1;
                  return;
               end if;

               Read_Limited;

            when Chunked =>
               if Connection.Length = 0 then
                  Connection.Length
                    := Utils.Hex_Value
                         (Strings.Fixed.Trim
                            (Net.Buffered.Get_Line (Sock), Strings.Both));

                  if Connection.Length = 0 then
                     Skip_Line;
                     Connection.Transfer := End_Response;

                     Last := Data'First - 1;
                     return;
                  end if;
               end if;

               Read_Limited;

               if Connection.Length = 0 then
                  Skip_Line;
               end if;

            when None =>
               raise Constraint_Error;
         end case;
      end Read_Internal;

   begin
      if ZLib.Is_Open (Connection.Decode_Filter) then
         declare
            procedure Read is new ZLib.Read
              (Read_Internal, Connection.Decode_Buffer.all,
               Connection.Decode_First, Connection.Decode_Last,
               Allow_Read_Some => True);
            --  Decompress gzip or deflate encoded data.
         begin
            Read (Connection.Decode_Filter, Data, Last);
         end;

      else
         Read_Internal (Data, Last);
      end if;
   end Read_Some;

   ----------------
   -- Read_Until --
   ----------------

   function Read_Until
     (Connection : in HTTP_Connection;
      Delimiter  : in String)
      return String
   is
      Result : Unbounded_String;
   begin
      Read_Until (Connection.Self.all, Delimiter, Result);
      return To_String (Result);
   end Read_Until;

   procedure Read_Until
     (Connection : in out HTTP_Connection;
      Delimiter  : in     String;
      Result     : in out Ada.Strings.Unbounded.Unbounded_String)
   is
      Sample_Idx : Natural := Delimiter'First;
      Buffer     : String (1 .. 1024);

   begin
      Net.Set_Timeout (Connection.Socket.all, Connection.Read_Timeout);

      Main : loop
         for I in Buffer'Range loop
            begin
               Buffer (I) := Net.Buffered.Get_Char (Connection.Socket.all);
            exception
               when Net.Socket_Error =>
                  Append (Result, Buffer (Buffer'First .. I - 1));
                  exit Main;
            end;

            if Buffer (I) = Delimiter (Sample_Idx) then

               if Sample_Idx = Delimiter'Last then
                  Append (Result, Buffer (Buffer'First .. I));
                  exit Main;
               else
                  Sample_Idx := Sample_Idx + 1;
               end if;

            else
               Sample_Idx := Delimiter'First;
            end if;
         end loop;

         Append (Result, Buffer);
      end loop Main;
   end Read_Until;

   -----------------
   -- Send_Header --
   -----------------

   procedure Send_Header
     (Sock : in Net.Socket_Type'Class;
      Data : in String) is
   begin
      Net.Buffered.Put_Line (Sock, Data);
      Debug_Message ("> ", Data);
   end Send_Header;

   ------------------------
   -- Set_Authentication --
   ------------------------

   procedure Set_Authentication
     (Auth :    out Authentication_Type;
      User : in     String;
      Pwd  : in     String;
      Mode : in     Authentication_Mode) is
   begin
      Auth.User      := To_Unbounded_String (User);
      Auth.Pwd       := To_Unbounded_String (Pwd);
      Auth.Init_Mode := Mode;

      --  The Digest authentication could not be send without
      --  server authentication request, because client have to have nonce
      --  value, so in the Digest and Any authentication modes we are not
      --  setting up Work_Mode to the exact value.
      --  But for Basic authentication we are sending just username/password,
      --  and do not need any information from server to do it.
      --  So if the client want to authenticate "Basic", we are setting up
      --  Work_Mode right now.

      if Mode = Basic then
         Auth.Work_Mode := Basic;
      end if;
   end Set_Authentication;

   ---------------
   -- Set_Debug --
   ---------------

   procedure Set_Debug (On : in Boolean) is
   begin
      Debug_On := On;
      AWS.Headers.Set.Debug (On);
   end Set_Debug;

   ------------------------------
   -- Set_Proxy_Authentication --
   ------------------------------

   procedure Set_Proxy_Authentication
     (Connection : in out HTTP_Connection;
      User       : in     String;
      Pwd        : in     String;
      Mode       : in     Authentication_Mode) is
   begin
      Set_Authentication
        (Auth => Connection.Auth (Proxy),
         User => User,
         Pwd  => Pwd,
         Mode => Mode);
   end Set_Proxy_Authentication;

   --------------------------
   -- Set_Streaming_Output --
   --------------------------

   procedure Set_Streaming_Output
     (Connection : in out HTTP_Connection;
      Value      : in     Boolean) is
   begin
      Connection.Streaming := Value;
   end Set_Streaming_Output;

   ----------------------------
   -- Set_WWW_Authentication --
   ----------------------------

   procedure Set_WWW_Authentication
     (Connection : in out HTTP_Connection;
      User       : in     String;
      Pwd        : in     String;
      Mode       : in     Authentication_Mode) is
   begin
      Set_Authentication
        (Auth => Connection.Auth (WWW),
         User => User,
         Pwd  => Pwd,
         Mode => Mode);
   end Set_WWW_Authentication;

   ---------------
   -- SOAP_Post --
   ---------------

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
      return Response.Data
   is
      Connection : HTTP_Connection;
      Result     : Response.Data;

   begin
      Create (Connection,
              URL, User, Pwd, Proxy, Proxy_User, Proxy_Pwd,
              Persistent => False,
              Timeouts   => Timeouts);

      SOAP_Post (Connection, Result, SOAPAction, Data => Data);
      Close (Connection);
      return Result;

   exception
      when others =>
         Close (Connection);
         raise;
   end SOAP_Post;

   procedure SOAP_Post
     (Connection : in     HTTP_Connection;
      Result     :    out Response.Data;
      SOAPAction : in     String;
      Data       : in     String;
      Streaming  : in     Boolean := False)
   is
      Save_Streaming : constant Boolean := Connection.Streaming;
   begin
      Connection.Self.Streaming := Streaming;

      Internal_Post
        (Connection   => Connection.Self.all,
         Result       => Result,
         Data         => AWS.Translator.To_Stream_Element_Array (Data),
         URI          => No_Data,
         SOAPAction   => SOAPAction,
         Content_Type => MIME.Text_XML);

      Connection.Self.Streaming := Save_Streaming;
   end SOAP_Post;

   ------------
   -- Upload --
   ------------

   procedure Upload
     (Connection : in out HTTP_Connection;
      Result     :    out Response.Data;
      Filename   : in     String;
      URI        : in     String := No_Data)
   is
      Pref_Suf  : constant String        := "--";
      Now       : constant Calendar.Time := Calendar.Clock;
      Boundary  : constant String
        := "AWS_File_Upload-" & GNAT.Calendar.Time_IO.Image (Now, "%s");

      CT        : constant String
        := Messages.Content_Type (MIME.Content_Type (Filename));

      CD        : constant String
        := Messages.Content_Disposition ("form-data", "filename", Filename);

      Try_Count : Natural := Connection.Retry;

      Auth_Attempts : Auth_Attempts_Count := (others => 2);
      Auth_Is_Over  : Boolean;

      function Content_Length return Integer;
      --  Returns the total message content length.

      procedure Send_File;
      --  Send file content to the server.

      --------------------
      -- Content_Length --
      --------------------

      function Content_Length return Integer is
      begin
         return 2 * Boundary'Length                -- 2 boundaries
           + 2                                     -- second one end with "--"
           + 10                                    -- 5 lines with CR+LF
           + CT'Length                             -- content length header
           + CD'Length                             -- content disposition head
           + Integer (OS_Lib.File_Size (Filename)) -- file size
           + 2;                                    -- CR+LF after file data
      end Content_Length;

      ---------------
      -- Send_File --
      ---------------

      procedure Send_File is
         Sock     : Net.Socket_Type'Class renames Connection.Socket.all;
         Buffer   : Streams.Stream_Element_Array (1 .. 4_096);
         Last     : Streams.Stream_Element_Offset;
         File     : Streams.Stream_IO.File_Type;
      begin
         --  Send multipart message start boundary

         Net.Buffered.Put_Line (Sock, Pref_Suf & Boundary);

         --  Send Content-Disposition header

         Net.Buffered.Put_Line (Sock, CD);

         --  Send Content-Type: header

         Net.Buffered.Put_Line (Sock, CT);

         Net.Buffered.New_Line (Sock);

         --  Send file content

         Streams.Stream_IO.Open (File, Streams.Stream_IO.In_File, Filename);

         while not Streams.Stream_IO.End_Of_File (File) loop
            Streams.Stream_IO.Read (File, Buffer, Last);
            Net.Buffered.Write (Sock, Buffer (1 .. Last));
         end loop;

         Streams.Stream_IO.Close (File);

         Net.Buffered.New_Line (Sock);

         --  Send multipart message end boundary

         Net.Buffered.Put_Line (Sock, Pref_Suf & Boundary & Pref_Suf);

      exception
         when Net.Socket_Error =>
            --  Properly close the file if needed
            if Streams.Stream_IO.Is_Open (File) then
               Streams.Stream_IO.Close (File);
            end if;
            raise;
      end Send_File;

   begin
      Retry : loop
         begin
            Open_Send_Common_Header (Connection, "POST", URI);

            declare
               Sock : Net.Socket_Type'Class renames Connection.Socket.all;
            begin
               --  Send message Content-Type (Multipart/form-data)

               Send_Header
                 (Sock,
                  Messages.Content_Type (MIME.Multipart_Form_Data, Boundary));

               --  Send message Content-Length

               Send_Header (Sock, Messages.Content_Length (Content_Length));

               Net.Buffered.New_Line (Sock);

               --  Send message body

               Send_File;
            end;

            --  Get answer from server

            Get_Response (Connection, Result, not Connection.Streaming);

            Decrement_Authentication_Attempt
              (Connection, Auth_Attempts, Auth_Is_Over);

            if Auth_Is_Over then
               return;
            end if;

         exception

            when Net.Socket_Error =>

               Disconnect (Connection);

               if Try_Count = 0 then
                  Result := Response.Build
                    (MIME.Text_HTML, "Upload Timeout", Messages.S408);
                  exit Retry;
               end if;

               Try_Count := Try_Count - 1;
         end;
      end loop Retry;
   end Upload;

   function Upload
     (URL        : in String;
      Filename   : in String;
      User       : in String          := No_Data;
      Pwd        : in String          := No_Data;
      Proxy      : in String          := No_Data;
      Proxy_User : in String          := No_Data;
      Proxy_Pwd  : in String          := No_Data;
      Timeouts   : in Timeouts_Values := No_Timeout)
      return Response.Data
   is
      Connection : HTTP_Connection;
      Result     : Response.Data;

   begin
      Create (Connection,
              URL, User, Pwd, Proxy, Proxy_User, Proxy_Pwd,
              Persistent => False,
              Timeouts   => Timeouts);

      Upload (Connection, Result, Filename);

      Close (Connection);
      return Result;

   exception
      when others =>
         Close (Connection);
         raise;
   end Upload;

   -----------
   -- Value --
   -----------

   function Value (V : in String) return Unbounded_String is
   begin
      if V = No_Data then
         return Null_Unbounded_String;
      else
         return To_Unbounded_String (V);
      end if;
   end Value;

end AWS.Client;
