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

with Ada.Characters.Handling;
with Ada.Exceptions;
with Ada.Streams.Stream_IO;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;

with AWS.Digest;
with AWS.Headers.Values;
with AWS.HTTP2.Connection;
with AWS.HTTP2.Frame.GoAway;
with AWS.HTTP2.Frame.List;
with AWS.HTTP2.Message;
with AWS.HTTP2.Stream;
with AWS.Messages;
with AWS.MIME;
with AWS.Net.Buffered;
with AWS.Net.SSL;
with AWS.Resources.Files;
with AWS.Response.Set;
with AWS.Server.Context;
with AWS.Server.HTTP_Utils;
with AWS.Translator;
with AWS.Utils;

package body AWS.Client.HTTP_Utils is

   function Image (Data_Range : Content_Range) return String;
   --  Returns the partial content range parameter to be passed to the Range
   --  header.

   function "+"
     (Left  : Real_Time.Time;
      Right : Real_Time.Time_Span) return Real_Time.Time;
   --  Returns Real_Time.Time_Last if Right is Real_Time.Time_Span_Last,
   --  otherwise returns Left + Right.

   procedure Send_Request_1
     (Connection : in out HTTP_Connection;
      Kind       : Method_Kind;
      Result     : out Response.Data;
      URI        : String;
      Data       : Stream_Element_Array := No_Data;
      Headers    : Header_List := Empty_Header_List);
   --  Send a simple GET request data For HTTP/1

   procedure Send_Request_2
     (Connection : in out HTTP_Connection;
      Kind       : Method_Kind;
      Result     : out Response.Data;
      URI        : String;
      Data       : Stream_Element_Array := No_Data;
      Headers    : Header_List := Empty_Header_List);
   --  Send a simple GET request data For HTTP/2

   procedure Internal_Post_Without_Attachment_1
     (Connection   : in out HTTP_Connection;
      Result       : out Response.Data;
      Data         : Stream_Element_Array;
      URI          : String;
      SOAPAction   : String;
      Content_Type : String;
      Headers      : Header_List := Empty_Header_List);
   --  Send a simple POST request data For HTTP/1

   procedure Internal_Post_Without_Attachment_2
     (Connection   : in out HTTP_Connection;
      Result       : out Response.Data;
      Data         : Stream_Element_Array;
      URI          : String;
      SOAPAction   : String;
      Content_Type : String;
      Headers      : Header_List := Empty_Header_List);
   --  Send a simple POST request data For HTTP/2

   procedure Internal_Post_With_Attachment_1
     (Connection   : in out HTTP_Connection;
      Result       : out Response.Data;
      Data         : Stream_Element_Array;
      URI          : String;
      SOAPAction   : String;
      Content_Type : String;
      Attachments  : Attachment_List;
      Headers      : Header_List := Empty_Header_List);
   --  Send a simple POST request data For HTTP/1

   procedure Internal_Post_With_Attachment_2
     (Connection   : in out HTTP_Connection;
      Result       : out Response.Data;
      Data         : Stream_Element_Array;
      URI          : String;
      SOAPAction   : String;
      Content_Type : String;
      Attachments  : Attachment_List;
      Headers      : Header_List := Empty_Header_List);
   --  Send a simple POST request data For HTTP/2

   procedure Send_H2_Connection_Preface (Connection : in out HTTP_Connection)
     with Pre => not Connection.H2_Preface_Sent;
   --  Send connection preface and get response from server

   procedure Next_Stream_Id (Connection : in out HTTP_Connection);
   --  Update client's stream-id to next value

   procedure Get_H2_Frame
     (Connection : in out HTTP_Connection;
      Ctx        : in out Server.Context.Object;
      Stream     : in out HTTP2.Stream.Object;
      Result     : out Response.Data);
   --  Process incoming HTTP/2 frame

   procedure Send_H2_Request
     (Connection : in out HTTP_Connection;
      Ctx        : in out Server.Context.Object;
      Stream     : in out HTTP2.Stream.Object;
      Request    : in out HTTP2.Message.Object);
   --  Send H2 request

   procedure Get_H2_Response
     (Connection : in out HTTP_Connection;
      Ctx        : in out Server.Context.Object;
      Stream     : in out HTTP2.Stream.Object;
      Result     : out Response.Data);
   --  Get H2 response

   procedure Handle_H2_Request
     (Connection    : in out HTTP_Connection;
      Result        : out Response.Data;
      Data          : Stream_Element_Array;
      Auth_Attempts : in out Auth_Attempts_Count;
      Auth_Is_Over  : out Boolean);
   --  Send request and get response for HTTP/2 protocol

   procedure Internal_Upload_1
     (Connection : in out HTTP_Connection;
      Result     : out Response.Data;
      Filename   : String;
      URI        : String;
      Headers    : Header_List := Empty_Header_List;
      Progress   : access procedure
                     (Total, Sent : Stream_Element_Offset) := null);
   --  Upload for HTTP/1

   procedure Internal_Upload_2
     (Connection : in out HTTP_Connection;
      Result     : out Response.Data;
      Filename   : String;
      URI        : String;
      Headers    : Header_List := Empty_Header_List;
      Progress   : access procedure
                     (Total, Sent : Stream_Element_Offset) := null);
   --  Upload for HTTP/2

   ---------
   -- "+" --
   ---------

   function "+"
     (Left  : Real_Time.Time;
      Right : Real_Time.Time_Span) return Real_Time.Time
   is
      use Real_Time;
   begin
      if Right = Time_Span_Last then
         return Time_Last;
      else
         return Real_Time."+" (Left, Right);
      end if;
   end "+";

   -------------
   -- Connect --
   -------------

   procedure Connect (Connection : in out HTTP_Connection) is
      use type Net.Socket_Access;
      use type Net.SSL.Session_Type;

      Connect_URL : AWS.URL.Object renames Connection.Connect_URL;
      Security    : constant Boolean := AWS.URL.Security (Connect_URL);
      Sock        : Net.Socket_Access;

      procedure Get_SSL_Session;
      --  Get SSL session data from connectio socket and store it into
      --  connection record.

      procedure Set_SSL_Session;
      --  Set SSL session data from connection record to connection socket

      ---------------------
      -- Get_SSL_Session --
      ---------------------

      procedure Get_SSL_Session is
      begin
         if Connection.SSL_Session /= Net.SSL.Null_Session then
            Net.SSL.Free (Connection.SSL_Session);
         end if;

         Connection.SSL_Session :=
           Net.SSL.Socket_Type (Connection.Socket.all).Session_Data;
      end Get_SSL_Session;

      ---------------------
      -- Set_SSL_Session --
      ---------------------

      procedure Set_SSL_Session is
      begin
         if Connection.SSL_Session /= Net.SSL.Null_Session then
            --  Try to reuse SSL session to speedup handshake

            Net.SSL.Socket_Type (Connection.Socket.all).Set_Session_Data
              (Connection.SSL_Session);
         end if;
      end Set_SSL_Session;

   begin
      pragma Assert (not Connection.Opened);
      --  This should never be called with an open connection

      --  Keep-alive reconnection will be with old socket. We cannot reuse it,
      --  and have to free it.

      if Connection.Socket /= null then
         Net.Free (Connection.Socket);
      end if;

      Sock := Net.Socket (Security);
      Connection.Socket := Sock;

      Connection.H2_Preface_Sent := False;

      --  Set default HTTP2 connection settings

      Connection.H2_Settings := HTTP_Utils.Get_Settings (Connection.Config);
      HTTP2.Connection.Set (Connection.H2_Connection, Connection.H2_Settings);
      Connection.Enc_Table.Clear;
      Connection.Dec_Table.Clear;

      if Security then
         --  This is a secure connection, set the SSL config for this socket

         Net.SSL.Socket_Type (Sock.all).Set_Config (Connection.SSL_Config);

         Set_SSL_Session;
      end if;

      Sock.Set_Timeout (Connection.Timeouts.Connect);

      Sock.Connect (AWS.URL.Host (Connect_URL), AWS.URL.Port (Connect_URL));

      if Security then
         --  Save SSL session to be able to reuse it later

         Get_SSL_Session;
      end if;

      Connection.Opened := True;

      if AWS.URL.Security (Connection.Host_URL)
        and then Connection.Proxy /= Client.No_Data
      then
         --  We want to connect to the host using HTTPS, this can only be
         --  done by opening a tunnel through the proxy.
         --
         --  CONNECT <host> HTTP/1.1
         --  Host: <host>
         --  [Proxy-Authorization: xxxx]
         --  <other headers>
         --  <empty line>

         Sock.Set_Timeout (Connection.Timeouts.Send);

         declare
            use AWS.URL;
            Host_Address : constant String :=
                             Host (Connection.Host_URL, IPv6_Brackets => True)
                             & ':' & Port (Connection.Host_URL);
         begin
            Set_Header
              (Connection.F_Headers,
               Messages.Connect_Token,
               Host_Address & ' ' & AWS.HTTP_Version);
            Set_Header
              (Connection.F_Headers, Messages.Host_Token, Host_Address);
         end;

         --  Proxy Authentication

         Set_Authentication_Header
           (Connection,
            Messages.Proxy_Authorization_Token,
            Connection.Auth (Proxy),
            URI    => "/",
            Method => Messages.Connect_Token);

         if Connection.User_Agent /= Null_Unbounded_String then
            Set_Header
              (Connection.F_Headers,
               Messages.User_Agent_Token,
               To_String (Connection.User_Agent));
         end if;

         --  Send CONNECT command with headers to proxy

         Headers.Send_Header
           (Sock.all, Connection.F_Headers, End_Block => True);

         --  Wait for reply from the proxy, and check status

         Sock.Set_Timeout (Connection.Timeouts.Receive);

         declare
            use type Messages.Status_Code;
            Line   : constant String := Net.Buffered.Get_Line (Sock.all);
            Status : Messages.Status_Code;
         begin
            Debug_Message ("< ", Line);

            Status := Messages.Status_Code'Value
              ('S' & Line (Messages.HTTP_Token'Length + 5
                           .. Messages.HTTP_Token'Length + 7));

            if Status >= Messages.S400 then
               raise Connection_Error
                 with "Can't connect to proxy, status "
                   & Messages.Image (Status);
            end if;
         end;

         --  Ignore all remainings lines

         loop
            declare
               Line : constant String := Net.Buffered.Get_Line (Sock.all);
            begin
               Debug_Message ("< ", Line);
               exit when Line = "";
            end;
         end loop;

         --  Now the tunnel is open, we need to create an SSL connection
         --  around this tunnel.

         declare
            SS : Net.SSL.Socket_Type :=
                   Net.SSL.Secure_Client
                     (Sock.all, Connection.SSL_Config,
                      Host => URL.Host (Connection.Host_URL));
         begin
            Net.Free (Sock);
            Connection.Socket := new Net.SSL.Socket_Type'(SS);

            Set_SSL_Session;

            --  Do explicit handshake to be able to get server certificate
            --  and SSL session after.

            SS.Do_Handshake;

            Get_SSL_Session;
         end;
      end if;

   exception
      when E : Net.Socket_Error =>
         raise Connection_Error with Exceptions.Exception_Message (E);
   end Connect;

   --------------------------------------
   -- Decrement_Authentication_Attempt --
   --------------------------------------

   procedure Decrement_Authentication_Attempt
     (Connection : in out HTTP_Connection;
      Counter    : in out Auth_Attempts_Count;
      Over       : out Boolean)
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
         if Connection.HTTP_Version = HTTPv2
           and then Connection.Socket /= null
         then
            --  Properly close the connection

            declare
               GoAway : constant HTTP2.Frame.GoAway.Object :=
                          HTTP2.Frame.GoAway.Create
                            (Connection.H2_Stream_Id, HTTP2.C_No_Error);
            begin
               GoAway.Send (Connection.Socket.all);
            exception
               when Net.Socket_Error =>
                  --  Be ready that socket already closed
                  null;
            end;
         end if;

         Connection.Opened             := False;
         Connection.Disconnect_Counter := Connection.Disconnect_Counter + 1;

         if Connection.Socket /= null then
            Connection.Socket.Shutdown;
         end if;
      end if;

      if Connection.Socket /= null then
         Net.Free (Connection.Socket);
      end if;
   end Disconnect;

   ------------------
   -- Get_H2_Frame --
   ------------------

   procedure Get_H2_Frame
     (Connection : in out HTTP_Connection;
      Ctx        : in out Server.Context.Object;
      Stream     : in out HTTP2.Stream.Object;
      Result     : out Response.Data)
   is
      use AWS.HTTP2;
      Frame   : constant HTTP2.Frame.Object'Class :=
                  HTTP2.Frame.Read (Connection.Socket.all, Ctx.Settings.all);
      Answers : HTTP2.Frame.List.Object;
      Error   : Error_Codes := C_No_Error;
      Add_FC  : Integer     := 0;
   begin
      Response.Set.Mode (Result, Response.No_Data);

      if Frame.Stream_Id = 0 then
         case Frame.Kind is
            when HTTP2.Frame.K_GoAway =>
               declare
                  G : constant HTTP2.Frame.GoAway.Object :=
                        HTTP2.Frame.GoAway.Object (Frame);
               begin
                  if G.Error /= C_No_Error then
                     --  We map all HTTP/2 GoAway to HTTP/1 code if present

                     if G.Has_Data and then G.Has_Code_Message then
                        Result := Response.Build
                          (Status_Code  => G.Code,
                           Content_Type => "text/plain",
                           Message_Body => G.Message);
                     else
                        Result := Response.Build
                          (Status_Code  => Messages.S500,
                           Content_Type => "text/plain",
                           Message_Body =>
                             (if G.Has_Data then G.Data else ""));
                     end if;
                  end if;
               end;

            when others =>
               Ctx.Settings.Handle_Control_Frame
                 (Frame, Answers, Add_FC, Error);

               if Add_FC /= 0
                 and then HTTP2.Connection.Flow_Control_Window_Valid
                            (Stream.Flow_Control_Window, Add_FC)
               then
                  Stream.Update_Flow_Control_Window (Add_FC);
               end if;

               for A of Answers loop
                  Stream.Send_Frame (A);
               end loop;
         end case;

      else
         if not Stream.Is_Defined then
            Stream := HTTP2.Stream.Create
              (Connection.Socket, Frame.Stream_Id,
               Ctx.Settings.Initial_Window_Size);

            if Connection.H2_Stream_Id < Frame.Stream_Id then
               Connection.H2_Stream_Id := Frame.Stream_Id;
            end if;
         end if;

         pragma Assert
           (Frame.Stream_Id = Stream.Identifier,
            Frame.Stream_Id'Img & Stream.Identifier'Img);

         Stream.Received_Frame (Ctx, Frame, Error);
      end if;
   end Get_H2_Frame;

   ---------------------
   -- Get_H2_Response --
   ---------------------

   procedure Get_H2_Response
     (Connection : in out HTTP_Connection;
      Ctx        : in out Server.Context.Object;
      Stream     : in out HTTP2.Stream.Object;
      Result     : out Response.Data)
   is
      Keep_Alive : Boolean;
   begin
      Connection.Socket.Set_Timeout (Connection.Timeouts.Receive);

      Response.Set.Clear (Result);

      Stream := HTTP2.Stream.Undefined;

      --  Read response frames

      while not Stream.Is_Message_Ready loop
         Get_H2_Frame (Connection, Ctx, Stream, Result);
         exit when not Response.Is_Empty (Result);
      end loop;

      if Stream.Is_Message_Ready then
         --  Set headers into Answer

         Response.Set.Headers (Result, Stream.Headers);

         --  Then parse headers

         Read_Parse_Header (Connection, Result, Keep_Alive);

         Stream.Append_Body (Result);

         --  Check encoding

         declare
            TE     : constant String  :=
                       Response.Header
                          (Result, Messages.Transfer_Encoding_Token);
            CT_Len : constant Response.Content_Length_Type :=
                       Response.Content_Length (Result);
         begin
            if not Messages.With_Body (Response.Status_Code (Result)) then
               --  RFC-2616 4.4
               --  ...
               --  Any response message which "MUST NOT" include a
               --  message-body (such as the 1xx, 204, and 304
               --  responses and any response to a HEAD request) is
               --  always terminated by the first empty line after the
               --  header fields, regardless of the entity-header
               --  fields present in the message.

               Connection.Transfer := Content_Length;
               Connection.Length   := 0;

            elsif TE = "chunked" then
               raise Protocol_Error
                 with "chunked encoding is not part of HTTP/2";

            elsif CT_Len = Response.Undefined_Length then
               Connection.Transfer := Until_Close;

            else
               Connection.Transfer := Content_Length;
               Connection.Length   := CT_Len;
            end if;
         end;

         if not Connection.Persistent then
            Disconnect (Connection);
         end if;
      end if;
   end Get_H2_Response;

   ------------------
   -- Get_Response --
   ------------------

   procedure Get_Response
     (Connection : in out HTTP_Connection;
      Result     : out Response.Data;
      Get_Body   : Boolean         := True)
   is
      procedure Disconnect;
      --  close connection socket

      Sock       : Net.Socket_Type'Class renames Connection.Socket.all;
      Keep_Alive : Boolean;

      ----------------
      -- Disconnect --
      ----------------

      procedure Disconnect is
      begin
         if not Keep_Alive and then not Connection.Streaming then
            Disconnect (Connection);
         end if;
      end Disconnect;

   begin
      Sock.Set_Timeout (Connection.Timeouts.Receive);

      --  Clear the data in the response

      Response.Set.Clear (Result);

      Read_Parse_Header (Connection, Result, Keep_Alive);

      declare
         TE     : constant String  :=
                    Response.Header (Result, Messages.Transfer_Encoding_Token);
         CT_Len : constant Response.Content_Length_Type :=
                    Response.Content_Length (Result);
      begin
         if not Messages.With_Body (Response.Status_Code (Result)) then
            --  RFC-2616 4.4
            --  ...
            --  Any response message which "MUST NOT" include a message-body
            --  (such as the 1xx, 204, and 304 responses and any response to a
            --  HEAD request) is always terminated by the first empty line
            --  after the header fields, regardless of the entity-header fields
            --  present in the message.

            Connection.Transfer := Content_Length;
            Connection.Length   := 0;

         elsif TE = "chunked" then

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

      --  If we get an Unauthorized response we want to get the body. This is
      --  needed as in Digest mode the body will gets read by the next request
      --  and will raise a protocol error.

      if Get_Body then
         Read_Body (Connection, Result, Store => True);
         Connection.Transfer := None;
      end if;

      Disconnect;
   end Get_Response;

   ------------------
   -- Get_Settings --
   ------------------

   function Get_Settings
     (Config : AWS.Config.Object) return HTTP2.Frame.Settings.Set
   is
      use all type HTTP2.Frame.Settings.Settings_Kind;
      subtype Byte_4 is HTTP2.Byte_4;
   begin
      return HTTP2.Frame.Settings.Set'
        (1 => (HEADER_TABLE_SIZE,
               Byte_4 (Config.HTTP2_Header_Table_Size)),
         2 => (ENABLE_PUSH,
               0),
         3 => (MAX_CONCURRENT_STREAMS,
               Byte_4 (Config.HTTP2_Max_Concurrent_Streams)),
         4 => (INITIAL_WINDOW_SIZE,
               Byte_4 (Config.HTTP2_Initial_Window_Size)),
         5 => (MAX_FRAME_SIZE,
               Byte_4 (Config.HTTP2_Max_Frame_Size)),
         6 => (MAX_HEADER_LIST_SIZE,
               Byte_4 (Config.HTTP2_Max_Header_List_Size)));
   end Get_Settings;

   -----------------------
   -- Handle_H2_Request --
   -----------------------

   procedure Handle_H2_Request
     (Connection    : in out HTTP_Connection;
      Result        : out Response.Data;
      Data          : Stream_Element_Array;
      Auth_Attempts : in out Auth_Attempts_Count;
      Auth_Is_Over  : out Boolean)
   is
      Request       : HTTP2.Message.Object;
      Stream        : HTTP2.Stream.Object;
      Ctx           : Server.Context.Object (null,
                                             1,
                                             Connection.Enc_Table'Access,
                                             Connection.Dec_Table'Access,
                                             Connection.H2_Connection'Access);
   begin
      --  Create the request HTTP/2 message out of Status.Data

      if Data'Length > 0 then
         Set_Header
           (Connection.F_Headers,
            Messages.Content_Length_Token,
            Utils.Image (Stream_Element_Offset'(Data'Length)));
      end if;

      if not Connection.H2_Preface_Sent then
         --  Update H_Connection with server settings
         Send_H2_Connection_Preface (Connection);
      end if;

      --  Create frames and send them

      Stream := HTTP2.Stream.Create
        (Connection.Socket,
         Connection.H2_Stream_Id, Ctx.Settings.Initial_Window_Size);

      Next_Stream_Id (Connection);

      Request := HTTP2.Message.Create
        (Connection.F_Headers, Data, Stream.Identifier);

      Send_H2_Request (Connection, Ctx, Stream, Request);

      --  Get response

      Get_H2_Response (Connection, Ctx, Stream, Result);

      Decrement_Authentication_Attempt
        (Connection, Auth_Attempts, Auth_Is_Over);
   end Handle_H2_Request;

   -----------
   -- Image --
   -----------

   function Image (Data_Range : Content_Range) return String is
      Result : Unbounded_String;
   begin
      Append (Result, "bytes=");

      if Data_Range.First /= Undefined then
         Append
           (Result, Utils.Image (Stream_Element_Offset (Data_Range.First)));
      end if;

      Append (Result, "-");

      if Data_Range.Last /= Undefined then
         Append
           (Result, Utils.Image (Stream_Element_Offset (Data_Range.Last)));
      end if;

      return To_String (Result);
   end Image;

   -------------------
   -- Internal_Post --
   -------------------

   procedure Internal_Post
     (Connection   : in out HTTP_Connection;
      Result       : out Response.Data;
      Data         : Stream_Element_Array;
      URI          : String;
      SOAPAction   : String;
      Content_Type : String;
      Attachments  : Attachment_List;
      Headers      : Header_List          := Empty_Header_List)
   is
      use type AWS.Attachments.List;
   begin
      if Attachments = AWS.Attachments.Empty_List then
         Internal_Post_Without_Attachment
           (Connection   => Connection,
            Result       => Result,
            Data         => Data,
            URI          => URI,
            SOAPAction   => SOAPAction,
            Content_Type => Content_Type,
            Headers      => Headers);

      else
         Internal_Post_With_Attachment
           (Connection   => Connection,
            Result       => Result,
            Data         => Data,
            URI          => URI,
            SOAPAction   => SOAPAction,
            Content_Type => Content_Type,
            Attachments  => Attachments,
            Headers      => Headers);
      end if;
   end Internal_Post;

   -----------------------------------
   -- Internal_Post_With_Attachment --
   -----------------------------------

   procedure Internal_Post_With_Attachment
     (Connection   : in out HTTP_Connection;
      Result       : out Response.Data;
      Data         : Stream_Element_Array;
      URI          : String;
      SOAPAction   : String;
      Content_Type : String;
      Attachments  : Attachment_List;
      Headers      : Header_List          := Empty_Header_List) is
   begin
      if Connection.HTTP_Version = HTTPv1 then
         Internal_Post_With_Attachment_1
           (Connection, Result, Data, URI, SOAPAction,
            Content_Type, Attachments, Headers);
      else
         Internal_Post_With_Attachment_2
           (Connection, Result, Data, URI, SOAPAction,
            Content_Type, Attachments, Headers);
      end if;
   end Internal_Post_With_Attachment;

   -------------------------------------
   -- Internal_Post_With_Attachment_1 --
   -------------------------------------

   procedure Internal_Post_With_Attachment_1
     (Connection   : in out HTTP_Connection;
      Result       : out Response.Data;
      Data         : Stream_Element_Array;
      URI          : String;
      SOAPAction   : String;
      Content_Type : String;
      Attachments  : Attachment_List;
      Headers      : Header_List          := Empty_Header_List)
   is
      use Real_Time;
      Stamp    : constant Time := Clock;
      Pref_Suf : constant String := "--";
      Boundary : constant String :=
                   "AWS_Attachment-" & Utils.Random_String (8);

      Root_Content_Id  : constant String := "<rootpart>";
      Root_Part_Header : AWS.Headers.List;

      Try_Count        : Natural := Connection.Retry;

      Auth_Attempts    : Auth_Attempts_Count := (others => 2);
      Auth_Is_Over     : Boolean;

      procedure Build_Root_Part_Header;
      --  Builds the rootpart header and calculates its size

      function Content_Length return Stream_Element_Offset;
      --  Returns the total message content length

      ----------------------------
      -- Build_Root_Part_Header --
      ----------------------------

      procedure Build_Root_Part_Header is
      begin
         Root_Part_Header.Add
           (Name  => AWS.Messages.Content_Type_Token,
            Value => Content_Type);

         Root_Part_Header.Add
           (Name  => AWS.Messages.Content_Id_Token,
            Value => Root_Content_Id);
      end Build_Root_Part_Header;

      --------------------
      -- Content_Length --
      --------------------

      function Content_Length return Stream_Element_Offset is
      begin
         return 2
           + Boundary'Length + 2    -- Root part boundary + CR+LF
           + Stream_Element_Offset (AWS.Headers.Length (Root_Part_Header))
           + Data'Length            -- Root part data length
           + Stream_Element_Offset
               (AWS.Attachments.Length (Attachments, Boundary));
      end Content_Length;

   begin
      Connection.F_Headers.Reset;

      Build_Root_Part_Header;

      Retry : loop
         begin
            Open_Set_Common_Header
              (Connection, Messages.Post_Token, URI, Headers);

            declare
               Sock : Net.Socket_Type'Class renames Connection.Socket.all;
            begin
               --  Send message Content-Type (multipart/related)

               if Content_Type = "" then
                  Set_Header
                    (Connection.F_Headers,
                     Messages.Content_Type_Token,
                     MIME.Multipart_Related
                     & "; type=" & Content_Type
                     & "; start=""" & Root_Content_Id & '"'
                     & "; boundary=""" & Boundary & '"');
               else
                  Set_Header
                    (Connection.F_Headers,
                     Messages.Content_Type_Token,
                     MIME.Multipart_Form_Data
                     & "; boundary=""" & Boundary & '"');
               end if;

               if SOAPAction /= Client.No_Data then
                  --  SOAP header

                  Set_Header
                    (Connection.F_Headers,
                     Messages.SOAPAction_Token, SOAPAction);
               end if;

               --  Send message Content-Length

               Set_Header
                 (Connection.F_Headers,
                  Messages.Content_Length_Token, Utils.Image (Content_Length));

               AWS.Headers.Send_Header
                 (Sock, Connection.F_Headers, End_Block => True);

               --  Send multipart message start boundary

               Net.Buffered.Put_Line (Sock, Pref_Suf & Boundary);

               --  Send root part header

               AWS.Headers.Send_Header
                 (Sock, Root_Part_Header, End_Block => True);

               --  Send root part data

               if Data'Length /= 0 then
                  Net.Buffered.Write (Sock, Data);
                  Net.Buffered.New_Line (Sock);
               end if;

               --  Send the attachments

               AWS.Attachments.Send (Sock, Attachments, Boundary);

               Net.Buffered.Put_Line
                 (Sock, Pref_Suf & Boundary & Pref_Suf);
            end;

            --  Get answer from server

            Get_Response
              (Connection, Result, Get_Body => not Connection.Streaming);

            Decrement_Authentication_Attempt
              (Connection, Auth_Attempts, Auth_Is_Over);

            if Auth_Is_Over then
               return;

            elsif Connection.Streaming then
               Read_Body (Connection, Result, Store => False);
            end if;

         exception
            when E : Net.Socket_Error | Connection_Error =>
               Error_Processing
                 (Connection, Try_Count, Result, "UPLOAD", E, Stamp);

               exit Retry when not Response.Is_Empty (Result);
         end;
      end loop Retry;
   end Internal_Post_With_Attachment_1;

   -------------------------------------
   -- Internal_Post_With_Attachment_2 --
   -------------------------------------

   procedure Internal_Post_With_Attachment_2
     (Connection   : in out HTTP_Connection;
      Result       : out Response.Data;
      Data         : Stream_Element_Array;
      URI          : String;
      SOAPAction   : String;
      Content_Type : String;
      Attachments  : Attachment_List;
      Headers      : Header_List          := Empty_Header_List)
   is
      use Real_Time;

      Stamp     : constant Time := Clock;

      Pref_Suf  : constant String := "--";
      Boundary  : constant String :=
                    "AWS_Attachment-" & Utils.Random_String (8);

      Root_Content_Id  : constant String := "<rootpart>";
      Root_Part_Header : AWS.Headers.List;

      Request       : HTTP2.Message.Object;
      Try_Count     : Natural := Connection.Retry;
      Auth_Attempts : Auth_Attempts_Count := (others => 2);
      Auth_Is_Over  : Boolean;
      Stream        : HTTP2.Stream.Object;
      Ctx           : Server.Context.Object (null,
                                             1,
                                             Connection.Enc_Table'Access,
                                             Connection.Dec_Table'Access,
                                             Connection.H2_Connection'Access);
      procedure Build_Root_Part_Header;
      --  Builds the rootpart header and calculates its size

      ----------------------------
      -- Build_Root_Part_Header --
      ----------------------------

      procedure Build_Root_Part_Header is
      begin
         Root_Part_Header.Add
           (Name  => Messages.Content_Type_Token,
            Value => Content_Type);

         Root_Part_Header.Add
           (Name  => Messages.Content_Id_Token,
            Value => Root_Content_Id);
      end Build_Root_Part_Header;

   begin
      Connection.F_Headers.Reset;

      Build_Root_Part_Header;

      Retry : loop
         begin
            Set_Common_Post
              (Connection, Data, URI, SOAPAction, "", Headers);

            if Content_Type = "" then
               Set_Header
                 (Connection.F_Headers,
                  Messages.Content_Type_Token,
                  MIME.Multipart_Related
                  & "; type=" & Content_Type
                  & "; start=""" & Root_Content_Id & '"'
                  & "; boundary=""" & Boundary & '"');
            else
               Set_Header
                 (Connection.F_Headers,
                  Messages.Content_Type_Token,
                  MIME.Multipart_Form_Data
                  & "; boundary=""" & Boundary & '"');
            end if;

            if not Connection.H2_Preface_Sent then
               --  Update H_Connection with server settings
               Send_H2_Connection_Preface (Connection);
            end if;

            --  Create frames and send them

            Stream := HTTP2.Stream.Create
              (Connection.Socket,
               Connection.H2_Stream_Id, Ctx.Settings.Initial_Window_Size);

            Next_Stream_Id (Connection);

            Request := HTTP2.Message.Create
              (Connection.F_Headers,
               Stream_Element_Array'(1 .. 0 => <>),
               Stream.Identifier);

            --  Append data & attachments

            Request.Append_Body (Pref_Suf & Boundary & CRLF);

            declare
               procedure Write (Data : String);
               procedure Write (Data : Stream_Element_Array);

               -----------
               -- Write --
               -----------

               procedure Write (Data : String) is
               begin
                  Request.Append_Body (Data);
               end Write;

               procedure Write (Data : Stream_Element_Array) is
               begin
                  Request.Append_Body (Data);
               end Write;

               procedure Append_Attachments is
                 new AWS.Attachments.Get_Content (Write);

               procedure Append_Header is
                 new AWS.Headers.Get_Content (Write);

            begin
               --  Root part header

               Append_Header (Root_Part_Header, End_Block => True);

               --  Data

               if Data'Length /= 0 then
                  Write (Data);
                  Write (CRLF);
               end if;

               --  Attachments

               Append_Attachments (Attachments, Boundary);
            end;

            Request.Append_Body (Pref_Suf & Boundary & Pref_Suf & CRLF);

            Send_H2_Request (Connection, Ctx, Stream, Request);

            --  Get response

            Get_H2_Response (Connection, Ctx, Stream, Result);

            Decrement_Authentication_Attempt
              (Connection, Auth_Attempts, Auth_Is_Over);

            exit Retry when Auth_Is_Over;
         exception
            when E : Net.Socket_Error
                   | Connection_Error
                   | HTTP2.Protocol_Error
              =>
               Error_Processing
                 (Connection, Try_Count, Result, "UPLOAD", E, Stamp);

               exit Retry when not Response.Is_Empty (Result);
         end;
      end loop Retry;
   end Internal_Post_With_Attachment_2;

   --------------------------------------
   -- Internal_Post_Without_Attachment --
   --------------------------------------

   procedure Internal_Post_Without_Attachment
     (Connection   : in out HTTP_Connection;
      Result       : out Response.Data;
      Data         : Stream_Element_Array;
      URI          : String;
      SOAPAction   : String;
      Content_Type : String;
      Headers      : Header_List := Empty_Header_List) is
   begin
      if Connection.HTTP_Version = HTTPv1 then
         Internal_Post_Without_Attachment_1
           (Connection, Result, Data, URI, SOAPAction, Content_Type, Headers);
      else
         Internal_Post_Without_Attachment_2
           (Connection, Result, Data, URI, SOAPAction, Content_Type, Headers);
      end if;
   end Internal_Post_Without_Attachment;

   ----------------------------------------
   -- Internal_Post_Without_Attachment_1 --
   ----------------------------------------

   procedure Internal_Post_Without_Attachment_1
     (Connection   : in out HTTP_Connection;
      Result       : out Response.Data;
      Data         : Stream_Element_Array;
      URI          : String;
      SOAPAction   : String;
      Content_Type : String;
      Headers      : Header_List := Empty_Header_List)
   is
      use Real_Time;
      Stamp         : constant Time := Clock;
      Try_Count     : Natural := Connection.Retry;

      Auth_Attempts : Auth_Attempts_Count := (others => 2);
      Auth_Is_Over  : Boolean;

   begin
      Connection.F_Headers.Reset;

      Retry : loop
         begin
            --  Post Data with headers

            Set_Common_Post
              (Connection, Data, URI, SOAPAction, Content_Type, Headers);

            AWS.Headers.Send_Header
              (Connection.Socket.all, Connection.F_Headers, End_Block => True);

            --  Send message body

            Net.Buffered.Write (Connection.Socket.all, Data);

            --  Get answer from server

            Get_Response
              (Connection, Result, Get_Body => not Connection.Streaming);

            Decrement_Authentication_Attempt
              (Connection, Auth_Attempts, Auth_Is_Over);

            if Auth_Is_Over then
               return;

            elsif Connection.Streaming then
               Read_Body (Connection, Result, Store => False);
            end if;

         exception
            when E : Net.Socket_Error | Connection_Error =>
               Error_Processing
                 (Connection, Try_Count, Result, "POST", E, Stamp);

               exit Retry when not Response.Is_Empty (Result);
         end;
      end loop Retry;
   end Internal_Post_Without_Attachment_1;

   ----------------------------------------
   -- Internal_Post_Without_Attachment_2 --
   ----------------------------------------

   procedure Internal_Post_Without_Attachment_2
     (Connection   : in out HTTP_Connection;
      Result       : out Response.Data;
      Data         : Stream_Element_Array;
      URI          : String;
      SOAPAction   : String;
      Content_Type : String;
      Headers      : Header_List := Empty_Header_List)
   is
      use Ada.Real_Time;

      Stamp         : constant Time := Clock;
      Try_Count     : Natural := Connection.Retry;
      Auth_Attempts : Auth_Attempts_Count := (others => 2);
      Auth_Is_Over  : Boolean;
   begin
      Connection.F_Headers.Reset;

      Retry : loop
         begin
            Set_Common_Post
              (Connection, Data, URI, SOAPAction, Content_Type, Headers);

            Handle_H2_Request
              (Connection, Result, Data, Auth_Attempts, Auth_Is_Over);

            exit Retry when Auth_Is_Over;
         exception
            when E : Net.Socket_Error
                   | Connection_Error
                   | HTTP2.Protocol_Error
              =>
               Error_Processing
                 (Connection, Try_Count, Result, "POST", E, Stamp);

               exit Retry when not Response.Is_Empty (Result);
         end;
      end loop Retry;
   end Internal_Post_Without_Attachment_2;

   ---------------------
   -- Internal_Upload --
   ---------------------

   procedure Internal_Upload
     (Connection : in out HTTP_Connection;
      Result     : out Response.Data;
      Filename   : String;
      URI        : String;
      Headers    : Header_List := Empty_Header_List;
      Progress   : access procedure
                     (Total, Sent : Stream_Element_Offset) := null) is
   begin
      if Connection.HTTP_Version = HTTPv1 then
         Internal_Upload_1
           (Connection, Result, Filename, URI, Headers, Progress);
      else
         Internal_Upload_2
           (Connection, Result, Filename, URI, Headers, Progress);
      end if;
   end Internal_Upload;

   -----------------------
   -- Internal_Upload_1 --
   -----------------------

   procedure Internal_Upload_1
     (Connection : in out HTTP_Connection;
      Result     : out Response.Data;
      Filename   : String;
      URI        : String;
      Headers    : Header_List := Empty_Header_List;
      Progress   : access procedure
                     (Total, Sent : Stream_Element_Offset) := null)
   is
      use Ada.Real_Time;
      Stamp    : constant Time   := Clock;
      Pref_Suf : constant String := "--";
      Boundary : constant String :=
                   "AWS_File_Upload-" & Utils.Random_String (8);
      CT        : constant String :=
                    Messages.Content_Type (MIME.Content_Type (Filename));
      CD        : constant String :=
                    Messages.Content_Disposition
                      ("form-data", "filename", URL.Encode (Filename));
      File_Size : constant Stream_Element_Offset :=
                    Stream_Element_Offset (Utils.File_Size (Filename));

      Try_Count     : Natural := Connection.Retry;
      Auth_Attempts : Auth_Attempts_Count := (others => 2);
      Auth_Is_Over  : Boolean;

      function Content_Length return Stream_Element_Offset;
      --  Returns the total message content length

      procedure Send_File;
      --  Send file content to the server

      --------------------
      -- Content_Length --
      --------------------

      function Content_Length return Stream_Element_Offset is
      begin
         return 2 * Boundary'Length  -- 2 boundaries
           + 4                       -- two boundaries start with "--"
           + 2                       -- second one ends with "--"
           + 10                      -- 5 lines with CR+LF
           + CT'Length               -- content type header
           + CD'Length               -- content disposition header
           + File_Size
           + 2;                      -- CR+LF after file data
      end Content_Length;

      ---------------
      -- Send_File --
      ---------------

      procedure Send_File is
         Sock   : Net.Socket_Type'Class renames Connection.Socket.all;
         Buffer : Stream_Element_Array (1 .. 4_096);
         Last   : Stream_Element_Offset;
         File   : Stream_IO.File_Type;
         Sent   : Stream_Element_Offset := 0;
      begin
         --  Send multipart message start boundary

         Net.Buffered.Put_Line (Sock, Pref_Suf & Boundary);

         --  Send Content-Disposition header

         Net.Buffered.Put_Line (Sock, CD);

         --  Send Content-Type: header

         Net.Buffered.Put_Line (Sock, CT);

         Net.Buffered.New_Line (Sock);

         --  Send file content

         Stream_IO.Open (File, Stream_IO.In_File, Filename);

         while not Stream_IO.End_Of_File (File) loop
            Stream_IO.Read (File, Buffer, Last);
            Net.Buffered.Write (Sock, Buffer (1 .. Last));

            if Progress /= null then
               Sent := Sent + Last;
               Progress (File_Size, Sent);
            end if;
         end loop;

         Stream_IO.Close (File);

         Net.Buffered.New_Line (Sock);

         --  Send multipart message end boundary

         Net.Buffered.Put_Line (Sock, Pref_Suf & Boundary & Pref_Suf);

      exception
         when Net.Socket_Error =>
            --  Properly close the file if needed
            if Stream_IO.Is_Open (File) then
               Stream_IO.Close (File);
            end if;
            raise;
      end Send_File;

   begin
      Retry : loop
         begin
            Open_Set_Common_Header (Connection, "POST", URI, Headers);

            declare
               Sock : Net.Socket_Type'Class renames Connection.Socket.all;
            begin
               --  Send message Content-Type (Multipart/form-data)

               Set_Header
                 (Connection.F_Headers,
                  Messages.Content_Type_Token,
                  MIME.Multipart_Form_Data
                  & "; boundary=""" & Boundary & '"');

               --  Send message Content-Length

               Set_Header
                 (Connection.F_Headers,
                  Messages.Content_Length_Token,
                  Utils.Image (Content_Length));

               AWS.Headers.Send_Header
                 (Sock, Connection.F_Headers, End_Block => True);

               --  Send message body

               Send_File;
            end;

            --  Get answer from server

            Get_Response
              (Connection, Result, Get_Body => not Connection.Streaming);

            Decrement_Authentication_Attempt
              (Connection, Auth_Attempts, Auth_Is_Over);

            if Auth_Is_Over then
               return;

            elsif Connection.Streaming then
               Read_Body (Connection, Result, Store => False);
            end if;

         exception
            when E : Net.Socket_Error | Connection_Error =>
               Error_Processing
                 (Connection, Try_Count, Result, "Upload", E, Stamp);

               exit Retry when not Response.Is_Empty (Result);
         end;
      end loop Retry;
   end Internal_Upload_1;

   -----------------------
   -- Internal_Upload_2 --
   -----------------------

   procedure Internal_Upload_2
     (Connection : in out HTTP_Connection;
      Result     : out Response.Data;
      Filename   : String;
      URI        : String;
      Headers    : Header_List := Empty_Header_List;
      Progress   : access procedure
                     (Total, Sent : Stream_Element_Offset) := null)
   is
      use Ada.Real_Time;

      CT        : constant String :=
                    Messages.Content_Type (MIME.Content_Type (Filename));
      CD        : constant String :=
                    Messages.Content_Disposition
                      ("form-data", "filename", URL.Encode (Filename));

      Stamp     : constant Time := Clock;

      Pref_Suf  : constant String := "--";
      Boundary  : constant String :=
                    "AWS_Attachment-" & Utils.Random_String (8);

      Request       : HTTP2.Message.Object;
      Try_Count     : Natural := Connection.Retry;
      Auth_Attempts : Auth_Attempts_Count := (others => 2);
      Auth_Is_Over  : Boolean;
      Stream        : HTTP2.Stream.Object;
      Ctx           : Server.Context.Object (null,
                                             1,
                                             Connection.Enc_Table'Access,
                                             Connection.Dec_Table'Access,
                                             Connection.H2_Connection'Access);

   begin
      Connection.F_Headers.Reset;

      Retry : loop
         begin
            Open_Set_Common_Header
              (Connection, Messages.Post_Token, URI, Headers);

            Set_Header
              (Connection.F_Headers,
               Messages.Content_Type_Token,
               MIME.Multipart_Form_Data
               & "; boundary=""" & Boundary & '"');

            if not Connection.H2_Preface_Sent then
               --  Update H_Connection with server settings
               Send_H2_Connection_Preface (Connection);
            end if;

            --  Create frames and send them

            Stream := HTTP2.Stream.Create
              (Connection.Socket,
               Connection.H2_Stream_Id, Ctx.Settings.Initial_Window_Size);

            Next_Stream_Id (Connection);

            Request := HTTP2.Message.Create
              (Connection.F_Headers,
               Stream_Element_Array'(1 .. 0 => <>),
               Stream.Identifier);

            --  Append file content

            Request.Append_Body (Pref_Suf & Boundary & CRLF);

            declare
               procedure Write (Data : String);
               procedure Write
                 (Data      : Stream_Element_Array;
                  Next_Size : in out Stream_Element_Count);

               Total : constant Stream_Element_Offset :=
                         Stream_Element_Offset
                           (Resources.Files.File_Size (Filename));

               Sent  : Stream_Element_Offset := 0;

               -----------
               -- Write --
               -----------

               procedure Write (Data : String) is
               begin
                  Request.Append_Body (Data);
               end Write;

               procedure Write
                 (Data      : Stream_Element_Array;
                  Next_Size : in out Stream_Element_Count)
               is
                  pragma Unreferenced (Next_Size);
               begin
                  Request.Append_Body (Data);

                  Sent := Sent + Data'Length;

                  if Progress /= null then
                     Progress (Total, Sent);
                  end if;
               end Write;

               procedure Send_File is
                 new AWS.Server.HTTP_Utils.Send_File_G (Write);

               Length : Resources.Content_Length_Type := 0;

               File   : Resources.File_Type;

            begin
               --  Append part headers

               Write (CD & CRLF);
               Write (CT & CRLF);
               Write (CRLF);

               Resources.Files.Open (File, Filename);

               Send_File
                 (null, 1, File, 1,
                  Chunk_Size => 4 * 1024,
                  Length     => Length);

               Resources.Close (File);

               Write (CRLF);
            end;

            Request.Append_Body (Pref_Suf & Boundary & Pref_Suf & CRLF);

            Send_H2_Request (Connection, Ctx, Stream, Request);

            --  Get response

            Get_H2_Response (Connection, Ctx, Stream, Result);

            Decrement_Authentication_Attempt
              (Connection, Auth_Attempts, Auth_Is_Over);

            exit Retry when Auth_Is_Over;
         exception
            when E : Net.Socket_Error
                   | Connection_Error
                   | HTTP2.Protocol_Error
              =>
               Error_Processing
                 (Connection, Try_Count, Result, "Upload", E, Stamp);

               exit Retry when not Response.Is_Empty (Result);
         end;
      end loop Retry;
   end Internal_Upload_2;

   --------------------
   -- Next_Stream_Id --
   --------------------

   procedure Next_Stream_Id (Connection : in out HTTP_Connection) is
      use type AWS.HTTP2.Stream_Id;
   begin
      Connection.H2_Stream_Id := Connection.H2_Stream_Id + 2;
   end Next_Stream_Id;

   ----------------------------
   -- Open_Set_Common_Header --
   ----------------------------

   procedure Open_Set_Common_Header
     (Connection : in out HTTP_Connection;
      Method     : String;
      URI        : String;
      Headers    : Header_List := Empty_Header_List)
   is
      Is_H2   : constant Boolean := Connection.HTTP_Version = HTTPv2;
      Sock    : Net.Socket_Access := Connection.Socket;
      No_Data : Unbounded_String renames Null_Unbounded_String;

      function Persistence return String with Inline;
      --  Returns "Keep-Alive" is we have a persistent connection and "Close"
      --  otherwise.

      function Encoded_URI return String is
        (Strings.Fixed.Translate (URI, Strings.Maps.To_Mapping (" ", "+")));
      --  Returns URI encoded (' ' -> '+')

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

      Host_Address : constant String :=
                       AWS.URL.Host
                         (Connection.Host_URL, IPv6_Brackets => True)
                       & AWS.URL.Port_Not_Default (Connection.Host_URL);

   begin
      --  Open connection if needed

      if not Connection.Opened then
         Connect (Connection);
         Sock := Connection.Socket;
      end if;

      Sock.Set_Timeout (Connection.Timeouts.Send);

      --  First the the method (request) line

      if Is_H2 then
         Set_Header
           (Connection.F_Headers,
            Messages.Method_Token,
            Method);

         Set_Header
           (Connection.F_Headers,
            Messages.Scheme_Token,
            AWS.URL.Protocol_Name (Connection.Connect_URL));
      end if;

      if Connection.Proxy = No_Data
        or else AWS.URL.Security (Connection.Host_URL)
      then
         --  Without proxy or over proxy tunneling.
         --  In both cases we want to send the pathname only, we are not
         --  required to send the absolute path.

         if URI = "" then
            if Is_H2 then
               Set_Header
                 (Connection.F_Headers,
                  Messages.Path2_Token,
                  AWS.URL.Pathname_And_Parameters (Connection.Host_URL));
            else
               Set_Header
                 (Connection.F_Headers,
                  Method,
                  AWS.URL.Pathname_And_Parameters (Connection.Host_URL)
                  & ' ' & AWS.HTTP_Version);
            end if;

         else
            if Is_H2 then
               Set_Header
                 (Connection.F_Headers,
                  Messages.Path2_Token,
                  Encoded_URI);
            else
               Set_Header
                 (Connection.F_Headers,
                  Method, Encoded_URI & ' ' & AWS.HTTP_Version);
            end if;
         end if;

      else
         --  We have a proxy configured, in thoses case we want to send the
         --  absolute path and parameters.

         if URI = "" then
            if Is_H2 then
               Set_Header
                 (Connection.F_Headers,
                  Messages.Path2_Token,
                  AWS.URL.URL (Connection.Host_URL));
            else
               Set_Header
                 (Connection.F_Headers,
                  Method,
                  AWS.URL.URL (Connection.Host_URL) & ' ' & AWS.HTTP_Version);
            end if;

         else
            --  Send GET http://<host>[:port]/URI HTTP/1.1

            if Is_H2 then
               Set_Header
                 (Connection.F_Headers,
                  Messages.Path2_Token,
                  URL.Protocol_Name (Connection.Host_URL) & "://"
                  & Host_Address & Encoded_URI);
            else
               Set_Header
                 (Connection.F_Headers,
                  Method,
                  URL.Protocol_Name (Connection.Host_URL) & "://"
                  & Host_Address & Encoded_URI & ' ' & AWS.HTTP_Version);
            end if;
         end if;
      end if;

      Connection.F_Headers.Union (Headers, Unique => True);
      Connection.F_Headers.Union (Connection.C_Headers, Unique => True);

      --  Header command

      if Connection.Proxy = No_Data
        or else AWS.URL.Security (Connection.Host_URL)
      then
         --  Unless Header already contains connection info (like would be
         --  the case for web sockets for instance)

         if not Is_H2
           and then not Connection.F_Headers.Exist (Messages.Connection_Token)
         then
            Set_Header
              (Connection.F_Headers, Messages.Connection_Token, Persistence);
         end if;

      else
         if not Is_H2 then
            Set_Header
              (Connection.F_Headers,
               Messages.Proxy_Connection_Token,
               Persistence);
         end if;

         --  Proxy Authentication

         Set_Authentication_Header
           (Connection,
            Messages.Proxy_Authorization_Token,
            Connection.Auth (Proxy),
            URI,
            Method);
      end if;

      --  Cookie

      if Connection.Cookie /= No_Data then
         Set_Header
           (Connection.F_Headers,
            Messages.Cookie_Token,
            To_String (Connection.Cookie));
      end if;

      Set_Header
        (Connection.F_Headers, Messages.Host_Token, Host_Address);

      Set_Header
        (Connection.F_Headers, Messages.Accept_Token, "text/html, */*");

      Set_Header
        (Connection.F_Headers,
         Messages.Accept_Encoding_Token, "gzip, deflate");

      Set_Header
        (Connection.F_Headers, Messages.Accept_Language_Token, "fr, ru, us");

      if Connection.User_Agent /= Null_Unbounded_String then
         Set_Header
           (Connection.F_Headers,
            Messages.User_Agent_Token, To_String (Connection.User_Agent));
      end if;

      if Connection.Data_Range /= No_Range then
         Set_Header
           (Connection.F_Headers,
            Messages.Range_Token, Image (Connection.Data_Range));
      end if;

      --  User Authentication

      Set_Authentication_Header
        (Connection,
         Messages.Authorization_Token,
         Connection.Auth (WWW),
         URI,
         Method);
   end Open_Set_Common_Header;

   ---------------
   -- Read_Body --
   ---------------

   procedure Read_Body
     (Connection : in out HTTP_Connection;
      Result     : out Response.Data;
      Store      : Boolean)
   is
      use Ada.Real_Time;
      Expire : constant Time := Clock + Connection.Timeouts.Response;
   begin
      loop
         declare
            Buffer : Stream_Element_Array (1 .. 8192);
            Last   : Stream_Element_Offset;
         begin
            Read_Some (Connection, Buffer, Last);
            exit when Last < Buffer'First;

            if Store then
               Response.Set.Append_Body
                 (Result, Buffer (Buffer'First .. Last));
            end if;
         end;

         if Clock > Expire then
            if Store then
               Response.Set.Append_Body
                 (Result, "..." & ASCII.LF & " Response Timeout");
            end if;

            Response.Set.Status_Code (Result, Messages.S408);
            exit;
         end if;
      end loop;
   end Read_Body;

   -----------------------
   -- Read_Parse_Header --
   -----------------------

   procedure Read_Parse_Header
     (Connection : in out HTTP_Connection;
      Answer     : in out Response.Data;
      Keep_Alive : out Boolean)
   is
      procedure Parse_Authenticate_Line
        (Level     : Authentication_Level;
         Auth_Line : String);
      --  Parses Authentication request line and fill Connection.Auth (Level)
      --  field with the information read on the line. Handle WWW and Proxy
      --  authentication.

      procedure Read_Status_Line
        with Pre => Connection.HTTP_Version = HTTPv1;
      --  Read the status line

      procedure Set_Keep_Alive (Data : String);
      --  Set the Parse_Header.Keep_Alive depending on data from the
      --  Proxy-Connection or Connection header line.

      function "+" (S : String) return Unbounded_String
             renames To_Unbounded_String;

      Sock   : Net.Socket_Type'Class renames Connection.Socket.all;
      Status : Messages.Status_Code;

      Request_Auth_Mode : array (Authentication_Level)
                            of Authentication_Mode := (others => Any);

      -----------------------------
      -- Parse_Authenticate_Line --
      -----------------------------

      procedure Parse_Authenticate_Line
        (Level     : Authentication_Level;
         Auth_Line : String)
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
           (Item : String;
            Quit : in out Boolean);
         --  Routine receiving unnamed value during parsing of
         --  authentication line.

         procedure Named_Value
           (Name  : String;
            Value : String;
            Quit  : in out Boolean);
         --  Routine receiving name/value pairs during parsing of
         --  authentication line.

         -----------------
         -- Named_Value --
         -----------------

         procedure Named_Value
           (Name  : String;
            Value : String;
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
                  raise Constraint_Error
                    with "Only MD5 algorithm is supported.";
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
           (Item : String;
            Quit : in out Boolean)
         is
            pragma Warnings (Off, Quit);
            Mode_Image : constant String := To_Upper (Item);
         begin
            if Mode_Image = Digest_Token then
               Request_Mode := Digest;
            elsif Mode_Image = Basic_Token then
               Request_Mode := Basic;
            else
               Request_Mode := Unknown;
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

         if Utils.Match (Line, Messages.HTTP_Token) then
            Status :=
              Messages.Status_Code'Value
                ('S' & Line (Messages.HTTP_Token'Length + Line'First + 4
                              .. Messages.HTTP_Token'Length + Line'First + 6));
            Response.Set.Status_Code (Answer, Status);

            --  By default HTTP/1.0 connection is not keep-alive but
            --  HTTP/1.1 is keep-alive.

            Keep_Alive :=
              Line (Messages.HTTP_Token'Length + Line'First
                    .. Messages.HTTP_Token'Length + Line'First + 2) >= "1.1";
         else
            --  or else it is wrong answer from server
            raise Protocol_Error with Line;
         end if;
      end Read_Status_Line;

      --------------------
      -- Set_Keep_Alive --
      --------------------

      procedure Set_Keep_Alive (Data : String) is
      begin
         if Utils.Match (Data, "Close") then
            Keep_Alive := False;

         elsif Utils.Match (Data, "Keep-Alive") then
            Keep_Alive := True;
         end if;
      end Set_Keep_Alive;

      use type Messages.Status_Code;

      function Get_Content_Encoding return String is
        (Characters.Handling.To_Lower
           (Response.Header (Answer, Messages.Content_Encoding_Token)));

   begin
      --  Reset authentication information

      for Level in Authentication_Level'Range loop
         Connection.Auth (Level).Requested := False;
      end loop;

      --  By default we have at least some headers. This value will be
      --  updated if a message body is read.

      Response.Set.Mode (Answer, Response.Header);

      --  Read headers from server's answer only in HTTP/1.x mode

      if Connection.HTTP_Version = HTTPv1 then
         Read_Status_Line;
         Response.Set.Read_Header (Sock, Answer);
         Response.Set.Parse_Header (Answer);

         declare

            procedure Decode_Init (Z_Header : ZLib.Header_Type);

            -----------------
            -- Decode_Init --
            -----------------

            procedure Decode_Init (Z_Header : ZLib.Header_Type) is
               use type Utils.Stream_Element_Array_Access;
            begin
               ZLib.Inflate_Init
                 (Connection.Decode_Filter, Header => Z_Header);

               if Connection.Decode_Buffer = null then
                  Connection.Decode_Buffer :=
                    new Stream_Element_Array (1 .. 8096);
               end if;

               Connection.Decode_First := Connection.Decode_Buffer'Last + 1;
               Connection.Decode_Last  := Connection.Decode_Buffer'Last;
            end Decode_Init;

            Content_Encoding : constant String := Get_Content_Encoding;

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

      else -- HTTP/2
         Response.Header (Answer).Debug_Print;

         --  In HTTP/2 the status is encoded in :status pseudo header

         Status := Messages.Status_Code'Value
                     ('S' & Response.Header (Answer, Messages.Status_Token));
         Response.Set.Status_Code (Answer, Status);
         Response.Set.Parse_Header (Answer);

         declare
            Content_Encoding : constant String := Get_Content_Encoding;
         begin
            if Content_Encoding in "gzip" | "deflate" then
               Response.Set.Data_Encoding
                 (Answer, Messages.Content_Encoding'Value (Content_Encoding),
                  Direction => Response.Set.Decode);
            end if;
         end;
      end if;

      --  ??? we should not expect 100 response message after the body sent.
      --  This code needs to be fixed.
      --  We should expect 100 status line only before sending the message
      --  body to server.
      --  And we should send Expect: header line in the header if we could
      --  deal with 100 status code.
      --  See [RFC 2616 - 8.2.3] use of the 100 (Continue) Status.

      if Connection.HTTP_Version = HTTPv1
        and then Status = Messages.S100
      then
         Read_Status_Line;
         Response.Set.Read_Header (Sock, Answer);
         Response.Set.Parse_Header (Answer);
      end if;

      Set_Keep_Alive
        (Response.Header (Answer, Messages.Connection_Token));

      Set_Keep_Alive
        (Response.Header (Answer, Messages.Proxy_Connection_Token));

      --  Read and store all cookies from response header

      declare
         Set_Cookies : constant Headers.VString_Array :=
                         Response.Header (Answer).Get_Values
                           (Messages.Set_Cookie_Token);
         Cookie      : Unbounded_String;
         I           : Natural;
      begin
         for K in Set_Cookies'Range loop
            if Set_Cookies (K) /= Null_Unbounded_String then
               I := Strings.Unbounded.Index (Set_Cookies (K), ";");

               if Cookie /= Null_Unbounded_String then
                  Append (Cookie, "; ");
               end if;

               --  We found a cookie NAME=VALUE, record it

               if I = 0 then
                  Append (Cookie, Set_Cookies (K));
               else
                  Append (Cookie, Slice (Set_Cookies (K), 1, I - 1));
               end if;
            end if;
         end loop;

         --  If we have some value, update the connection status

         if Cookie /= Null_Unbounded_String then
            Connection.Cookie := Cookie;
         end if;
      end;

      Parse_Authenticate_Line
        (WWW,
         Response.Header (Answer, Messages.WWW_Authenticate_Token));

      Parse_Authenticate_Line
        (Proxy,
         Response.Header (Answer, Messages.Proxy_Authenticate_Token));

      if Debug_On then
         declare
            List : constant AWS.Headers.List := Response.Header (Answer);
         begin
            for J in 1 .. List.Count loop
               Debug_Message ("< ", List.Get_Line (J));
            end loop;
         end;
      end if;
   end Read_Parse_Header;

   --------------------------------
   -- Send_H2_Connection_Preface --
   --------------------------------

   procedure Send_H2_Connection_Preface (Connection : in out HTTP_Connection)
   is
      use all type HTTP2.Frame.Kind_Type;
   begin
      --  Send the HTTP/2 connection preface

      Net.Buffered.Write
        (Connection.Socket.all, HTTP2.Client_Connection_Preface);

      --  Send the setting frame (stream id 0)

      HTTP2.Frame.Settings.Create (Connection.H2_Settings).Send
        (Connection.Socket.all);

      --  We need to read the settings from server

      declare
         Frame : constant HTTP2.Frame.Object'Class :=
                   HTTP2.Frame.Read
                     (Connection.Socket.all, Connection.H2_Connection);
      begin
         if Frame.Kind /= K_Settings then
            if HTTP2.Debug then
               Frame.Dump ("UNEXPECTED");
            end if;

            raise Constraint_Error with
              "server should have answered with a setting frame";

         else
            declare
               S_Frame : constant HTTP2.Frame.Settings.Object :=
                           HTTP2.Frame.Settings.Object (Frame);
            begin
               --  Make sure the settings frame is not an aknowledged, this
               --  should not happen anyway.

               if not S_Frame.Has_Flag (HTTP2.Frame.Ack_Flag) then
                  HTTP2.Connection.Set
                    (Connection.H2_Connection,
                     HTTP2.Frame.Settings.Values (S_Frame));
               end if;
            end;
         end if;
      end;

      Connection.H2_Preface_Sent := True;
   end Send_H2_Connection_Preface;

   ---------------------
   -- Send_H2_Request --
   ---------------------

   procedure Send_H2_Request
     (Connection : in out HTTP_Connection;
      Ctx        : in out Server.Context.Object;
      Stream     : in out HTTP2.Stream.Object;
      Request    : in out HTTP2.Message.Object)
   is
      use all type HTTP2.Frame.Kind_Type;
      Result : Response.Data;
   begin
      All_Frames : loop
         for F of Request.To_Frames (Ctx, Stream) loop
            Stream.Send_Frame (F);

            if F.Kind = HTTP2.Frame.K_Data then
               Ctx.Settings.Update_Flow_Control_Window
                 (-Natural (F.Length));
            end if;
         end loop;

         exit All_Frames when not Request.More_Frames;

         while Ctx.Settings.Flow_Control_Window <= 0
           or else Stream.Flow_Control_Window <= 0
         loop
            Get_H2_Frame (Connection, Ctx, Stream, Result);
         end loop;
      end loop All_Frames;
   end Send_H2_Request;

   ------------------
   -- Send_Request --
   ------------------

   procedure Send_Request
     (Connection : in out HTTP_Connection;
      Kind       : Method_Kind;
      Result     : out Response.Data;
      URI        : String;
      Data       : Stream_Element_Array := No_Data;
      Headers    : Header_List := Empty_Header_List) is
   begin
      if Connection.HTTP_Version = HTTPv1 then
         Send_Request_1 (Connection, Kind, Result, URI, Data, Headers);
      else
         Send_Request_2 (Connection, Kind, Result, URI, Data, Headers);
      end if;
   end Send_Request;

   --------------------
   -- Send_Request_1 --
   --------------------

   procedure Send_Request_1
     (Connection : in out HTTP_Connection;
      Kind       : Method_Kind;
      Result     : out Response.Data;
      URI        : String;
      Data       : Stream_Element_Array := No_Data;
      Headers    : Header_List := Empty_Header_List)
   is
      use Ada.Real_Time;
      Stamp         : constant Time := Clock;
      Try_Count     : Natural := Connection.Retry;
      Auth_Attempts : Auth_Attempts_Count := (others => 2);
      Auth_Is_Over  : Boolean;
   begin
      Retry : loop
         begin
            Open_Set_Common_Header
              (Connection, Method_Kind'Image (Kind), URI, Headers);

            --  Add content length if needed

            if Data'Length > 0 then
               Set_Header
                 (Connection.F_Headers,
                  Messages.Content_Length_Token,
                  Utils.Image (Stream_Element_Offset'(Data'Length)));
            end if;

            --  Send all headers for this connection

            AWS.Headers.Send_Header
              (Connection.Socket.all,
               Connection.F_Headers,
               End_Block => True);

            --  If there is some data to send

            if Data'Length > 0 then
               --  Send message body

               Net.Buffered.Write (Connection.Socket.all, Data);
            end if;

            Get_Response
              (Connection, Result,
               Get_Body => Kind /= HEAD and then not Connection.Streaming);

            Decrement_Authentication_Attempt
              (Connection, Auth_Attempts, Auth_Is_Over);

            if Auth_Is_Over then
               return;

            elsif Kind /= HEAD and then Connection.Streaming then
               Read_Body (Connection, Result, Store => False);
            end if;

         exception
            when E : Net.Socket_Error | Connection_Error =>
               Error_Processing
                 (Connection, Try_Count, Result,
                  Method_Kind'Image (Kind), E, Stamp);

               exit Retry when not Response.Is_Empty (Result);
         end;
      end loop Retry;
   end Send_Request_1;

   --------------------
   -- Send_Request_2 --
   --------------------

   procedure Send_Request_2
     (Connection : in out HTTP_Connection;
      Kind       : Method_Kind;
      Result     : out Response.Data;
      URI        : String;
      Data       : Stream_Element_Array := No_Data;
      Headers    : Header_List := Empty_Header_List)
   is
      use Ada.Real_Time;

      Stamp         : constant Time := Clock;
      Try_Count     : Natural := Connection.Retry;
      Auth_Attempts : Auth_Attempts_Count := (others => 2);
      Auth_Is_Over  : Boolean;

   begin
      Connection.F_Headers.Reset;

      Retry : loop
         begin
            Open_Set_Common_Header
              (Connection, Method_Kind'Image (Kind), URI, Headers);

            Handle_H2_Request
              (Connection, Result, Data, Auth_Attempts, Auth_Is_Over);

            exit Retry when Auth_Is_Over;
         exception
            when E : Net.Socket_Error
                   | Connection_Error
                   | HTTP2.Protocol_Error
              =>
               Error_Processing
                 (Connection, Try_Count, Result,
                  Method_Kind'Image (Kind), E, Stamp);

               exit Retry when not Response.Is_Empty (Result);
         end;
      end loop Retry;
   end Send_Request_2;

   ------------------------
   -- Set_Authentication --
   ------------------------

   procedure Set_Authentication
     (Auth : out Authentication_Type;
      User : String;
      Pwd  : String;
      Mode : Authentication_Mode) is
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

   -------------------------------
   -- Set_Authentication_Header --
   -------------------------------

   procedure Set_Authentication_Header
     (Connection : in out HTTP_Connection;
      Token      : String;
      Data       : in out Authentication_Type;
      URI        : String;
      Method     : String)
   is
      User : constant String := To_String (Data.User);
      Pwd  : constant String := To_String (Data.Pwd);
   begin
      if User /= Client.No_Data and then Pwd /= Client.No_Data then

         if Data.Work_Mode = Basic then
            Set_Header
              (Connection.F_Headers,
               Token,
               "Basic " & AWS.Translator.Base64_Encode (User & ':' & Pwd));

         elsif Data.Work_Mode = Digest then

            declare
               Nonce : constant String := To_String (Data.Nonce);
               Realm : constant String := To_String (Data.Realm);
               QOP   : constant String := To_String (Data.QOP);

               function Get_URI return String;
               --  Returns the real URI where the request is going to be
               --  sent. It is either Open_Set_Common_Header.URI parameter
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
                  CNonce : constant AWS.Digest.Nonce :=
                             AWS.Digest.Create_Nonce;
               begin
                  if QOP = Client.No_Data then
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
               Set_Header
                 (Connection.F_Headers,
                  Token,
                  "Digest "
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
   end Set_Authentication_Header;

   ---------------------
   -- Set_Common_Post --
   ---------------------

   procedure Set_Common_Post
     (Connection   : in out HTTP_Connection;
      Data         : Stream_Element_Array;
      URI          : String;
      SOAPAction   : String;
      Content_Type : String;
      Headers      : Header_List := Empty_Header_List)
   is
      Is_H2 : constant Boolean := Connection.HTTP_Version = HTTPv2;
   begin
      Open_Set_Common_Header (Connection, Messages.Post_Token, URI, Headers);

      if Content_Type /= Client.No_Data then
         Set_Header
           (Connection.F_Headers,
            Messages.Content_Type_Token,
            Content_Type);
      end if;

      if SOAPAction /= Client.No_Data then
         --  SOAP header

         Set_Header
           (Connection.F_Headers, Messages.SOAPAction_Token, SOAPAction);
      end if;

      --  Send message Content_Length

      if not Is_H2 then
         Set_Header
           (Connection.F_Headers,
            Messages.Content_Length_Token,
            Utils.Image (Stream_Element_Offset'(Data'Length)));
      end if;
   end Set_Common_Post;

   ----------------
   -- Set_Header --
   ----------------

   procedure Set_Header
     (Headers : in out Header_List;
      Header  : String;
      Value   : String := "") is
   begin
      if not Headers.Exist (Header) then
         Headers.Add (Header, Value);
         Debug_Message
           ("> ", Header & (if Value = "" then "" else "=" & Value));
      end if;
   end Set_Header;

   -------------------------
   -- Set_HTTP_Connection --
   -------------------------

   procedure Set_HTTP_Connection
     (HTTP_Client : in out HTTP_Connection;
      Sock_Ptr    : Net.Socket_Access) is
   begin
      HTTP_Client.Socket := Sock_Ptr;
      HTTP_Client.Opened := True;
   end Set_HTTP_Connection;

   -----------
   -- Value --
   -----------

   function Value (V : String) return Unbounded_String is
   begin
      if V = Client.No_Data then
         return Null_Unbounded_String;
      else
         return To_Unbounded_String (V);
      end if;
   end Value;

end AWS.Client.HTTP_Utils;
