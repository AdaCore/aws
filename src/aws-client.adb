------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2000-2001                          --
--                               ACT-Europe                                 --
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

with Ada.Strings.Unbounded;
with Ada.Streams;
with Ada.Unchecked_Deallocation;

with Sockets;

with AWS.Messages;
with AWS.MIME;
with AWS.Translator;
with AWS.Net;

package body AWS.Client is

   use Ada;
   use Ada.Strings.Unbounded;

   End_Section : constant String := "";

   procedure Get_Response
     (Connection : in out HTTP_Connection;
      Result     :    out Response.Data;
      Get_Body   : in     Boolean         := True);
   --  Receives response from server for GET and POST and HEAD commands.
   --  If Get_Body is set then the message body will be read.

   procedure Parse_Header
     (Sock              : in     Sockets.Socket_FD'Class;
      Status            :    out Messages.Status_Code;
      Content_Length    :    out Natural;
      Content_Type      :    out Unbounded_String;
      Transfer_Encoding :    out Unbounded_String;
      Location          :    out Unbounded_String;
      Connection        :    out Unbounded_String;
      Cookie            :    out Unbounded_String);
   --  Read server answer and set corresponding variable with the value
   --  read. Most of the fields are ignored right now.

   procedure Disconnect (Connection : in out HTTP_Connection);
   --  Close connection. Further use is not possible.

   procedure Open_Send_Common_Header
     (Connection : in out HTTP_Connection;
      Method     : in     String;
      URI        : in     String);
   --  Open the the Connection if it is not open. Send the common HTTP headers
   --  for all requests like the proxy, authentification, user agent, host.

   ----------------
   -- Disconnect --
   ----------------

   procedure Disconnect (Connection : in out HTTP_Connection) is
   begin
      Sockets.Shutdown (Connection.Socket.all);
      Connection.Opened := False;
   end Disconnect;

   ------------------
   -- Get_Response --
   ------------------

   procedure Get_Response
     (Connection : in out HTTP_Connection;
      Result     :    out Response.Data;
      Get_Body   : in     Boolean         := True)
   is

      function Read_Chunk return Streams.Stream_Element_Array;
      --  read a chunk object from the stream

      function Read_Message return String;
      --  read a textual message from the socket for which there is no known
      --  length.

      procedure Disconnect;
      --  close connection socket.

      Sock : Sockets.Socket_FD'Class := Connection.Socket.all;

      CT       : Unbounded_String;
      CT_Len   : Natural              := 0;
      TE       : Unbounded_String;
      Location : Unbounded_String;
      Connect  : Unbounded_String;
      Status   : Messages.Status_Code;
      Message  : Unbounded_String;

      ----------------
      -- Read_Chunk --
      ----------------

      function Read_Chunk return Streams.Stream_Element_Array is

         use Streams;

         use type Stream_Element_Array;
         use type Stream_Element_Offset;

         type Stream_Element_Array_Access is access Stream_Element_Array;

         procedure Free is new Ada.Unchecked_Deallocation
           (Stream_Element_Array, Stream_Element_Array_Access);

         Data : Stream_Element_Array_Access :=
           new Streams.Stream_Element_Array (1 .. 10_000);

         Data_Last : Streams.Stream_Element_Offset := 0;

         procedure Skip_Line;
         --  skip a line on the socket

         procedure Skip_Line is
            D : constant String := Sockets.Get_Line (Sock);
            pragma Warnings (Off, D);
         begin
            null;
         end Skip_Line;

         Size : Stream_Element_Offset;
         Help : Stream_Element_Array_Access;

      begin
         loop
            --  Read the chunk size that is an hex number
            Size := Stream_Element_Offset'Value
              ("16#" & Sockets.Get_Line (Sock) & '#');

            if Size = 0 then
               Skip_Line;
               exit;

            else
               if Data_Last + Size > Data'Last then

                  Help := new Stream_Element_Array
                    (1
                     .. Stream_Element_Offset'Max
                     (Data_Last + Size, 2 * Data'Length));

                  Help (1 .. Data_Last) := Data (1 .. Data_Last);
                  Free (Data);
                  Data := Help;
               end if;

               Sockets.Receive
                 (Sock, Data (Data_Last + 1 .. Data_Last + Size));

               Skip_Line;
               Data_Last := Data_Last + Size;
            end if;

         end loop;

         declare
            Copy : Stream_Element_Array (1 .. Data_Last);
         begin
            Copy := Data (1 .. Data_Last);
            Free (Data);
            return Copy;
         end;

      exception
         when others =>
            Free (Data);
            raise;
      end Read_Chunk;

      ------------------
      -- Read_Message --
      ------------------

      function Read_Message return String is
         Results : Unbounded_String;
      begin
         --  we don't know the message body length, so read the socket until
         --  it is closed by the server. At this time an exception will be
         --  raised as are trying to read the socket.

         while True loop
            declare
               One_Line : constant String := Sockets.Get_Line (Sock);
            begin
               Append (Results, One_Line);
            end;
         end loop;

         return To_String (Results);

      exception
         when others =>
            return To_String (Results);
      end Read_Message;

      use type Messages.Status_Code;

      ----------------
      -- Disconnect --
      ----------------

      procedure Disconnect is
      begin
         if Messages.Is_Match (To_String (Connect), "close") then
            Disconnect (Connection);
         end if;
      end Disconnect;


   begin
      Parse_Header
        (Sock, Status, CT_Len, CT, TE, Location, Connect, Connection.Cookie);

      --  check for special status

      if Status = Messages.S301 then
         --  moved permanently

         Result := Response.Build (To_String (CT),
                                   To_String (Location),
                                   Status);
         Disconnect;
         return;
      end if;

      if not Get_Body then
         Result := Response.Build (To_String (CT), "", Status);
         Disconnect;
         return;
      end if;

      --  read the message body

      if To_String (TE) = "chunked" then

         --  a chuncked message is written on the stream as list of data
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

         Result := Response.Build (To_String (CT), Read_Chunk, Status);

      else
         if CT_Len = 0 and then CT = MIME.Text_HTML then
            --  Here we do not know the message body length, but this is a
            --  textual data, read it as a string.
            Result := Response.Build (To_String (CT), Read_Message, Status);
         else

            declare
               Elements : Streams.Stream_Element_Array
                  (1 .. Streams.Stream_Element_Offset (CT_Len));
            begin
               Sockets.Receive (Sock, Elements);

               if CT = MIME.Text_HTML or else CT = MIME.Text_XML then

                  --  if the content is textual info put it in a string

                  for K in Elements'Range loop
                     Append (Message, Character'Val (Natural (Elements (K))));
                  end loop;

                  Result := Response.Build (To_String (CT),
                                            To_String (Message),
                                            Status);
               else

                  --  this is some kind of binary data.

                  Result := Response.Build
                    (To_String (CT), Elements, Status);
               end if;
            end;
         end if;
      end if;

      Disconnect;
   end Get_Response;

   ---------
   -- Get --
   ---------

   function Get
     (URL        : in String;
      User       : in String := No_Data;
      Pwd        : in String := No_Data;
      Proxy      : in String := No_Data;
      Proxy_User : in String := No_Data;
      Proxy_Pwd  : in String := No_Data)
     return Response.Data
   is

      Connect : HTTP_Connection := Create (URL, User, Pwd,
                                           Proxy, Proxy_User, Proxy_Pwd,
                                           Persistent => False);
      Result  : Response.Data;

   begin
      Get (Connect, Result);
      Close (Connect);
      return Result;
   end Get;

   ----------
   -- Head --
   ----------

   function Head
     (URL        : in String;
      User       : in String := No_Data;
      Pwd        : in String := No_Data;
      Proxy      : in String := No_Data;
      Proxy_User : in String := No_Data;
      Proxy_Pwd  : in String := No_Data)
     return Response.Data
   is

      Connect : HTTP_Connection := Create (URL, User, Pwd,
                                           Proxy, Proxy_User, Proxy_Pwd,
                                           Persistent => False);

      Result : Response.Data;

   begin
      Head (Connect, Result);
      Close (Connect);
      return Result;
   end Head;

   ------------------
   -- Parse_Header --
   ------------------

   procedure Parse_Header
     (Sock              : in     Sockets.Socket_FD'Class;
      Status            :    out Messages.Status_Code;
      Content_Length    :    out Natural;
      Content_Type      :    out Unbounded_String;
      Transfer_Encoding :    out Unbounded_String;
      Location          :    out Unbounded_String;
      Connection        :    out Unbounded_String;
      Cookie            :    out Unbounded_String) is
   begin
      Content_Length := 0;

      loop
         declare
            Line : constant String := Sockets.Get_Line (Sock);
         begin
            if Line = End_Section then
               exit;

            elsif Messages.Is_Match (Line, Messages.HTTP_Token) then
               Status := Messages.Status_Code'Value
                  ('S' & Line (Messages.HTTP_Token'Last + 5
                     .. Messages.HTTP_Token'Last + 7));

            elsif Messages.Is_Match (Line, Messages.Content_Type_Token) then
               Content_Type := To_Unbounded_String
                  (Line (Messages.Content_Type_Token'Last + 1 .. Line'
                     Last));

            elsif Messages.Is_Match (Line, Messages.Content_Length_Token) then
               Content_Length := Natural'Value
                  (Line (Messages.Content_Length_Range'Last + 1 .. Line'
                     Last));

            elsif Messages.Is_Match (Line, Messages.Location_Token) then
               Location := To_Unbounded_String
                  (Line (Messages.Location_Token'Last + 1 .. Line'Last));

            elsif Messages.Is_Match (Line,
                  Messages.Transfer_Encoding_Token) then

               Transfer_Encoding := To_Unbounded_String
                  (Line (Messages.Transfer_Encoding_Range'Last + 1
                     .. Line'Last));

            elsif Messages.Is_Match (Line, Messages.Connection_Token) then
               Connection := To_Unbounded_String
                  (Line (Messages.Connection_Token'Last + 1 .. Line'Last));

            elsif Messages.Is_Match (Line, Messages.
                  Proxy_Connection_Token) then
               Connection := To_Unbounded_String
                  (Line (Messages.Proxy_Connection_Token'Last + 1 .. Line'
                     Last));

            elsif Messages.Is_Match (Line, Messages.Set_Cookie_Token) then
               Cookie := To_Unbounded_String
                  (Line (Messages.Set_Cookie_Token'Last + 1 .. Line'Last));

            else
               --  everything else is ignore right now
               null;
            end if;
         end;
      end loop;
   end Parse_Header;

   ---------
   -- Put --
   ---------

   function Put
     (URL        : in String;
      Data       : in String;
      User       : in String := No_Data;
      Pwd        : in String := No_Data;
      Proxy      : in String := No_Data;
      Proxy_User : in String := No_Data;
      Proxy_Pwd  : in String := No_Data)
     return Response.Data
   is

      Connect : HTTP_Connection := Create (URL, User, Pwd,
                                           Proxy, Proxy_User, Proxy_Pwd,
                                           Persistent => False);
      Result  : Response.Data;

   begin
      Put (Connect, Result, Data);
      Close (Connect);
      return Result;
   end Put;

   ----------
   -- Post --
   ----------

   function Post
     (URL        : in String;
      Data       : in String;
      User       : in String := No_Data;
      Pwd        : in String := No_Data;
      Proxy      : in String := No_Data;
      Proxy_User : in String := No_Data;
      Proxy_Pwd  : in String := No_Data)
     return Response.Data
   is
      use Streams;
   begin
      return Post (URL, Translator.To_Stream_Element_Array (Data),
                   User, Pwd, Proxy, Proxy_User, Proxy_Pwd);
   end Post;

   function Post
     (URL        : in String;
      Data       : in Streams.Stream_Element_Array;
      User       : in String := No_Data;
      Pwd        : in String := No_Data;
      Proxy      : in String := No_Data;
      Proxy_User : in String := No_Data;
      Proxy_Pwd  : in String := No_Data)
     return Response.Data
   is
      Connect : HTTP_Connection
        := Create (URL, User, Pwd, Proxy, Proxy_User, Proxy_Pwd,
                   Persistent => False);

      Result  : Response.Data;

   begin
      Post (Connect, Result, Data);
      Close (Connect);
      return Result;
   end Post;

   ---------------
   -- SOAP_Post --
   ---------------

   function SOAP_Post
     (URL        : in String;
      Data       : in String;
      SOAPAction : in String;
      User       : in String := No_Data;
      Pwd        : in String := No_Data;
      Proxy      : in String := No_Data;
      Proxy_User : in String := No_Data;
      Proxy_Pwd  : in String := No_Data)
     return Response.Data
   is
      Connect : HTTP_Connection
        := Create (URL, User, Pwd, Proxy, Proxy_User, Proxy_Pwd,
                   SOAPAction => SOAPAction, Persistent => False);

      Result  : Response.Data;

   begin
      Post (Connect, Result, Data);
      Close (Connect);
      return Result;
   end SOAP_Post;

   -----------
   -- Close --
   -----------

   procedure Close (Connection : in out HTTP_Connection) is

      procedure Free is new Ada.Unchecked_Deallocation
        (Sockets.Socket_FD'Class, Socket_Access);

   begin
      if Connection.Socket = null then
         return;
      end if;

      Sockets.Shutdown (Connection.Socket.all);
      Free (Connection.Socket);
      Connection.Socket := null;
   end Close;

   -----------------------------
   -- Open_Send_Common_Header --
   -----------------------------

   procedure Open_Send_Common_Header
     (Connection : in out HTTP_Connection;
      Method     : in     String;
      URI        : in     String)
   is

      Sock    : Sockets.Socket_FD'Class := Connection.Socket.all;

      No_Data : Unbounded_String renames Null_Unbounded_String;

      function HTTP_Prefix (Security : in Boolean) return String;
      --  Returns "http://" or "https://" if Security is set to True.

      function Persistence return String;
      --  Returns "Keep-Alive" is we have a persistent connection and "Close"
      --  otherwise.

      function Port_Not_Default (Port : in Positive)
        return String;
      --  Returns the port image (preceded by character ':') if it is not the
      --  default port.

      ----------------------
      -- Port_Not_Default --
      ----------------------

      function Port_Not_Default
        (Port : in Positive)
        return String is
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

      Host_Address : constant String :=
        AWS.URL.Server_Name (Connection.Host_URL)
        & Port_Not_Default (AWS.URL.Port (Connection.Host_URL));

   begin

      --  Open socket if needed.

      if not Connection.Opened then
         Sock := AWS.Net.Connect
           (AWS.URL.Server_Name (Connection.Connect_URL),
            AWS.URL.Port (Connection.Connect_URL),
            AWS.URL.Security (Connection.Connect_URL));

         Connection.Socket.all := Sock;
         Connection.Opened     := True;
      end if;

      --  Header command.

      if Connection.Proxy = No_Data then

         if URI = "" then
            Sockets.Put_Line (Sock, Method & ' '
                              & AWS.URL.URI (Connection.Host_URL)
                              & ' ' & HTTP_Version);
         else
            Sockets.Put_Line (Sock, Method & ' '
                              & URI
                              & ' ' & HTTP_Version);
         end if;

         Sockets.Put_Line (Sock, Messages.Connection (Persistence));

      else
         if URI = "" then
            Sockets.Put_Line (Sock, Method & ' '
                              & To_String (Connection.Host)
                              & ' ' & HTTP_Version);
         else
            Sockets.Put_Line
              (Sock, Method & ' '
               & HTTP_Prefix (AWS.URL.Security (Connection.Host_URL))
               & Host_Address & URI
               & ' ' & HTTP_Version);
         end if;

         Sockets.Put_Line (Sock, Messages.Proxy_Connection (Persistence));

      end if;

      --  Cookie

      if Connection.Cookie /= No_Data then
         Sockets.Put_Line
           (Sock, Messages.Cookie_Token & To_String (Connection.Cookie));
      end if;

      Sockets.Put_Line (Sock, Messages.Host (Host_Address));
      Sockets.Put_Line (Sock, Messages.Accept_Type ("text/html, */*"));
      Sockets.Put_Line (Sock, Messages.Accept_Language ("fr, us"));
      Sockets.Put_Line
        (Sock,
         Messages.User_Agent ("AWS (Ada Web Server) v" & Version));

      --  User Authentification

      if Connection.User /= No_Data
        and then Connection.Pwd /= No_Data
      then
         Sockets.Put_Line
           (Sock,
            Messages.Authorization
            ("Basic",
             AWS.Translator.Base64_Encode
             (To_String (Connection.User)
              & ':' & To_String (Connection.Pwd))));
      end if;

      --  Proxy Authentification

      if Connection.Proxy_User /= No_Data
        and then Connection.Proxy_Pwd /= No_Data
      then
         Sockets.Put_Line
         (Sock,
          Messages.Proxy_Authorization
          ("Basic",
           AWS.Translator.Base64_Encode
           (To_String (Connection.Proxy_User)
            & ':' & To_String (Connection.Proxy_Pwd))));
      end if;

      --  SOAP header

      if Connection.SOAPAction /= No_Data then
         Sockets.Put_Line
           (Sock,
            Messages.SOAPAction (To_String (Connection.SOAPAction)));
      end if;

   end Open_Send_Common_Header;

   ------------
   -- Create --
   ------------

   function Create
     (Host       : in String;
      User       : in String   := No_Data;
      Pwd        : in String   := No_Data;
      Proxy      : in String   := No_Data;
      Proxy_User : in String   := No_Data;
      Proxy_Pwd  : in String   := No_Data;
      Retry      : in Positive := Retry_Default;
      SOAPAction : in String   := No_Data;
      Persistent : in Boolean  := True)
     return HTTP_Connection
   is
      function Set (V : in String) return Unbounded_String;
      --  Returns V as an Unbounded_String if V is not the empty string
      --  otherwise it returns Null_Unbounded_String.

      ---------
      -- Set --
      ---------

      function Set (V : in String) return Unbounded_String is
      begin
         if V = No_Data then
            return Null_Unbounded_String;
         else
            return To_Unbounded_String (V);
         end if;
      end Set;

      Connect_URL : AWS.URL.Object;
      Host_URL    : AWS.URL.Object := AWS.URL.Parse (Host);
      Proxy_URL   : AWS.URL.Object := AWS.URL.Parse (Proxy);
   begin
      if Proxy = No_Data then
         Connect_URL := Host_URL;
      else
         Connect_URL := Proxy_URL;
      end if;

      return (Host        => To_Unbounded_String (Host),
              Host_URL    => Host_URL,
              Connect_URL => Connect_URL,
              User        => Set (User),
              Pwd         => Set (Pwd),
              Proxy       => Set (Proxy),
              Proxy_URL   => Proxy_URL,
              Proxy_User  => Set (Proxy_User),
              Proxy_Pwd   => Set (Proxy_Pwd),
              Opened      => True,
              Socket      => new Sockets.Socket_FD'Class'
                (AWS.Net.Connect (AWS.URL.Server_Name (Connect_URL),
                                  AWS.URL.Port (Connect_URL),
                                  AWS.URL.Security (Connect_URL))),
              Retry       => Create.Retry,
              Cookie      => Null_Unbounded_String,
              SOAPAction  => Set (SOAPAction),
              Persistent  => Persistent);
   end Create;

   ---------
   -- Get --
   ---------

   procedure Get
     (Connection : in out HTTP_Connection;
      Result     :    out Response.Data;
      URI        : in     String          := No_Data)
   is
      Try_Count : Natural := Connection.Retry;
   begin

      loop
         begin

            Open_Send_Common_Header (Connection, "GET", URI);

            Sockets.New_Line (Connection.Socket.all);

            Get_Response (Connection, Result);

            return;

         exception
            when Sockets.Connection_Closed | Constraint_Error =>

               if Try_Count = 0 then
                  raise;
               end if;

               Try_Count := Try_Count - 1;
               Disconnect (Connection);
         end;
      end loop;

   end Get;

   ----------
   -- Head --
   ----------

   procedure Head
     (Connection : in out HTTP_Connection;
      Result     :    out Response.Data;
      URI        : in     String := No_Data)
   is
      Try_Count : Natural := Connection.Retry;
   begin

      loop
         begin

            Open_Send_Common_Header (Connection, "HEAD", URI);

            Sockets.New_Line (Connection.Socket.all);

            Get_Response (Connection, Result, Get_Body => False);

            return;

         exception
            when Sockets.Connection_Closed | Constraint_Error =>

               if Try_Count = 0 then
                  raise;
               end if;

               Try_Count := Try_Count - 1;
               Disconnect (Connection);
         end;
      end loop;
   end Head;

   ----------
   -- Post --
   ----------

   procedure Post
     (Connection : in out HTTP_Connection;
      Result     :    out Response.Data;
      Data       : in     Streams.Stream_Element_Array;
      URI        : in     String := No_Data)
   is
      No_Data : Unbounded_String renames Null_Unbounded_String;

      Try_Count : Natural := Connection.Retry;
   begin

      loop
         begin

            Open_Send_Common_Header (Connection, "POST", URI);

            declare
               Sock : Sockets.Socket_FD'Class := Connection.Socket.all;
            begin

               if Connection.SOAPAction = No_Data then
                  Sockets.Put_Line
                    (Sock,
                     Messages.Content_Type (MIME.Appl_Form_Data));

               else
                  Sockets.Put_Line
                    (Sock,
                     Messages.Content_Type (MIME.Text_XML));
               end if;

               --  Send message Content_Length

               Sockets.Put_Line (Sock, Messages.Content_Length (Data'Length));

               Sockets.New_Line (Sock);

               --  Send message body

               Sockets.Send (Sock, Data);
            end;

            --  Get answer from server

            Get_Response (Connection, Result);

            return;

         exception

            when Sockets.Connection_Closed | Constraint_Error =>

               if Try_Count = 0 then
                  raise;
               end if;

               Try_Count := Try_Count - 1;
               Disconnect (Connection);
         end;
      end loop;
   end Post;

   procedure Post
     (Connection : in out HTTP_Connection;
      Result     :    out Response.Data;
      Data       : in     String;
      URI        : in     String := No_Data) is
   begin
      Post (Connection, Result,
            Translator.To_Stream_Element_Array (Data), URI);
   end Post;

   ---------
   -- Put --
   ---------

   procedure Put
     (Connection : in out HTTP_Connection;
      Result     :    out Response.Data;
      Data       : in     String;
      URI        : in     String          := No_Data)
   is
      Sock : Sockets.Socket_FD'Class := Connection.Socket.all;
      CT       : Unbounded_String;
      CT_Len   : Natural;
      TE       : Unbounded_String;
      Status   : Messages.Status_Code;
      Location : Unbounded_String;
      Connect  : Unbounded_String;

      Try_Count : Natural := Connection.Retry;

   begin

      loop

         begin
            Open_Send_Common_Header (Connection, "PUT", URI);

            --  send message Content_Length

            Sockets.Put_Line (Sock, Messages.Content_Length (Data'Length));

            Sockets.New_Line (Sock);

            --  send message body

            Sockets.Put_Line (Sock, Data);

            --  get answer from server

            Parse_Header (Sock, Status, CT_Len, CT, TE,
                          Location, Connect, Connection.Cookie);

            if Messages.Is_Match (To_String (Connect), "close") then
               Disconnect (Connection);
            end if;

            Result := Response.Acknowledge (Status);

            return;

         exception
            when Sockets.Connection_Closed | Constraint_Error =>

               if Try_Count = 0 then
                  raise;
               end if;

               Try_Count := Try_Count - 1;
               Disconnect (Connection);
         end;
      end loop;
   end Put;

end AWS.Client;
