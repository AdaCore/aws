------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2000-2001                          --
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

with Ada.Exceptions;
with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Ada.Streams;
with Ada.Unchecked_Deallocation;

with GNAT.Table;
with Sockets;

with AWS.Messages;
with AWS.MIME;
with AWS.Translator;
with AWS.Net;

package body AWS.Client is

   use Ada;
   use Ada.Strings.Unbounded;

   Debug_On    : Boolean := False;

   End_Section : constant String := "";

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

   procedure Set_Phase
     (Connection : in out HTTP_Connection;
      Phase      : in     Client_Phase);
   pragma Inline (Set_Phase);
   --  Set the phase for the connection. This will activate the Send and
   --  Receive timeouts of the cleaner task if needed.

   procedure Send_Header
     (Sock : in Sockets.Socket_FD'Class;
      Data : in String);
   pragma Inline (Send_Header);
   --  Send header Data to socket and call Debug_Message.

   -------------------
   -- Build_Cleaner --
   -------------------

   function Build_Cleaner
     (Connection : access HTTP_Connection)
     return Cleaner_Access is
   begin
      if Connection.With_Timeouts then
         return new Cleaner_Task (Connection);

      else
         return null;
      end if;
   end Build_Cleaner;

   ------------------
   -- Cleaner_Task --
   ------------------

   task body Cleaner_Task is
      P : Client_Phase;
      W : Duration;
   begin
      Phase_Loop : loop

         --  Wait for the job to be done

         select
            accept Send do
               W := Duration (Connection.Timeouts.Send);
               P := Send;
            end Send;
         or
            accept Receive do
               W := Duration (Connection.Timeouts.Receive);
               P := Receive;
            end Receive;

         or
            accept Stop;
            exit Phase_Loop;

         or
            accept Next_Phase;
            exit Phase_Loop;

         end select;

         --  Delay for the right time

         select
            accept Stop;
            exit Phase_Loop;
         or
            accept Next_Phase;
         or
            delay W;
         end select;

         --  Still in the same phase after the delay, just close the socket
         --  now.

         if Connection.Current_Phase = P then
            Sockets.Shutdown (Connection.Socket.all);
            Connection.Socket := null;
         end if;

      end loop Phase_Loop;

   exception
      when E : others =>
         Text_IO.Put_Line (Exceptions.Exception_Information (E));
   end Cleaner_Task;

   -----------
   -- Close --
   -----------

   procedure Close (Connection : in out HTTP_Connection) is

      procedure Free is new Ada.Unchecked_Deallocation
        (Sockets.Socket_FD'Class, Socket_Access);

      procedure Free is new Ada.Unchecked_Deallocation
        (Cleaner_Task, Cleaner_Access);

   begin
      Connection.Current_Phase := Stopped;

      if not (Connection.Cleaner = null) then

         begin
            --  We don't want to fail here, we really want to free the cleaner
            --  object.
            if not Connection.Cleaner'Terminated then
               Connection.Cleaner.Stop;
            end if;
         exception
            when others =>
               null;
         end;

         while not Connection.Cleaner'Terminated loop
            delay 0.01;
         end loop;

         Free (Connection.Cleaner);

      end if;

      if not (Connection.Socket = null) then
         Sockets.Shutdown (Connection.Socket.all);
         Free (Connection.Socket);
      end if;
   end Close;

   ------------
   -- Create --
   ------------

   procedure Create
     (Connection : in out HTTP_Connection;
      Host       : in     String;
      User       : in     String          := No_Data;
      Pwd        : in     String          := No_Data;
      Proxy      : in     String          := No_Data;
      Proxy_User : in     String          := No_Data;
      Proxy_Pwd  : in     String          := No_Data;
      Retry      : in     Natural         := Retry_Default;
      SOAPAction : in     String          := No_Data;
      Persistent : in     Boolean         := True;
      Timeouts   : in     Timeouts_Values := No_Timeout)
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

      Connection.Host            := To_Unbounded_String (Host);
      Connection.Host_URL        := Host_URL;
      Connection.Connect_URL     := Connect_URL;
      Connection.User            := Set (User);
      Connection.Pwd             := Set (Pwd);
      Connection.Proxy           := Set (Proxy);
      Connection.Proxy_URL       := Proxy_URL;
      Connection.Proxy_User      := Set (Proxy_User);
      Connection.Proxy_Pwd       := Set (Proxy_Pwd);
      Connection.Opened          := True;
      Connection.Socket          := new Sockets.Socket_FD'Class'
        (AWS.Net.Connect (AWS.URL.Host (Connect_URL),
                          AWS.URL.Port (Connect_URL),
                          AWS.URL.Security (Connect_URL)));
      Connection.Retry           := Create.Retry;
      Connection.Cookie          := Null_Unbounded_String;
      Connection.SOAPAction      := Set (SOAPAction);
      Connection.Persistent      := Persistent;
      Connection.Current_Phase   := Not_Monitored;

      if Connection.With_Timeouts then
         Connection.Timeouts     := Timeouts;
      else
         Connection.Timeouts     := No_Timeout;
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

   ----------------
   -- Disconnect --
   ----------------

   procedure Disconnect (Connection : in out HTTP_Connection) is
   begin
      Sockets.Shutdown (Connection.Socket.all);
      Connection.Opened := False;
   end Disconnect;

   ---------
   -- Get --
   ---------

   function Get
     (URL        : in String;
      User       : in String          := No_Data;
      Pwd        : in String          := No_Data;
      Proxy      : in String          := No_Data;
      Proxy_User : in String          := No_Data;
      Proxy_Pwd  : in String          := No_Data;
      Timeouts   : in Timeouts_Values := No_Timeout)
     return Response.Data
   is

      Connection : HTTP_Connection (Timeouts /= No_Timeout);
      Result     : Response.Data;

   begin
      Create (Connection,
              URL, User, Pwd, Proxy, Proxy_User, Proxy_Pwd,
              Persistent => False,
              Timeouts   => Timeouts);

      Get (Connection, Result);

      Close (Connection);
      return Result;

   exception
      when others =>
         Close (Connection);
         raise;
   end Get;

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
                  Close (Connection);
                  Result := Response.Build
                    (MIME.Text_HTML, "Get Timeout", Messages.S408);
                  exit;
               end if;

               Try_Count := Try_Count - 1;
               Disconnect (Connection);
         end;
      end loop;
   end Get;

   ------------------
   -- Get_Response --
   ------------------

   procedure Get_Response
     (Connection : in out HTTP_Connection;
      Result     :    out Response.Data;
      Get_Body   : in     Boolean         := True)
   is

      function Read_Chunk return Streams.Stream_Element_Array;
      --  Read a chunk object from the stream

      function Read_Binary_Message
        (Len : in Positive)
        return Streams.Stream_Element_Array;
      pragma Inline (Read_Binary_Message);
      --  Read a binary message of Len bytes from the socket.

      function Read_Message return String;
      --  Read a textual message from the socket for which there is no known
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

      ----------------
      -- Disconnect --
      ----------------

      procedure Disconnect is
      begin
         if Messages.Is_Match (To_String (Connect), "close") then
            Disconnect (Connection);
         end if;
      end Disconnect;

      -------------------------
      -- Read_Binary_Message --
      -------------------------

      function Read_Binary_Message
        (Len : in Positive)
        return Streams.Stream_Element_Array
      is
         Elements : Streams.Stream_Element_Array
           (1 .. Streams.Stream_Element_Offset (Len));
      begin
         Sockets.Receive (Sock, Elements);
         return Elements;
      end Read_Binary_Message;

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
         --  We don't know the message body length, so read the socket until
         --  it is closed by the server. At this time an exception will be
         --  raised as we are trying to read the socket.

         loop
            declare
               Part : constant String := Sockets.Get (Sock);
            begin
               Append (Results, Part);
            end;
         end loop;

         return To_String (Results);

      exception
         when others =>
            return To_String (Results);
      end Read_Message;

      use type Messages.Status_Code;

   begin
      Set_Phase (Connection, Receive);

      Parse_Header
        (Sock, Status, CT_Len, CT, TE, Location, Connect, Connection.Cookie);

      --  check for special status

      if Status = Messages.S301 then
         --  moved permanently

         Result := Response.Build
           (To_String (CT), To_String (Location), Status);

         Disconnect;
         Set_Phase (Connection, Not_Monitored);
         return;

      elsif Status = Messages.S404 then

         if CT_Len = 0 then
            Result := Response.Build
              (MIME.Text_HTML, "(404) not found", Status);
         else
            Result := Response.Build
              (MIME.Text_HTML,
               Translator.To_String (Read_Binary_Message (CT_Len)),
               Status);
         end if;

         Disconnect;
         Set_Phase (Connection, Not_Monitored);
         return;

      end if;

      if not Get_Body then
         Result := Response.Build (To_String (CT), "", Status);
         Disconnect;
         Set_Phase (Connection, Not_Monitored);
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

         declare
            CT  : constant String := To_String (Get_Response.CT);
         begin
            if MIME.Is_Text (CT) then
               --  This is a textual chunked encoded body
               Result := Response.Build
                 (CT, Translator.To_String (Read_Chunk), Status);

            else
               --  This is really some kind of binary data
               Result := Response.Build (CT, Read_Chunk, Status);
            end if;
         end;

      else
         if CT_Len = 0 then
            if MIME.Is_Text (To_String (CT)) then
               --  Here we do not know the message body length, but this is a
               --  textual data, read it as a string.

               Result := Response.Build
                 (To_String (CT), Read_Message, Status);

            else
               declare

                  package Stream_Element_Table is new GNAT.Table
                    (Streams.Stream_Element, Positive, 1, 30_000, 10_000);

                  procedure Add (B : in Streams.Stream_Element_Array);
                  --  Add B to Data

                  procedure Read_Until_Close;
                  --  Read data on socket, stop when the socket is closed.

                  ---------
                  -- Add --
                  ---------

                  procedure Add (B : in Streams.Stream_Element_Array) is
                  begin
                     for K in B'Range loop
                        Stream_Element_Table.Increment_Last;
                        Stream_Element_Table.Table
                          (Stream_Element_Table.Last) := B (K);
                        --  ??? in GNAT 3.15 we will use:
                        --  Stream_Element_Table.Append (B (K));
                     end loop;
                  end Add;

                  ----------------------
                  -- Read_Until_Close --
                  ----------------------

                  procedure Read_Until_Close is
                  begin
                     loop
                        declare
                           Data : constant Streams.Stream_Element_Array
                             := Sockets.Receive (Sock);
                        begin
                           Add (Data);
                        end;
                     end loop;
                  exception
                     when others =>
                        null;
                  end Read_Until_Close;

               begin
                  Read_Until_Close;

                  Result := Response.Build
                    (To_String (CT),
                     Streams.Stream_Element_Array
                       (Stream_Element_Table.Table.all),
                     Status);
               end;
            end if;

         else

            declare
               Elements : Streams.Stream_Element_Array
                 := Read_Binary_Message (CT_Len);
            begin
               if MIME.Is_Text (To_String (CT)) then
                  Result := Response.Build
                    (To_String (CT), Translator.To_String (Elements), Status);

               else
                  --  This is some kind of binary data.

                  Result := Response.Build (To_String (CT), Elements, Status);
               end if;
            end;
         end if;
      end if;

      Disconnect;

      Set_Phase (Connection, Not_Monitored);
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

      Connection : HTTP_Connection (Timeouts /= No_Timeout);
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
                  Close (Connection);
                  Result := Response.Build
                    (MIME.Text_HTML, "Head Timeout", Messages.S408);
                  exit;
               end if;

               Try_Count := Try_Count - 1;
               Disconnect (Connection);
         end;
      end loop;
   end Head;

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

      Host_Address : constant String :=
        AWS.URL.Host (Connection.Host_URL)
        & Port_Not_Default (AWS.URL.Port (Connection.Host_URL));

   begin

      --  Open socket if needed.

      if not Connection.Opened then
         Sock := AWS.Net.Connect
           (AWS.URL.Host (Connection.Connect_URL),
            AWS.URL.Port (Connection.Connect_URL),
            AWS.URL.Security (Connection.Connect_URL));

         Connection.Socket.all := Sock;
         Connection.Opened     := True;
      end if;

      Set_Phase (Connection, Send);

      --  Header command.

      if Connection.Proxy = No_Data then

         if URI = "" then
            Send_Header (Sock, Method & ' '
                         & AWS.URL.Pathname (Connection.Host_URL, False)
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

               Send_Header (Sock, Method & ' ' & E_URI & ' ' & HTTP_Version);
            end;
         end if;

         Send_Header (Sock, Messages.Connection (Persistence));

      else
         if URI = "" then
            Send_Header (Sock, Method & ' '
                         & To_String (Connection.Host)
                         & ' ' & HTTP_Version);
         else
            Send_Header
              (Sock, Method & ' '
               & HTTP_Prefix (AWS.URL.Security (Connection.Host_URL))
               & Host_Address & URI
               & ' ' & HTTP_Version);
         end if;

         Send_Header (Sock, Messages.Proxy_Connection (Persistence));

      end if;

      --  Cookie

      if Connection.Cookie /= No_Data then
         Send_Header
           (Sock, Messages.Cookie_Token & To_String (Connection.Cookie));
      end if;

      Send_Header (Sock, Messages.Host (Host_Address));
      Send_Header (Sock, Messages.Accept_Type ("text/html, */*"));
      Send_Header (Sock, Messages.Accept_Language ("fr, us"));
      Send_Header
        (Sock, Messages.User_Agent ("AWS (Ada Web Server) v" & Version));

      --  User Authentification

      if Connection.User /= No_Data
        and then Connection.Pwd /= No_Data
      then
         Send_Header
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
         Send_Header
           (Sock,
            Messages.Proxy_Authorization
            ("Basic",
             AWS.Translator.Base64_Encode
             (To_String (Connection.Proxy_User)
              & ':' & To_String (Connection.Proxy_Pwd))));
      end if;

      --  SOAP header

      if Connection.SOAPAction /= No_Data then
         Send_Header
           (Sock, Messages.SOAPAction (To_String (Connection.SOAPAction)));
      end if;

      Set_Phase (Connection, Not_Monitored);
   end Open_Send_Common_Header;

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
            Debug_Message ("< ", Line);

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
      use Streams;
   begin
      return Post (URL, Translator.To_Stream_Element_Array (Data),
                   User, Pwd, Proxy, Proxy_User, Proxy_Pwd);
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
      Connection : HTTP_Connection (Timeouts /= No_Timeout);
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
                  Send_Header
                    (Sock,
                     Messages.Content_Type (MIME.Appl_Form_Data));

               else
                  Send_Header
                    (Sock,
                     Messages.Content_Type (MIME.Text_XML));
               end if;

               --  Send message Content_Length

               Send_Header (Sock, Messages.Content_Length (Data'Length));

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
                  Close (Connection);
                  Result := Response.Build
                    (MIME.Text_HTML, "Post Timeout", Messages.S408);
                  exit;
               end if;

               Try_Count := Try_Count - 1;
               Disconnect (Connection);
         end;
      end loop;
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
      Post (Connection, Result,
            Translator.To_Stream_Element_Array (Data), URI);
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

      Connection : HTTP_Connection (Timeouts /= No_Timeout);
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

            --  Send message Content_Length

            Send_Header (Sock, Messages.Content_Length (Data'Length));

            Sockets.New_Line (Sock);

            --  Send message body

            Sockets.Put_Line (Sock, Data);

            --  Get answer from server

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
                  Close (Connection);
                  Result := Response.Build
                    (MIME.Text_HTML, "Put Timeout", Messages.S408);
                  exit;
               end if;

               Try_Count := Try_Count - 1;
               Disconnect (Connection);
         end;
      end loop;
   end Put;

   -----------------
   -- Send_Header --
   -----------------

   procedure Send_Header
     (Sock : in Sockets.Socket_FD'Class;
      Data : in String) is
   begin
      Sockets.Put_Line (Sock, Data);
      Debug_Message ("> ", Data);
   end Send_Header;

   ---------------
   -- Set_Debug --
   ---------------

   procedure Set_Debug (On : in Boolean) is
   begin
      Debug_On := On;
   end Set_Debug;

   ---------------
   -- Set_Phase --
   ---------------

   procedure Set_Phase
     (Connection : in out HTTP_Connection;
      Phase      : in     Client_Phase) is
   begin
      Connection.Current_Phase := Phase;

      if Phase = Send and then Connection.Timeouts.Send /= 0 then
         Connection.Cleaner.Send;

      elsif Phase = Receive and then Connection.Timeouts.Receive /= 0 then
         Connection.Cleaner.Receive;

      elsif Phase = Not_Monitored and then
        (Connection.Timeouts.Send /= 0
         or else Connection.Timeouts.Receive /= 0)
      then
         Connection.Cleaner.Next_Phase;

      end if;
   end Set_Phase;

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
      Connection : HTTP_Connection (Timeouts /= No_Timeout);
      Result     : Response.Data;

   begin
      Create (Connection,
              URL, User, Pwd, Proxy, Proxy_User, Proxy_Pwd,
              SOAPAction => SOAPAction,
              Persistent => False,
              Timeouts   => Timeouts);

      Post (Connection, Result, Data);
      Close (Connection);
      return Result;
   exception
      when others =>
         Close (Connection);
         return Response.Build (MIME.Text_HTML, "Timeouts", Messages.S408);
   end SOAP_Post;

end AWS.Client;
