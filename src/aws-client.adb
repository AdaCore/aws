------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                            Copyright (C) 2000                            --
--                      Dmitriy Anisimov - Pascal Obry                      --
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
with Interfaces.C;

with Sockets.Thin;

with AWS.Messages;
with AWS.Translater;
with AWS.URL;
with AWS.Net;

package body AWS.Client is

   use Ada;
   use Ada.Strings.Unbounded;

   End_Section : constant String := "";

   function Get_Response (Socket : in Sockets.Socket_FD'Class)
                         return Response.Data;
   --  Receives response from server for GET and POST commands

   function Init_Connection
     (Method     : in     String;
      URL        : in     String;
      User       : in     String            := No_Data;
      Pwd        : in     String            := No_Data;
      Proxy      : in     String            := No_Data;
      Proxy_User : in     String            := No_Data;
      Proxy_Pwd  : in     String            := No_Data)
   return Sockets.Socket_FD'Class;

   --  send a header to the server eventually going through a proxy server
   --  with authentification.

   procedure Parse_Header
     (Sock              : in     Sockets.Socket_FD'Class;
      Status            :    out Messages.Status_Code;
      Content_Length    :    out Natural;
      Content_Type      :    out Unbounded_String;
      Transfer_Encoding :    out Unbounded_String;
      Location          :    out Unbounded_String);
   --  Read server answer and set corresponding variable with the value
   --  read. Most of the field are ignored right now.

   ---------------------
   -- Init_Connection --
   ---------------------

   function Init_Connection
     (Method     : in     String;
      URL        : in     String;
      User       : in     String            := No_Data;
      Pwd        : in     String            := No_Data;
      Proxy      : in     String            := No_Data;
      Proxy_User : in     String            := No_Data;
      Proxy_Pwd  : in     String            := No_Data)
     return Sockets.Socket_FD'Class
   is
      function Get_Host_Name return String;
      --  returns the local hostname

      function Open_Socket return Sockets.Socket_FD'Class;
      --  Opens socket (SSL if need) with server

      -------------------
      -- Get_Host_Name --
      -------------------

      function Get_Host_Name return String is
         Buffer : Interfaces.C.char_array (1 .. 100);
         Res    : Interfaces.C.int;
      begin
         Res := Sockets.Thin.C_Gethostname (Buffer (1)'Address, 100);
         return Interfaces.C.To_Ada (Buffer);
      end Get_Host_Name;

      -----------------
      -- Open_Socket --
      -----------------

      function Open_Socket return Sockets.Socket_FD'Class is
      begin
         if Proxy = No_Data then
            declare
               URL_Data : AWS.URL.Object := AWS.URL.Parse (URL);
               Sock     : Sockets.Socket_FD'Class :=
                 AWS.Net.Connect (AWS.URL.Server_Name (URL_Data),
                                  AWS.URL.Port (URL_Data),
                                  AWS.URL.Security (URL_Data));
            begin
               Sockets.Put_Line (Sock, Method & ' '
                                 & AWS.URL.URI (URL_Data)
                                 & ' ' & HTTP_Version);
               Sockets.Put_Line (Sock, "Connection: Keep-Alive");
               return Sock;
            end;
         else
            declare
               Proxy_Data  : AWS.URL.Object := AWS.URL.Parse (Proxy);
               Sock        : Sockets.Socket_FD'Class :=
                 AWS.Net.Connect (AWS.URL.Server_Name (Proxy_Data),
                                  AWS.URL.Port (Proxy_Data),
                                  AWS.URL.Security (Proxy_Data));
            begin
               Sockets.Put_Line (Sock,
                                 Method & ' ' & URL & ' ' & HTTP_Version);
               Sockets.Put_Line (Sock, "Proxy-Connection: Keep-Alive");
               return Sock;
            end;
         end if;
      end Open_Socket;

      --  Connect to server

      Sock : Sockets.Socket_FD'Class := Open_Socket;

   begin

      Sockets.Put_Line (Sock, "Accept: text/html, */*");
      Sockets.Put_Line (Sock, "Accept-Language: fr, us");
      Sockets.Put_Line (Sock, "User-Agent: AWS/v" & Version);
      Sockets.Put_Line (Sock, "Host: " & Get_Host_Name);

      if User /= No_Data and then Pwd /= No_Data then
         Sockets.Put_Line
           (Sock, "Authorization: Basic " &
            AWS.Translater.Base64_Encode (User & ':' & Pwd));
      end if;

      if Proxy_User /= No_Data and then Proxy_Pwd /= No_Data then
         Sockets.Put_Line
           (Sock, "Proxy-Authorization: Basic " &
            AWS.Translater.Base64_Encode (Proxy_User & ':' & Proxy_Pwd));
      end if;

      return Sock;
   end Init_Connection;

   ------------------
   -- Get_Response --
   ------------------

   function Get_Response (Socket : in Sockets.Socket_FD'Class)
                         return Response.Data
   is

      function Read_Chunk return Streams.Stream_Element_Array;
      --  read a chunk object from the stream

      function Read_Message return String;
      --  read a textual message from the socket for which there is no known
      --  length.

      Sock : Sockets.Socket_FD'Class := Socket;

      CT       : Unbounded_String;
      CT_Len   : Natural := 0;
      TE       : Unbounded_String;
      Location : Unbounded_String;
      Status   : Messages.Status_Code;
      Message  : Unbounded_String;

      ----------------
      -- Read_Chunk --
      ----------------

      function Read_Chunk return Streams.Stream_Element_Array is

         use type Streams.Stream_Element_Array;
         use type Streams.Stream_Element_Offset;

         procedure Skip_Line;
         --  skip a line on the socket

         --  read the chunk size that is an hex number

         Size : Streams.Stream_Element_Offset
           := Streams.Stream_Element_Offset'Value
           ("16#" & Sockets.Get_Line (Sock) & '#');

         Elements : Streams.Stream_Element_Array (1 .. Size);

         procedure Skip_Line is
            D : constant String := Sockets.Get_Line (Sock);
         begin
            null;
         end Skip_Line;

      begin
         if Size = 0 then
            Skip_Line;
            return Elements;
         else
            Sockets.Receive (Sock, Elements);
            Skip_Line;
            return Elements & Read_Chunk;
         end if;
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


   begin

      Parse_Header (Sock, Status, CT_Len, CT, TE, Location);

      --  check for special status

      if Status = Messages.S301 then
         --  moved permanently

         return Response.Build (To_String (CT), To_String (Location), Status);
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

         return Response.Build (To_String (CT), Read_Chunk, Status);

      else

         if CT_Len = 0 and then CT = "text/html" then
            --  here we do not know the message body length, but this is a
            --  textual data, read it as a string.

            return Response.Build (To_String (CT), Read_Message, Status);

         else

            declare
               Elements : Streams.Stream_Element_Array
                 (1 .. Streams.Stream_Element_Offset (CT_Len));
            begin
               Sockets.Receive (Sock, Elements);
               Sockets.Shutdown (Sock);

               if CT = "text/html" then

                  --  if the content is textual info put it in a string

                  for K in Elements'Range loop
                     Append (Message, Character'Val (Natural (Elements (K))));
                  end loop;

                  return Response.Build (To_String (CT),
                                         To_String (Message),
                                         Status);
               else

                  --  this is some kind of binary data.

                  return Response.Build (To_String (CT), Elements, Status);
               end if;
            end;
         end if;
      end if;
   end Get_Response;

   ---------
   -- Get --
   ---------

   function Get (URL        : in String;
                 User       : in String := No_Data;
                 Pwd        : in String := No_Data;
                 Proxy      : in String := No_Data;
                 Proxy_User : in String := No_Data;
                 Proxy_Pwd  : in String := No_Data) return Response.Data
   is

      Sock : Sockets.Socket_FD'Class :=
        Init_Connection ("GET", URL, User, Pwd, Proxy, Proxy_User, Proxy_Pwd);

   begin

      Sockets.New_Line (Sock);

      return Get_Response (Sock);
   end Get;

   ------------------
   -- Parse_Header --
   ------------------

   procedure Parse_Header
     (Sock              : in     Sockets.Socket_FD'Class;
      Status            :    out Messages.Status_Code;
      Content_Length    :    out Natural;
      Content_Type      :    out Unbounded_String;
      Transfer_Encoding :    out Unbounded_String;
      Location          :    out Unbounded_String) is
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
                 (Line (Messages.Content_Type_Token'Last + 1 .. Line'Last));

            elsif Messages.Is_Match (Line, Messages.Content_Length_Token) then
               Content_Length := Natural'Value
                 (Line (Messages.Content_Length_Range'Last + 1 .. Line'Last));

            elsif Messages.Is_Match (Line, Messages.Location_Token) then
               Location := To_Unbounded_String
                 (Line (Messages.Location_Token'Last + 1 .. Line'Last));

            elsif Messages.Is_Match (Line,
                                     Messages.Transfer_Encoding_Token)
            then
               Transfer_Encoding := To_Unbounded_String
                 (Line (Messages.Transfer_Encoding_Range'Last + 1
                        .. Line'Last));

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

   function Put (URL        : in String;
                 Data       : in String;
                 User       : in String := No_Data;
                 Pwd        : in String := No_Data;
                 Proxy      : in String := No_Data;
                 Proxy_User : in String := No_Data;
                 Proxy_Pwd  : in String := No_Data) return Response.Data
   is
      Sock     : Sockets.Socket_FD'Class :=
        Init_Connection ("PUT", URL, User, Pwd, Proxy, Proxy_User, Proxy_Pwd);
      CT       : Unbounded_String;
      CT_Len   : Natural;
      TE       : Unbounded_String;
      Status   : Messages.Status_Code;
      Location : Unbounded_String;
   begin

      --  send message Content_Length

      Sockets.Put_Line (Sock, Messages.Content_Length (Data'Length));

      Sockets.New_Line (Sock);

      --  send message body

      Sockets.Put_Line (Sock, Data);

      --  get answer from server

      Parse_Header (Sock, Status, CT_Len, CT, TE, Location);

      return Response.Acknowledge (Status);
   end Put;

   ----------
   -- Post --
   ----------

   function Post (URL        : in String;
                  Data       : in String;
                  User       : in String := No_Data;
                  Pwd        : in String := No_Data;
                  Proxy      : in String := No_Data;
                  Proxy_User : in String := No_Data;
                  Proxy_Pwd  : in String := No_Data) return Response.Data
   is
      Sock : Sockets.Socket_FD'Class :=
        Init_Connection ("POST", URL, User, Pwd, Proxy, Proxy_User, Proxy_Pwd);

   begin

      Sockets.Put_Line (Sock, Messages.Content_Type (Messages.Form_Data));

      --  send message Content_Length
      Sockets.Put_Line (Sock, Messages.Content_Length (Data'Length));

      Sockets.New_Line (Sock);

      --  send message body

      Sockets.Put_Line (Sock, Data);

      --  get answer from server

      return Get_Response (Sock);

   end Post;

end AWS.Client;
