------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                            Copyright (C) 2000                            --
--                               Pascal Obry                                --
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

with Sockets;

with AWS.Messages;
with AWS.Translater;
with AWS.URL;

package body AWS.Client is

   use Ada;
   use Ada.Strings.Unbounded;

   End_Section : constant String := "";

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

      function Read_Chunk return Streams.Stream_Element_Array;
      --  read a chunk object from the stream

      Sock    : Sockets.Socket_FD;
      CT      : Unbounded_String;
      CT_Len  : Natural;
      TE      : Unbounded_String;
      Status  : Messages.Status_Code;
      Message : Unbounded_String;
      Proxy_Data, URL_Data : AWS.URL.Object;

      function Read_Chunk return Streams.Stream_Element_Array is

         use type Streams.Stream_Element_Array;
         use type Streams.Stream_Element_Offset;

         procedure Skip_Line;
         --  skip a line on the socket

         --  read the chunk size that is an hex number

         Size     : Streams.Stream_Element_Offset
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

   begin

      URL_Data   := AWS.URL.Parse (URL);
      Proxy_Data := AWS.URL.Parse (Proxy);

      -- Connect to server

      if Proxy = No_Data then
         Sock := Sockets.Socket (Sockets.AF_INET, Sockets.SOCK_STREAM);

         Sockets.Connect (Sock,
                          AWS.URL.Server_Name (URL_Data),
                          AWS.URL.Port (URL_Data));

         Sockets.Put_Line (Sock, "GET "
                           & AWS.URL.URI (URL_Data)
                           & ' ' & HTTP_Version);
         Sockets.Put_Line (Sock, "Connection: Keep-Alive");

      else
         Sock := Sockets.Socket (Sockets.AF_INET, Sockets.SOCK_STREAM);

         Sockets.Connect (Sock,
                          AWS.URL.Server_Name (Proxy_Data),
                          AWS.URL.Port (Proxy_Data));

         Sockets.Put_Line (Sock, "GET " & URL & ' ' & HTTP_Version);
         Sockets.Put_Line (Sock, "Proxy-Connection: Keep-Alive");
      end if;

      Sockets.Put_Line (Sock, "Accept: text/html, */*");
      Sockets.Put_Line (Sock, "Accept-Language: fr, us");
      Sockets.Put_Line (Sock, "User-Agent: AWS/v" & Version);
      Sockets.Put_Line (Sock, "Host: whatever_for_now");

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

      Sockets.New_Line (Sock);

      Parse_Header : loop
         declare
            Line : constant String := Sockets.Get_Line (Sock);
         begin
            if Line = End_Section then
               exit Parse_Header;

            elsif Messages.Is_Match (Line, Messages.HTTP_Token) then
               Status := Messages.Status_Code'Value
                 ('S' & Line (Messages.HTTP_Token'Last + 5
                              .. Messages.HTTP_Token'Last + 7));

            elsif Messages.Is_Match (Line, Messages.Content_Type_Token) then
               CT := To_Unbounded_String
                 (Line (Messages.Content_Type_Token'Last + 1 .. Line'Last));

            elsif Messages.Is_Match (Line, Messages.Content_Length_Token) then
               CT_Len := Natural'Value
                 (Line (Messages.Content_Length_Range'Last + 1 .. Line'Last));

            elsif Messages.Is_Match (Line,
                                     Messages.Transfer_Encoding_Token)
            then
               TE := To_Unbounded_String
                 (Line (Messages.Transfer_Encoding_Range'Last + 1
                        .. Line'Last));

            else
               --  everything else is ignore right now
               null;
            end if;
         end;
      end loop Parse_Header;

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
            Elements : Streams.Stream_Element_Array := Read_Chunk;
         begin
            return Response.Build (To_String (CT),
                                   Elements,
                                   Status);
         end;

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

               return Response.Build (To_String (CT),
                                      Elements,
                                      Status);
            end if;
         end;
      end if;

   exception
      when others =>
         raise URL_Error;
   end Get;

end AWS.Client;
