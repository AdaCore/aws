------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                      Copyright (C) 2016, AdaCore                         --
--                                                                          --
--  This is free software;  you can redistribute it  and/or modify it       --
--  under terms of the  GNU General Public License as published  by the     --
--  Free Software  Foundation;  either version 3,  or (at your option) any  --
--  later version.  This software is distributed in the hope  that it will  --
--  be useful, but WITHOUT ANY WARRANTY;  without even the implied warranty --
--  of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU     --
--  General Public License for  more details.                               --
--                                                                          --
--  You should have  received  a copy of the GNU General  Public  License   --
--  distributed  with  this  software;   see  file COPYING3.  If not, go    --
--  to http://www.gnu.org/licenses for a complete copy of the license.      --
------------------------------------------------------------------------------

with Ada.Calendar;
with Ada.Streams;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with AWS.Client.HTTP_Utils;
with AWS.Headers.Set;
with AWS.Messages;
with AWS.MIME;
with AWS.Net.Buffered;
with AWS.Net.Std;
with AWS.Response;
with AWS.Server.Status;
with AWS.Status;
with AWS.Translator;
with AWS.Utils;

with ZLib;

procedure Put_Chunked_Compressed is

   use Ada;
   use Ada.Streams;
   use Ada.Strings.Unbounded;
   use AWS;

   CRLF    : constant String := ASCII.CR & ASCII.LF;

   Message : constant String :=
               "This is a message to be sent " & CRLF
               & "by AWS as a chunked message non compressed" & CRLF
               & "and then as a compressed chunked message" & Crlf
               & "this is to test the Transfer-Encoding and"
               & " Content-Encoding used together";

   --------
   -- CB --
   --------

   function CB (Request : Status.Data) return Response.Data is
   begin
      if not Status.Is_Body_Uploaded (Request) then
         Server.Get_Message_Body;
      end if;

      declare
         Mes : constant String := To_String (Status.Binary_Data (Request));
      begin
         if Mes = Message then
            Text_IO.Put_Line ("Message OK");
         else
            Text_IO.Put_Line ("NOK: " & Mes);
         end if;
         return Response.Build (MIME.Text_HTML, Mes);
      end;
   end CB;

   ----------
   -- Send --
   ----------

   procedure Send
     (Sock    : Net.Socket_Type'Class;
      Headers : AWS.Headers.List;
      Message : Stream_Element_Array) is
   begin
      --  Send common headers

      Client.HTTP_Utils.Send_Header
        (Sock, "PUT /put HTTP/1.1");
      Client.HTTP_Utils.Send_Header
        (Sock,
         Messages.Host ("127.0.0.1:" & Utils.Image (Net.Get_Port (Sock))));
      Client.HTTP_Utils.Send_Header
        (Sock, Messages.User_Agent ("AWS testing"));

      Client.HTTP_Utils.Send_Header
        (Sock, Messages.Transfer_Encoding ("chunked"));

      --  Send message headers if any

      AWS.Headers.Send_Header (Sock, Headers);
      Net.Buffered.New_Line (Sock);

      --  Send message itself

      Net.Buffered.Write (Sock, Message);
      Net.Buffered.Flush (Sock);

      --  Get response, read until CRLF

      loop
         exit when Net.Buffered.Get_Line (Sock) = "";
      end loop;
   end Send;

   ---------------------
   -- Transfer_Encode --
   ---------------------

   function Transfer_Encode
     (Mes : Stream_Element_Array) return Stream_Element_Array
   is
      Chunk_Size : constant Stream_Element_Offset :=
                     Stream_Element_Offset
                       (Calendar.Seconds (Calendar.Clock)) + 1;
      Rest       : Stream_Element_Offset := Mes'Length;
      Result     : Unbounded_String;
      Size       : Stream_Element_Offset;
   begin
      while Rest > 0 loop
         Size := Rest - Chunk_Size;

         if Size < 0 then
            Size := Rest;
         end if;

         Append (Result, Utils.Hex (Natural (Size)));
         Append (Result, CRLF);
         Append
           (Result,
            Translator.To_String
              (Mes (Mes'Length - Rest + Mes'First
               .. Mes'Length - Rest + Mes'First + Size - 1)));
         Append (Result, CRLF);
         Rest := Rest - Size;
      end loop;

      --  Ending chunk

      Append (Result, "0");
      Append (Result, CRLF);
      Append (Result, CRLF);

      return Translator.To_Stream_Element_Array (To_String (Result));
   end Transfer_Encode;

   WS      : Server.HTTP;
   Sock    : Net.Socket_Type'Class := Net.Socket (Security => False);
   Headers : AWS.Headers.List;

begin
   Server.Start
     (WS, "chunked_compressed",
      CB'Unrestricted_Access,
      Port           => 0,
      Max_Connection => 5);
   Text_IO.Put_Line ("started");
   Ada.Text_IO.Flush;
   delay 1.0;

   --  AWS.Client.Set_Debug (True);

   Net.Connect
     (Sock,
      Net.Localhost (Server.Status.Is_IPv6 (WS)),
      Server.Status.Port (WS));

   --  Send message

   Send
     (Sock,
      Headers,
      Transfer_Encode (Translator.To_Stream_Element_Array (Message)));

   AWS.Headers.Set.Add (Headers, Messages.Content_Encoding_Token, "gzip");

   --  Send message compressed

   Send
     (Sock,
      Headers,
      Transfer_Encode
        (Translator.Compress
           (Translator.To_Stream_Element_Array (Message),
            Header => Zlib.GZip).all));

   Net.Shutdown (Sock);

   Server.Shutdown (WS);
   Text_IO.Put_Line ("shutdown");
end Put_Chunked_Compressed;
