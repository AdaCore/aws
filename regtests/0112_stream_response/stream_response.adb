------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2004-2012, AdaCore                     --
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

with Ada.Text_IO;
with Ada.Streams;

with AWS.Client;
with AWS.MIME;
with AWS.Resources;
with AWS.Response;
with AWS.Server.Status;
with AWS.Status;
with AWS.Translator;
with AWS.Utils;

procedure Stream_Response is

   use Ada;
   use Ada.Streams;
   use AWS;

   WS : Server.HTTP;
   N  : Natural := 0;

   function CB (Request : Status.Data) return Response.Data is
   begin
      N := N + 1;
      return Response.Build
        (MIME.Text_HTML, "This is response " & Natural'Image (N)
         & " from Stream_Response.CB callback procedure");
   end CB;

   procedure Get_Next is
      R      : Response.Data;
      S      : Resources.File_Type;
      Buffer : Stream_Element_Array (1 .. 10);
      Last   : Stream_Element_Offset;
   begin
      R := Client.Get (Server.Status.Local_URL (WS));

      Response.Message_Body (R, S);

      Text_IO.Put ('[');

      loop
         Resources.Read (S, Buffer, Last);
         Text_IO.Put (Translator.To_String (Buffer (1 .. Last)));
         exit when Last < Buffer'Last;
      end loop;

      Text_IO.Put_Line ("]");

      Resources.Close (S);
   end Get_Next;

begin
   Server.Start
     (WS, "stream_response",
      CB'Unrestricted_Access,
      Port => 0,
      Max_Connection => 15);

   Text_IO.Put_Line ("started"); Ada.Text_IO.Flush;

   for K in 1 .. 110 loop
      Get_Next;
   end loop;

   Server.Shutdown (WS);

   Text_IO.Put_Line ("shutdown");
end Stream_Response;
