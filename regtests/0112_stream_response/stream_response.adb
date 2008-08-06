------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2004-2008, AdaCore                     --
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

with Ada.Text_IO;
with Ada.Streams;

with AWS.Client;
with AWS.MIME;
with AWS.Resources;
with AWS.Response;
with AWS.Server;
with AWS.Status;
with AWS.Translator;
with AWS.Utils;

with Get_Free_Port;

procedure Stream_Response is

   use Ada;
   use Ada.Streams;
   use AWS;

   WS   : Server.HTTP;
   N    : Natural := 0;
   Port : Positive := 1246;

   function CB (Request : in Status.Data) return Response.Data is
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
      R := Client.Get ("http://localhost:" & Utils.Image (Port));

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
   Get_Free_Port (Port);

   Server.Start
     (WS, "stream_response",
      CB'Unrestricted_Access,
      Port => Port,
      Max_Connection => 15);

   Text_IO.Put_Line ("started"); Ada.Text_IO.Flush;

   for K in 1 .. 110 loop
      Get_Next;
   end loop;

   Server.Shutdown (WS);

   Text_IO.Put_Line ("shutdown");
end Stream_Response;
