------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                       Copyright (C) 2011, AdaCore                        --
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

with Ada.Streams;
with Ada.Text_IO;

with AWS.Client;
with AWS.MIME;
with AWS.Resources;
with AWS.Response;
with AWS.Server;
with AWS.Status;
with AWS.Translator;
with AWS.Utils;

with ZLib;

with Get_Free_Port;

procedure File_Versions is

   use Ada;
   use Ada.Streams;
   use AWS;

   WS : Server.HTTP;

   function CB (Request : Status.Data) return Response.Data is
   begin
      return Response.File (MIME.Text_HTML, "hello.txt.gz");
   end CB;

   procedure Exist (Filename : String) is
   begin
      Text_IO.Put ("> " & Filename);
      Text_IO.Set_Col (20);
      Text_IO.Put_Line
        (Resources.File_Instance'Image (Resources.Exist (Filename)));
   end Exist;

   Port : Natural := 1234;
   R    : Response.Data;

begin
   Get_Free_Port (Port);

   Server.Start
     (WS, "file_versions",
      CB'Unrestricted_Access, Port => Port, Max_Connection => 1);
   Text_IO.Put_Line ("started"); Ada.Text_IO.Flush;

   R := Client.Get ("http://localhost:" & Utils.Image (Port));

   Text_IO.Put_Line
     ("R : "
      & Translator.To_String
        (Translator.Decompress (Response.Message_Body (R), ZLib.GZip).all));

   Server.Shutdown (WS);
   Text_IO.Put_Line ("shutdown");

   Exist ("data1.txt");
   Exist ("data1.txt.gz");
   Exist ("data2.txt");
   Exist ("data2.txt.gz");
   Exist ("data3.txt");
   Exist ("data3.txt.gz");
   Exist ("data4.txt");
   Exist ("data4.txt.gz");
end File_Versions;
