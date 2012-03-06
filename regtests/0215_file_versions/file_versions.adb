------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2011-2012, AdaCore                     --
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

with Ada.Streams;
with Ada.Text_IO;

with AWS.Client;
with AWS.Config.Set;
with AWS.MIME;
with AWS.Resources;
with AWS.Response;
with AWS.Server.Status;
with AWS.Status;
with AWS.Translator;
with AWS.Utils;

with ZLib;

procedure File_Versions is

   use Ada;
   use Ada.Streams;
   use AWS;

   WS  : Server.HTTP;
   CNF : Config.Object;

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

   R : Response.Data;

begin
   Config.Set.Server_Name    (CNF, "file_versions");
   Config.Set.Server_Host    (CNF, "localhost");
   Config.Set.Server_Port    (CNF, 0);
   Config.Set.Max_Connection (CNF, 1);

   Server.Start (WS, CB'Unrestricted_Access, CNF);
   Text_IO.Put_Line ("started"); Ada.Text_IO.Flush;

   R := Client.Get (Server.Status.Local_URL (WS));

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
