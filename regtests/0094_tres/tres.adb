------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2000-2012, AdaCore                     --
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

--  Test for the embedded resource files

with Ada.Command_Line;
with Ada.Text_IO;
with Ada.Exceptions;

with AWS.Server.Status;
with AWS.Client;
with AWS.Status;
with AWS.MIME;
with AWS.Response;
with AWS.Parameters;
with AWS.Messages;
with AWS.Templates;
with AWS.Utils;

with Tresres;

procedure Tres is

   use Ada;
   use Ada.Text_IO;
   use AWS;
   use type AWS.Templates.Vector_Tag;

   function CB (Request : Status.Data) return Response.Data;

   task Server is
      entry Start;
      entry Started;
      entry Stopped;
   end Server;

   HTTP : AWS.Server.HTTP;

   Trans_Table : AWS.Templates.Translate_Table
     := (AWS.Templates.Assoc ("TAG1", "VAL1"),
         AWS.Templates.Assoc ("TAG2", "VAL2"),
         AWS.Templates.Assoc ("TAG_V", +"v1" & "v2" & "v3"),
         AWS.Templates.Assoc ("COND", True));

   --------
   -- CB --
   --------

   function CB (Request : Status.Data) return Response.Data is
      URI    : constant String := Status.URI (Request);
   begin
      if URI = "/file1" then
         return Response.File (MIME.Text_HTML, "file1.html");

      elsif URI = "/file2" then
         return Response.File (MIME.Text_HTML, "file2.html");

      elsif URI = "/file3" then
         return Response.File (MIME.Text_HTML, "file3.html");

      elsif URI = "/tmplt" then
         return Response.Build
           (MIME.Text_HTML,
            String'(AWS.Templates.Parse ("file.tmplt", Trans_Table)));

      else
         Put_Line ("Unknown URI " & URI);
         return Response.Build
           (MIME.Text_HTML, URI & " not found", Messages.S404);
      end if;
   end CB;

   ------------
   -- Server --
   ------------

   task body Server is
   begin
      accept Start;

      AWS.Server.Start
        (HTTP, "tres",
         CB'Unrestricted_Access, Port => 0, Max_Connection => 5);

      Put_Line ("Server started");
      New_Line;

      accept Started;

      select
         accept Stopped;
      or
         delay 5.0;
         Put_Line ("Too much time to do the job !");
      end select;

      AWS.Server.Shutdown (HTTP);
   exception
      when E : others =>
         Put_Line ("Server Error " & Exceptions.Exception_Information (E));
   end Server;

   -------------
   -- Request --
   -------------

   procedure Request (URL : String) is
      R : Response.Data;
   begin
      R := Client.Get (URL);
      Put_Line ("=> " & Response.Message_Body (R));
      New_Line;
   end Request;

begin
   Put_Line ("Start main, wait for server to start...");

   Server.Start;

   Server.Started;

   declare
      Port_I : constant String := Utils.Image (AWS.Server.Status.Port (HTTP));
   begin
      Request ("http://localhost:" & Port_I & "/file1");
      Request ("http://localhost:" & Port_I & "/file2");
      Request ("http://localhost:" & Port_I & "/file3");
      Request ("http://localhost:" & Port_I & "/tmplt");
      Request ("http://localhost:" & Port_I & "/file4");
   end;

   Server.Stopped;

exception
   when E : others =>
      Put_Line ("Main Error " & Exceptions.Exception_Information (E));
end Tres;
