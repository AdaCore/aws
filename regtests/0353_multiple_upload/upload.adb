------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2000-2016, AdaCore                     --
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
with Ada.Strings.Fixed;

with Ada.Exceptions;
with AWS.Server;
with AWS.Client;
with AWS.Status;
with AWS.MIME;
with AWS.Response;
with AWS.Parameters;
with AWS.Messages;
with AWS.Client.HTTP_Utils;

procedure Upload is

   use Ada;
   use Ada.Text_IO;
   use Ada.Exceptions;

   use AWS;
   use AWS.Client;

   --------
   -- CB --
   --------

   function CB (Request : Status.Data) return Response.Data is
      URI       : constant String          := Status.URI (Request);
      P_List    : constant Parameters.List := Status.Parameters (Request);
      Server_FN : constant String          :=
                    Parameters.Get (P_List, "filename");
      K         : constant Positive        :=
                    Strings.Fixed.Index (Server_FN, "-");
   begin
      if URI = "/upload" then
         Put_Line
           ("Client Filename = "
            & Parameters.Get (P_List, "filename", 2));
         Put_Line
           ("Server Filename = "
            & "./" & Server_FN (K + 1 .. Server_FN'Last));
         return Response.Build (MIME.Text_HTML, "call ok");
      else
         Put_Line ("Unknown URI " & URI);
         return Response.Build (MIME.Text_HTML, URI & " not found", Messages.S404);
      end if;
   end CB;

   ------------------
   -- Test_Upload0 --
   ------------------

   procedure Test_Upload0 (Url : String; File : String) is
      R : Response.Data;
   begin
      Put_Line ("Start upload0");
      for I in 1 .. 2 loop
         Put_Line ("Start upload " & I'Img);
         R := Client.Upload (Url & "/upload", File);
         Put_Line ("=> " & Response.Message_Body (R));
         New_Line;
      end loop;
      Put_Line ("Finish upload0");
      New_Line;
   end Test_Upload0;

   ------------------
   -- Test_Upload1 --
   ------------------

   procedure Test_Upload1 (Url : String; File : String) is
      Connection : HTTP_Connection;
      R : Response.Data;
   begin
      Put_Line ("Start upload1");
      Client.Create (Connection, Url);
      for I in 1 .. 2 loop
         Client.Upload (Connection, R, File, "/upload");
         Put_Line ("=> " & Response.Message_Body (R));
         New_Line;
      end loop;
      Put_Line ("Finish upload1");
      New_Line;
   end Test_Upload1;

   HTTP : AWS.Server.HTTP;

begin
   Put_Line ("Start main, wait for server to start...");
   Server.Start
     (HTTP, "upload",
      CB'Unrestricted_Access,
      Port             => 8080,
      Upload_Directory => "./");
   Put_Line ("Server started");
   New_Line;

   declare
      URL : constant String := "http://localhost:8080";
      File : constant String := "upload.adb";
   begin
      Test_Upload0 (URL, File);
      Test_Upload1 (URL, File);
   end;

   Server.Shutdown (HTTP);
exception
   when E : others =>
      Put_Line ("Main Error " & Exception_Information (E));
      Server.Shutdown (HTTP);
end Upload;
