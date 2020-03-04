------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2004-2020, AdaCore                     --
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

--  Test the upload directory config

with Ada.Strings.Fixed;
with Ada.Strings.Maps.Constants;
with Ada.Text_IO;
with Ada.Exceptions;

with GNAT.Directory_Operations;
with GNAT.OS_Lib;

with AWS.Server.Status;
with AWS.Client;
with AWS.Status;
with AWS.MIME;
with AWS.Response;
with AWS.Parameters;
with AWS.Messages;
with AWS.Utils;

procedure Upload5 is

   use Ada;
   use Ada.Text_IO;
   use GNAT;
   use AWS;

   function CB (Request : Status.Data) return Response.Data;

   HTTP  : AWS.Server.HTTP;

   First : Boolean := True;
   --  Set to True for the first file upload

   --------
   -- CB --
   --------

   function CB (Request : Status.Data) return Response.Data is
      URI    : constant String          := Status.URI (Request);
      P_List : constant Parameters.List := Status.Parameters (Request);
   begin
      if URI = "/upload" then
         Put_Line ("Client Filename = "
                     & Parameters.Get (P_List, "filename", 2));

         declare
            use Ada.Strings;
            use type Maps.Character_Set;

            Server_Filename : constant String :=
                                Parameters.Get (P_List, "filename");
            First_N : Positive;
            Last_N  : Natural;
         begin
            --  Remove number from uploaded filename

            Fixed.Find_Token
              (Server_Filename,
               Maps.Constants.Decimal_Digit_Set or Maps.To_Set ("-"),
               Inside, First_N, Last_N);

            Put_Line ("Server Filename = "
                      & Fixed.Replace_Slice
                          (Server_Filename, First_N, Last_N, ""));

            --  Checks that the first are in the upload directory

            if AWS.Utils.Is_Regular_File (Server_Filename) then

               if First then
                  First := False;
                  --  We rename the file, so the server will not be able to
                  --  delete it.

                  declare
                     Result : Boolean;
                  begin
                     GNAT.OS_Lib.Rename_File
                       (Server_Filename, "my_file_upload", Result);
                  end;
               end if;

               return Response.Build (MIME.Text_HTML, "call ok");

            else
               return Response.Build
                 (MIME.Text_HTML,
                  "Server file not found! : " & Server_Filename);
            end if;
         end;

      else
         Put_Line ("Unknown URI " & URI);
         return Response.Build
           (MIME.Text_HTML, URI & " not found", Messages.S404);
      end if;
   end CB;

   -------------
   -- Request --
   -------------

   procedure Request (URL : String; Filename : String) is
      R : Response.Data;
   begin
      R := Client.Upload (URL, Filename);
      Put_Line ("=> " & Response.Message_Body (R));
      New_Line;
   end Request;

begin
   --  First create the upload directory

   begin
      Directory_Operations.Make_Dir ("upload_dir");
   exception
      when others =>
         null;
   end;

   Put_Line ("Start main, wait for server to start...");

   Server.Start
     (HTTP, "upload",
      CB'Unrestricted_Access,
      Port             => 0,
      Max_Connection   => 5,
      Upload_Directory => "upload_dir/");

   Put_Line ("Server started");
   New_Line;

   declare
      URL : constant String := AWS.Server.Status.Local_URL (HTTP) &  "/upload";
   begin
      Request (URL, "test.out");
      Request (URL, "upload5.adb");
      Request (URL, "file%29.txt");
      Request (URL, "fi%87le%12.txt");
   end;

   Server.Shutdown (HTTP);

   --  Remove directory

   Directory_Operations.Remove_Dir ("upload_dir");

exception
   when E : others =>
      Put_Line ("Main Error " & Exceptions.Exception_Information (E));

      Server.Shutdown (HTTP);
end Upload5;
