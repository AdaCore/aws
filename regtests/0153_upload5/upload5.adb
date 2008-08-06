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

--  Test the upload directory config

with Ada.Strings.Fixed;
with Ada.Strings.Maps.Constants;
with Ada.Text_IO;
with Ada.Exceptions;

with GNAT.Directory_Operations;
with GNAT.OS_Lib;

with AWS.Server;
with AWS.Client;
with AWS.Status;
with AWS.MIME;
with AWS.Response;
with AWS.Parameters;
with AWS.Messages;
with AWS.Utils;

with Get_Free_Port;

procedure Upload5 is

   use Ada;
   use Ada.Text_IO;
   use GNAT;
   use AWS;

   function CB (Request : in Status.Data) return Response.Data;

   task Server is
      entry Start;
      entry Started;
      entry Stop;
      entry Stopped;
   end Server;

   HTTP  : AWS.Server.HTTP;

   Port  : Natural := 7645;

   First : Boolean := True;
   --  Set to True for the first file upload

   --------
   -- CB --
   --------

   function CB (Request : in Status.Data) return Response.Data is
      URI    : constant String          := Status.URI (Request);
      P_List : constant Parameters.List := Status.Parameters (Request);
   begin
      if URI = "/upload" then
         Put_Line ("Client Filename = "
                     & Parameters.Get (P_List, "filename", 2));

         declare
            use Ada.Strings;

            Server_Filename : constant String
              := Parameters.Get (P_List, "filename");
            First_N : Positive;
            Last_N  : Natural;
         begin
            --  Remove number from uploaded filename

            Fixed.Find_Token
              (Server_Filename,
               Maps.Constants.Decimal_Digit_Set, Inside, First_N, Last_N);

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
                 (MIME.Text_HTML, "Server file not found!");
            end if;
         end;

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
      Get_Free_Port (Port);

      accept Start;

      AWS.Server.Start
        (HTTP, "upload",
         CB'Unrestricted_Access,
         Port             => Port,
         Max_Connection   => 5,
         Upload_Directory => "upload_dir/");

      Put_Line ("Server started");
      New_Line;

      accept Started;

      select
         accept Stop;
      or
         delay 5.0;
         Put_Line ("Too much time to do the job !");
      end select;

      AWS.Server.Shutdown (HTTP);

      accept Stopped;
   exception
      when E : others =>
         Put_Line ("Server Error " & Exceptions.Exception_Information (E));
   end Server;

   -------------
   -- Request --
   -------------

   procedure Request (URL : in String; Filename : in String) is
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

   Server.Start;

   Server.Started;

   Request
     ("http://localhost:" & Utils.Image (Port) & "/upload", "test.out");
   Request
     ("http://localhost:" & Utils.Image (Port) & "/upload", "upload5.adb");

   Server.Stop;
   Server.Stopped;

   --  Remove directory

   Directory_Operations.Remove_Dir ("upload_dir");

exception
   when E : others =>
      Put_Line ("Main Error " & Exceptions.Exception_Information (E));

      Server.Stop;
      Server.Stopped;
end Upload5;
