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

with Ada.Directories;
with Ada.Strings.Fixed;
with Ada.Strings.Maps.Constants;
with Ada.Text_IO;
with Ada.Exceptions;

with AWS.Server.Status;
with AWS.Client;
with AWS.Status;
with AWS.MIME;
with AWS.OS_Lib;
with AWS.Response;
with AWS.Parameters;
with AWS.Messages;
with AWS.Utils;

procedure Upload is

   use Ada;
   use Ada.Text_IO;
   use AWS;

   function CB (Request : Status.Data) return Response.Data;

   HTTP : AWS.Server.HTTP;

   --------
   -- CB --
   --------

   function CB (Request : Status.Data) return Response.Data is
      use Ada.Strings;
      use type Maps.Character_Set;

      URI    : constant String          := Status.URI (Request);
      P_List : constant Parameters.List := Status.Parameters (Request);

      Server_FN : constant String := Parameters.Get (P_List, "filename");

      First  : Positive;
      Last   : Natural;

   begin
      if URI = "/upload" then
         Put_Line ("Client Filename = "
                   & Parameters.Get (P_List, "filename", 2));

         --  Remove number from uploaded filename

         Fixed.Find_Token
           (Server_FN,
            Maps.Constants.Decimal_Digit_Set or Maps.To_Set ("-"),
            Inside, First, Last);

         Put_Line ("Server Filename = "
                   & Fixed.Replace_Slice (Server_FN, First, Last, ""));

         return Response.Build (MIME.Text_HTML, "call ok");

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
      R := Client.Upload
        (URL, Directories.Current_Directory
         & OS_Lib.Directory_Separator & Filename);
      Put_Line ("=> " & Response.Message_Body (R));
      New_Line;
   end Request;

begin
   Put_Line ("Start main, wait for server to start...");

   Server.Start
     (HTTP, "upload",
      CB'Unrestricted_Access,
      Port             => 0,
      Max_Connection   => 5,
      Upload_Directory => "./");

   Put_Line ("Server started");
   New_Line;

   declare
      URL : constant String := Server.Status.Local_URL (HTTP);
   begin
      Request (URL & "/upload", "test.out");
      Request (URL & "/upload", "upload.adb");
   end;

   Server.Shutdown (HTTP);

exception
   when E : others =>
      Put_Line ("Main Error " & Exceptions.Exception_Information (E));
      Server.Shutdown (HTTP);
end Upload;
