------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                       Copyright (C) 2012, AdaCore                        --
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
with Ada.Exceptions;

with AWS.Client;
with AWS.Config.Set;
with AWS.MIME;
with AWS.Messages;
with AWS.Net;
with AWS.Response;
with AWS.Server.Status;
with AWS.Status;
with AWS.Utils;

procedure Upload_Progress is

   use Ada;
   use Ada.Streams;
   use Ada.Text_IO;
   use AWS;

   function CB (Request : Status.Data) return Response.Data;

   HTTP : AWS.Server.HTTP;

   procedure Start_Server;

   --------
   -- CB --
   --------

   function CB (Request : Status.Data) return Response.Data is
      URI : constant String := Status.URI (Request);
   begin
      if URI = "/upload" then
         return Response.Build (MIME.Text_HTML, "call ok");
      else
         Put_Line ("Unknown URI " & URI);
         return Response.Build
           (MIME.Text_HTML, URI & " not found", Messages.S404);
      end if;
   end CB;

   ------------------
   -- Start_Server --
   ------------------

   procedure Start_Server is
      Web_Config : Config.Object;
   begin
      Config.Set.Server_Name (Web_Config, "Upload Progress");
      Config.Set.Server_Port (Web_Config, 0);
      Config.Set.Max_Connection (Web_Config, 5);
      Config.Set.Upload_Directory (Web_Config, ".");

      AWS.Server.Start (HTTP, CB'Unrestricted_Access, Web_Config);

      Put_Line ("Server started");
      New_Line;
   end Start_Server;

   -------------
   -- Request --
   -------------

   procedure Request (URL : String; Filename : String) is

      procedure Display_Progress (Total, Sent : Stream_Element_Offset);

      ----------------------
      -- Display_Progress --
      ----------------------

      procedure Display_Progress (Total, Sent : Stream_Element_Offset) is
      begin
         Text_IO.Put_Line
           (Stream_Element_Offset'Image (Total)
            & " -" & Stream_Element_Offset'Image (Sent));
      end Display_Progress;

      R : Response.Data;
   begin
      R := Client.Upload (URL, Filename, Progress => Display_Progress'Access);
      New_Line;
   exception
      when AWS.Net.Socket_Error =>
         Put_Line ("NOk, exception Socket_Error on client side!");
   end Request;

begin
   Put_Line ("Start main, wait for server to start...");

   Start_Server;

   Request (AWS.Server.Status.Local_URL (HTTP) & "/upload", "file.txt");

   Server.Shutdown (HTTP);

exception
   when E : others =>
      Put_Line ("Main Error " & Ada.Exceptions.Exception_Information (E));
      Server.Shutdown (HTTP);
end Upload_Progress;
