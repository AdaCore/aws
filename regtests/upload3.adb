------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                            Copyright (C) 2003                            --
--                               ACT-Europe                                 --
--                                                                          --
--  Authors: Dmitriy Anisimokv - Pascal Obry                                --
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

--  $Id$

with Ada.Text_IO;
with Ada.Exceptions;

with AWS.Server;
with AWS.Client;
with AWS.Config.Set;
with AWS.Status;
with AWS.MIME;
with AWS.Net;
with AWS.Response;
with AWS.Parameters;
with AWS.Messages;

procedure Upload3 is

   use Ada;
   use Ada.Text_IO;
   use AWS;

   function CB (Request : in Status.Data) return Response.Data;

   task Server is
      entry Started;
      entry Stopped;
   end Server;

   HTTP : AWS.Server.HTTP;

   procedure Problem
     (E           : in     Ada.Exceptions.Exception_Occurrence;
      Termination : in     Boolean;
      Answer      : in out Response.Data);
   --  Web exception handler

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
         Put_Line ("Server Filename = "
                     & Parameters.Get (P_List, "filename"));

         return Response.Build (MIME.Text_HTML, "call ok");

      else
         Put_Line ("Unknown URI " & URI);
         return Response.Build
           (MIME.Text_HTML, URI & " not found", Messages.S404);
      end if;
   end CB;

   -------------
   -- Problem --
   -------------

   procedure Problem
     (E           : in     Ada.Exceptions.Exception_Occurrence;
      Termination : in     Boolean;
      Answer      : in out Response.Data) is
   begin
      Put_Line ("Ok, exception received on the server side");
      Put_Line ("  => " & Exceptions.Exception_Name (E));
      Put_Line ("  => " & Exceptions.Exception_Message (E));

      Answer := Response.Build
        (MIME.Text_HTML, "Sorry the server is temporarily unavailable.");
   end Problem;

   ------------
   -- Server --
   ------------

   task body Server is
      Web_Config : Config.Object;
   begin
      Config.Set.Server_Name (Web_Config, "upload2");
      Config.Set.Server_Port (Web_Config, 7643);
      Config.Set.Max_Connection (Web_Config, 5);
      Config.Set.Upload_Directory (Web_Config, "/this/one/does/not/exists");

      AWS.Server.Set_Unexpected_Exception_Handler
        (HTTP, Problem'Unrestricted_Access);

      AWS.Server.Start (HTTP, CB'Unrestricted_Access, Web_Config);

      Put_Line ("Server started");
      New_Line;

      accept Started;

      select
         accept Stopped;
      or
         delay 20.0;
         Put_Line ("Too much time to do the job !");
      end select;

      AWS.Server.Shutdown (HTTP);

      Put_Line ("Server stopped");
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
   exception
      when E : AWS.Net.Socket_Error =>
         Put_Line ("NOk, exception Socket_Error on client side!");
         Put_Line (Exceptions.Exception_Information (E));
   end Request;

begin
   Put_Line ("Start main, wait for server to start...");

   Server.Started;

   Request ("http://localhost:7643/upload", "upload2.ali");

   Server.Stopped;

exception
   when E : others =>
      Put_Line ("Main Error " & Exceptions.Exception_Information (E));
end Upload3;
