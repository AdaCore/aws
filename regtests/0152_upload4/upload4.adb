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
with Ada.Exceptions;

with AWS.Client;
with AWS.Exceptions;
with AWS.Log;
with AWS.Server;
with AWS.Status;
with AWS.MIME;
with AWS.Response;
with AWS.Parameters;
with AWS.Messages;
with AWS.Utils;

with Get_Free_Port;

procedure Upload4 is

   use Ada;
   use Ada.Text_IO;
   use AWS;

   function CB (Request : in Status.Data) return Response.Data;

   procedure Error
     (E           : in     Ada.Exceptions.Exception_Occurrence;
      Log         : in out AWS.Log.Object;
      Error       : in     AWS.Exceptions.Data;
      Answer      : in out Response.Data);

   task Server is
      entry Started;
      entry Stopped;
   end Server;

   HTTP : AWS.Server.HTTP;
   Port : Positive := 7644;

   -----------
   -- Error --
   -----------

   procedure Error
     (E           : in     Ada.Exceptions.Exception_Occurrence;
      Log         : in out AWS.Log.Object;
      Error       : in     AWS.Exceptions.Data;
      Answer      : in out Response.Data) is
   begin
      Put_Line (Ada.Exceptions.Exception_Message (E));
   end Error;

   --------
   -- CB --
   --------

   function CB (Request : in Status.Data) return Response.Data is
      URI    : constant String          := Status.URI (Request);
      P_List : constant Parameters.List := Status.Parameters (Request);
   begin
      if URI = "/upload" then
         return Response.Build (MIME.Text_HTML, "NOK, should not be called");

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
      AWS.Server.Set_Unexpected_Exception_Handler
        (HTTP, Error'Unrestricted_Access);

      Get_Free_Port (Port);

      AWS.Server.Start
        (HTTP, "upload4",
         CB'Unrestricted_Access,
         Port             => Port,
         Max_Connection   => 5);

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
         Put_Line ("Server Error " & Ada.Exceptions.Exception_Information (E));
   end Server;

   -------------
   -- Request --
   -------------

   procedure Request (URL : in String; Filename : in String) is
      R : Response.Data;
   begin
      R := Client.Upload (URL, Filename);
   end Request;

begin
   Put_Line ("Start main, wait for server to start...");

   Server.Started;

   Request
     ("http://localhost:" & Utils.Image (Port) & "/upload", "test.out");

   Server.Stopped;

exception
   when E : others =>
      Put_Line ("Main Error " & Ada.Exceptions.Exception_Information (E));
end Upload4;
