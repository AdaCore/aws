------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2003-2009, AdaCore                     --
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

with Ada.Strings.Fixed;
with Ada.Strings.Maps.Constants;
with Ada.Text_IO;
with Ada.Exceptions;

with AWS.Log;
with AWS.Client;
with AWS.Config.Set;
with AWS.Exceptions;
with AWS.MIME;
with AWS.Messages;
with AWS.Net;
with AWS.Parameters;
with AWS.Response;
with AWS.Server;
with AWS.Status;
with AWS.Utils;

with Get_Free_Port;

procedure Upload3 is

   use Ada;
   use Ada.Text_IO;
   use AWS;

   function CB (Request : Status.Data) return Response.Data;

   task Server is
      entry Start;
      entry Started;
      entry Stopped;
   end Server;

   HTTP : AWS.Server.HTTP;
   Port : Natural := 8821;

   procedure Problem
     (E      : Ada.Exceptions.Exception_Occurrence;
      Log    : in out AWS.Log.Object;
      Error  : AWS.Exceptions.Data;
      Answer : in out Response.Data);
   --  Web exception handler

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
     (E      : Ada.Exceptions.Exception_Occurrence;
      Log    : in out AWS.Log.Object;
      Error  : AWS.Exceptions.Data;
      Answer : in out Response.Data)
   is
      use Ada.Strings;
      Error_Message : constant String := Ada.Exceptions.Exception_Message (E);

      First : Positive;
      Last  : Natural;
   begin
      Put_Line ("Ok, exception received on the server side");
      Put_Line ("  => " & Ada.Exceptions.Exception_Name (E));

      Fixed.Find_Token
        (Error_Message, Maps.Constants.Decimal_Digit_Set, Inside, First, Last);

      Put_Line
        ("  => " & Fixed.Replace_Slice (Error_Message, First, Last, ""));

      Answer := Response.Build
        (MIME.Text_HTML, "Sorry the server is temporarily unavailable.");
   end Problem;

   ------------
   -- Server --
   ------------

   task body Server is
      Web_Config : Config.Object;
   begin
      Get_Free_Port (Port);

      Config.Set.Server_Name (Web_Config, "upload3");
      Config.Set.Server_Port (Web_Config, Port);
      Config.Set.Max_Connection (Web_Config, 5);
      Config.Set.Upload_Directory (Web_Config, "/this/one/does/not/exists/");

      AWS.Server.Set_Unexpected_Exception_Handler
        (HTTP, Problem'Unrestricted_Access);

      accept Start;

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
         Put_Line ("Server Error " & Ada.Exceptions.Exception_Information (E));
   end Server;

   -------------
   -- Request --
   -------------

   procedure Request (URL : String; Filename : String) is
      R : Response.Data;
   begin
      R := Client.Upload (URL, Filename);
      Put_Line ("=> " & Response.Message_Body (R));
      New_Line;
   exception
      when E : AWS.Net.Socket_Error =>
         Put_Line ("NOk, exception Socket_Error on client side!");
         Put_Line (Ada.Exceptions.Exception_Information (E));
   end Request;

begin
   Put_Line ("Start main, wait for server to start...");

   Server.Start;
   Server.Started;

   Request
     ("http://localhost:" & Utils.Image (Port) & "/upload", "test.out");

   Server.Stopped;

exception
   when E : others =>
      Put_Line ("Main Error " & Ada.Exceptions.Exception_Information (E));
end Upload3;
