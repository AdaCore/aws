------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2003-2012, AdaCore                     --
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

with Ada.Strings.Fixed;
with Ada.Strings.Maps.Constants;
with Ada.Text_IO;
with Ada.Exceptions;

with AWS.Client;
with AWS.Config.Set;
with AWS.Exceptions;
with AWS.Log;
with AWS.MIME;
with AWS.Messages;
with AWS.Net;
with AWS.Parameters;
with AWS.Response;
with AWS.Server.Status;
with AWS.Status;
with AWS.Utils;

with Stack_Size;

procedure Upload2 is

   use Ada;
   use Ada.Text_IO;
   use AWS;

   function CB (Request : Status.Data) return Response.Data;

   task Server is
      pragma Storage_Size (Stack_Size.Value);
      entry Start;
      entry Started;
      entry Stopped;
   end Server;

   HTTP : AWS.Server.HTTP;

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
   end Problem;

   ------------
   -- Server --
   ------------

   task body Server is
      Web_Config : Config.Object;
   begin
      accept Start;

      Config.Set.Server_Name (Web_Config, "upload2");
      Config.Set.Server_Port (Web_Config, 0);
      Config.Set.Max_Connection (Web_Config, 5);
      Config.Set.Upload_Directory (Web_Config, "/this/one/does/not/exists/");

      AWS.Server.Set_Unexpected_Exception_Handler
        (HTTP, Problem'Unrestricted_Access);

      AWS.Server.Start (HTTP, CB'Unrestricted_Access, Web_Config);

      Put_Line ("Server started");
      New_Line;

      accept Started;

      select
         accept Stopped;
      or
         delay 25.0;
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

   procedure Request (URL : String; Filename : String) is
      R : Response.Data;
   begin
      R := Client.Upload (URL, Filename);
      Put_Line
        ("=> "
         & Utils.Head_Before
             (Response.Message_Body (R), "Call stack traceback locations:"));
      New_Line;
   exception
      when AWS.Net.Socket_Error =>
         Put_Line ("NOk, exception Socket_Error on client side!");
   end Request;

begin
   Put_Line ("Start main, wait for server to start...");

   Server.Start;

   Server.Started;

   Request (AWS.Server.Status.Local_URL (HTTP) & "/upload", "file.txt");

   Server.Stopped;

exception
   when E : others =>
      Put_Line ("Main Error " & Ada.Exceptions.Exception_Information (E));
end Upload2;
