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

with Ada.Command_Line;
with Ada.Text_IO;
with Ada.Exceptions;

with AWS.Server;
with AWS.Client;
with AWS.Status;
with AWS.MIME;
with AWS.Response;
with AWS.Parameters;
with AWS.Messages;

procedure Check_Mem is

   use Ada;
   use Ada.Text_IO;
   use AWS;

   function CB (Request : in Status.Data) return Response.Data;

   task Server is
      entry Started;
      entry Stopped;
   end Server;

   HTTP : AWS.Server.HTTP;

   S_Port : constant String   := Command_Line.Argument (2);
   Port   : constant Positive := Positive'Value (S_Port);

   -----------
   -- Check --
   -----------

   procedure Check (Str : in String) is
   begin
      null;
   end Check;

   --------
   -- CB --
   --------

   function CB (Request : in Status.Data) return Response.Data is
      URI    : constant String          := Status.URI (Request);
      P_List : constant Parameters.List := Status.Parameters (Request);
   begin
      if URI = "/simple" then
         Check (Natural'Image (Parameters.Count (P_List)));
         Check (Parameters.Get (P_List, "p1"));
         Check (Parameters.Get (P_List, "p2"));

         return Response.Build (MIME.Text_HTML, "simple ok");

      elsif URI = "/complex" then
         Check (Natural'Image (Parameters.Count (P_List)));
         Check (Parameters.Get (P_List, "p1"));
         Check (Parameters.Get (P_List, "p2"));

         for K in 1 .. Parameters.Count (P_List) loop
            Check (Parameters.Get_Name (P_List, K));
            Check (Parameters.Get_Value (P_List, K));
         end loop;

         return Response.Build (MIME.Text_HTML, "complex ok");

      elsif URI = "/multiple" then
         Check (Natural'Image (Parameters.Count (P_List)));
         Check (Parameters.Get (P_List, "par", 1));
         Check (Parameters.Get (P_List, "par", 2));
         Check (Parameters.Get (P_List, "par", 3));
         Check (Parameters.Get (P_List, "par", 4));
         Check (Parameters.Get (P_List, "par", 5));

         return Response.Build (MIME.Text_HTML, "multiple ok");

      elsif URI = "/file" then
         return Response.File (MIME.Text_Plain, "check_mem.adb");

      else
         Check ("Unknown URI " & URI);
         return Response.Build
           (MIME.Text_HTML, URI & " not found", Messages.S404);
      end if;
   end CB;

   ------------
   -- Server --
   ------------

   task body Server is
   begin
      AWS.Server.Start
        (HTTP, "check_mem",
         CB'Unrestricted_Access, Port => Port, Max_Connection => 5);

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

   ------------
   -- Client --
   ------------

   procedure Client is

      -------------
      -- Request --
      -------------

      procedure Request (URL : in String) is
         R : Response.Data;
      begin
         R := AWS.Client.Get ("http://localhost:" & S_Port & URL);
         Check (Response.Message_Body (R));
      end Request;

   begin
      Request ("/simple");
      Request ("/simple?p1=8&p2=azerty%20qwerty");
      Request ("/simple?p2=8&p1=azerty%20qwerty");
      Request ("/doesnotexist?p=8");

      Request ("/complex?p1=1&p2=2&p3=3&p4=4&p5=5&p6=6"
                 & "&p7=7&p8=8&p9=9&p10=10&p11=11&p12=12&p13=13&p14=14&p15=15"
                 & "&very_long_name_in_a_get_form=alongvalueforthistest");

      Request ("/multiple?par=1&par=2&par=3&par=4&par=whatever");

      Request ("/simple?p1=8&p2=azerty%20qwerty");
      Request ("/file");
   end Client;

begin
   Put_Line ("Start main, wait for server to start...");

   Server.Started;

   for K in 1 .. Integer'Value (Command_Line.Argument (1)) loop
      Client;
   end loop;

   Server.Stopped;

exception
   when E : others =>
      Put_Line ("Main Error " & Exceptions.Exception_Information (E));
end Check_Mem;
