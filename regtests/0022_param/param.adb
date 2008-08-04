------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2000-2008, AdaCore                     --
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

with Ada.Exceptions;
with Ada.Text_IO;

with AWS.Client;
with AWS.Config.Set;
with AWS.Messages;
with AWS.MIME;
with AWS.Parameters;
with AWS.Response;
with AWS.Server;
with AWS.Status;
with AWS.Utils;

with Get_Free_Port;

procedure Param is

   use Ada;
   use Ada.Text_IO;
   use AWS;

   function CB (Request : in Status.Data) return Response.Data;

   task Server is
      entry Started;
      entry Stopped;
   end Server;

   HTTP : AWS.Server.HTTP;
   Port : Natural := 1243;

   --------
   -- CB --
   --------

   function CB (Request : in Status.Data) return Response.Data is
      URI    : constant String          := Status.URI (Request);
      P_List : constant Parameters.List := Status.Parameters (Request);

      procedure Print (Name, Value : in String);

      -----------
      -- Print --
      -----------

      procedure Print (Name, Value : in String) is
      begin
         Put_Line (Name & " = " & Value);
      end Print;

   begin
      if URI = "/call" then
         Put_Line ("N  = " & Natural'Image (Parameters.Count (P_List)));
         Put_Line ("p1 = " & Parameters.Get (P_List, "p1"));
         Put_Line ("p2 = " & Parameters.Get (P_List, "p2"));
         Put_Line ("Parameters = " & Parameters.URI_Format (P_List));

         return Response.Build (MIME.Text_HTML, "call ok");

      elsif URI = "/call call" then
         Put_Line ("N  = " & Natural'Image (Parameters.Count (P_List)));
         Put_Line ("p1 = " & Parameters.Get (P_List, "p1"));
         Put_Line ("p2 = " & Parameters.Get (P_List, "p2"));

         return Response.Build (MIME.Text_HTML, "call call ok");

      elsif URI = "/spec" then
         Put_Line ("N  = " & Natural'Image (Parameters.Count (P_List)));
         Put_Line ("p&1 = " & Parameters.Get (P_List, "p&1"));
         Put_Line ("p=2 = " & Parameters.Get (P_List, "p=2"));

         return Response.Build (MIME.Text_HTML, "call call ok");

      elsif URI = "/dup" then
         Put_Line ("N  = " & Natural'Image (Parameters.Count (P_List)));
         Put_Line ("P1 = " & P_List.Get ("P1", 1));
         Put_Line ("P2 = " & P_List.Get ("p2", 2));
         P_List.Iterate_Names (", ", Print'Access);

         return Response.Build (MIME.Text_HTML, "call call ok");

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
      Config : AWS.Config.Object;
   begin
      Get_Free_Port (Port);

      AWS.Config.Set.Server_Port (Config, Port);
      AWS.Config.Set.Max_Connection (Config, 5);
      AWS.Config.Set.Server_Name (Config, "param");
      AWS.Config.Set.Case_Sensitive_Parameters (Config, False);

      AWS.Server.Start (HTTP, CB'Unrestricted_Access, Config);

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

   -------------
   -- Request --
   -------------

   procedure Request (URL : in String) is
      R : Response.Data;
   begin
      R := Client.Get (URL);
      Put_Line ("=> " & Response.Message_Body (R));
      New_Line;
   end Request;

begin
   Put_Line ("Start main, wait for server to start...");

   Server.Started;

   Request ("http://localhost:" & Utils.Image (Port)
              & "/call");
   Request ("http://localhost:" & Utils.Image (Port)
              & "/call call");
   Request ("http://localhost:" & Utils.Image (Port)
              & "/call?p1=8&p2=azerty%3e%20%26%3c%3fqwerty");
   Request ("http://localhost:" & Utils.Image (Port)
              & "/call call?p1=8&p2=azerty%3e%20qwerty");
   Request ("http://localhost:" & Utils.Image (Port)
              & "/call%20call%3fp1=a%20a%3f");
   Request ("http://localhost:" & Utils.Image (Port)
              & "/spec?p%261=1%3d1&p%3D2=2%262");
   Request ("http://localhost:" & Utils.Image (Port)
              & "/dup?p1=p-1.1&P1=p-1.2&P2=p-2.1&p2=p-2.2");

   Server.Stopped;

exception
   when E : others =>
      Put_Line ("Main Error " & Exceptions.Exception_Information (E));
end Param;
