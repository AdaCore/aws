------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2000-2018, AdaCore                     --
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

with Ada.Exceptions;
with Ada.Text_IO;

with AWS.Client;
with AWS.Config.Set;
with AWS.Messages;
with AWS.MIME;
with AWS.Parameters;
with AWS.Response;
with AWS.Server.Status;
with AWS.Status;
with AWS.Utils;

procedure Param is

   use Ada;
   use Ada.Text_IO;
   use AWS;

   function CB (Request : Status.Data) return Response.Data;

   HTTP   : Server.HTTP;
   Config : AWS.Config.Object;

   --------
   -- CB --
   --------

   function CB (Request : Status.Data) return Response.Data is
      URI    : constant String          := Status.URI (Request);
      P_List : constant Parameters.List := Status.Parameters (Request);

      procedure Print (Name, Value : String);

      -----------
      -- Print --
      -----------

      procedure Print (Name, Value : String) is
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

   -------------
   -- Request --
   -------------

   procedure Request (URL : String) is
      R : Response.Data;
   begin
      R := Client.Get (URL);
      Put_Line ("=> " & Response.Message_Body (R));
      New_Line;
   end Request;

begin
   Put_Line ("Start main, wait for server to start...");

   AWS.Config.Set.Server_Host (Config, "localhost");
   AWS.Config.Set.Server_Port (Config, 0);
   AWS.Config.Set.Server_Name (Config, "param");
   AWS.Config.Set.Max_Connection (Config, 5);
   AWS.Config.Set.Case_Sensitive_Parameters (Config, False);

   Server.Start (HTTP, CB'Unrestricted_Access, Config);
   Put_Line ("Server started");
   New_Line;

   declare
      URL : constant String := Server.Status.Local_URL (HTTP);
   begin
      Request (URL & "/call");
      Request (URL & "/call call");
      Request (URL & "/call?p1=8&p2=azerty%3e%20%26%3c%3fqwerty");
      Request (URL & "/call call?p1=8&p2=azerty%3e%20qwerty");
      Request (URL & "/call%20call?p1=a%20a%3f");
      Request (URL & "/spec?p%261=1%3d1&p%3D2=2%262");
      Request (URL & "/dup?p1=p-1.1&P1=p-1.2&P2=p-2.1&p2=p-2.2");
   end;

   AWS.Server.Shutdown (HTTP);

exception
   when E : others =>
      Put_Line ("Main Error " & Exceptions.Exception_Information (E));
end Param;
