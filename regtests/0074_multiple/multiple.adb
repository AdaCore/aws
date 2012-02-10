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

with Ada.Text_IO;
with Ada.Exceptions;

with AWS.Server.Status;
with AWS.Client;
with AWS.Config.Set;
with AWS.Status;
with AWS.MIME;
with AWS.Response;
with AWS.Parameters;
with AWS.Messages;
with AWS.Utils;

procedure Multiple is

   use Ada;
   use Ada.Text_IO;
   use AWS;

   function CB (Request : Status.Data) return Response.Data;

   HTTP1 : AWS.Server.HTTP;
   HTTP2 : AWS.Server.HTTP;

   CNF : Config.Object;

   --------
   -- CB --
   --------

   function CB (Request : Status.Data) return Response.Data is
      Server : constant AWS.Server.HTTP_Access := AWS.Server.Get_Current;
   begin
      return Response.Build
        (MIME.Text_HTML, "call ok : "
           & Config.Server_Name (AWS.Server.Config (Server.all)));
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

   Config.Set.Server_Name    (CNF, "server1");
   Config.Set.Server_Host    (CNF, "localhost");
   Config.Set.Server_Port    (CNF, 0);
   Config.Set.Max_Connection (CNF, 5);

   AWS.Server.Start (HTTP1, CB'Unrestricted_Access, CNF);

   Config.Set.Server_Name (CNF, "server2");
   Config.Set.Server_Port (CNF, 0);

   AWS.Server.Start (HTTP2, CB'Unrestricted_Access, CNF);

   declare
      URL1 : constant String := AWS.Server.Status.Local_URL (HTTP1);
      URL2 : constant String := AWS.Server.Status.Local_URL (HTTP2);
   begin
      Request (URL1 & "/call");
      Request (URL2 & "/call");
      Request (URL2 & "/call");
      Request (URL1 & "/call");
   end;

   AWS.Server.Shutdown (HTTP1);
   AWS.Server.Shutdown (HTTP2);

   Put_Line ("Exit now");
exception
   when E : others =>
      Put_Line ("Main Error " & Exceptions.Exception_Information (E));
end Multiple;
