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

with AWS.Client;
with AWS.Messages;
with AWS.MIME;
with AWS.Response;
with AWS.Server;
with AWS.Status;
with AWS.Utils;

with Get_Free_Port;

procedure ZOpen is

   use Ada;
   use AWS;

   WS : Server.HTTP;

   Port : Positive := 1271;

   procedure Call_It (URI : String);
   function CB (Request : Status.Data) return Response.Data;

   -------------
   -- Call_It --
   -------------

   procedure Call_It (URI : String) is
      R : Response.Data;
   begin
      R := Client.Get ("http://localhost:" & Utils.Image (Port) & URI);
      Ada.Text_IO.Put (Response.Message_Body (R));
   end Call_It;

   --------
   -- CB --
   --------

   function CB (Request : Status.Data) return Response.Data is
      URI      : constant String := Status.URI (Request);
      Filename : constant String := URI (URI'First + 1 .. URI'Last);
   begin
      return Response.File (MIME.Content_Type (Filename), Filename);
   end CB;

begin
   Get_Free_Port (Port);

   Server.Start
     (WS, "zopen",
      CB'Unrestricted_Access,
      Port           => Port,
      Max_Connection => 5);

   Ada.Text_IO.Put_Line ("ZOpen started");

   Call_It ("/filea.txt");
   Call_It ("/fileb.txt");
   Call_It ("/filec.txt");

   Server.Shutdown (WS);

   Ada.Text_IO.Put_Line ("shutdown");
end ZOpen;
