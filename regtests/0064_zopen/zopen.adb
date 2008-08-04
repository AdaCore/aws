------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2003-2008, AdaCore                     --
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

   procedure Call_It (URI : in String);
   function CB (Request : in Status.Data) return Response.Data;

   -------------
   -- Call_It --
   -------------

   procedure Call_It (URI : in String) is
      R : Response.Data;
   begin
      R := Client.Get ("http://localhost:" & Utils.Image (Port) & URI);
      Ada.Text_IO.Put (Response.Message_Body (R));
   end Call_It;

   --------
   -- CB --
   --------

   function CB (Request : in Status.Data) return Response.Data is
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
