------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2003-2004                          --
--                                ACT-Europe                                --
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

--  ~ MAIN [STD]

with Ada.Text_IO;

with AWS.Client;
with AWS.Messages;
with AWS.MIME;
with AWS.Response;
with AWS.Server;
with AWS.Status;

procedure ZOpen is

   use Ada;
   use AWS;

   WS : Server.HTTP;

   function CB (Request : in Status.Data) return Response.Data;

   --------
   -- CB --
   --------

   function CB (Request : in Status.Data) return Response.Data is
      URI      : constant String := Status.URI (Request);
      Filename : constant String := URI (URI'First + 1 .. URI'Last);
   begin
      return Response.File (MIME.Content_Type (Filename), Filename);
   end CB;

   R : Response.Data;

begin
   Server.Start
     (WS, "zopen",
      CB'Unrestricted_Access,
      Port           => 1271,
      Max_Connection => 5);

   Ada.Text_IO.Put_Line ("ZOpen started");

   R := Client.Get ("http://localhost:1271/filea.txt");
   Ada.Text_IO.Put (Response.Message_Body (R));

   R := Client.Get ("http://localhost:1271/fileb.txt");
   Ada.Text_IO.Put (Response.Message_Body (R));

   R := Client.Get ("http://localhost:1271/filec.txt");
   Ada.Text_IO.Put (Response.Message_Body (R));

   Server.Shutdown (WS);

   Ada.Text_IO.Put_Line ("shutdown");
end ZOpen;
