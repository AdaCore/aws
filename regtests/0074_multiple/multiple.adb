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
with Ada.Exceptions;

with AWS.Server;
with AWS.Client;
with AWS.Config;
with AWS.Status;
with AWS.MIME;
with AWS.Response;
with AWS.Parameters;
with AWS.Messages;
with AWS.Utils;

with Get_Free_Port;

procedure Multiple is

   use Ada;
   use Ada.Text_IO;
   use AWS;

   function CB (Request : in Status.Data) return Response.Data;

   HTTP1 : AWS.Server.HTTP;
   Port1 : Natural := 1252;

   HTTP2 : AWS.Server.HTTP;
   Port2 : Natural := 1253;

   --------
   -- CB --
   --------

   function CB (Request : in Status.Data) return Response.Data is
      Server : constant AWS.Server.HTTP_Access := AWS.Server.Get_Current;
   begin
      return Response.Build
        (MIME.Text_HTML, "call ok : "
           & Config.Server_Name (AWS.Server.Config (Server.all)));
   end CB;

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

   Get_Free_Port (Port1);

   AWS.Server.Start
     (HTTP1, "server1",
      CB'Unrestricted_Access, Port => Port1, Max_Connection => 5);

   Get_Free_Port (Port2);

   AWS.Server.Start
     (HTTP2, "server2",
      CB'Unrestricted_Access, Port => Port2, Max_Connection => 5);

   Request ("http://localhost:" & Utils.Image (Port1) & "/call");
   Request ("http://localhost:" & Utils.Image (Port2) & "/call");
   Request ("http://localhost:" & Utils.Image (Port2) & "/call");
   Request ("http://localhost:" & Utils.Image (Port1) & "/call");

   AWS.Server.Shutdown (HTTP1);
   AWS.Server.Shutdown (HTTP2);

   Put_Line ("Exit now");
exception
   when E : others =>
      Put_Line ("Main Error " & Exceptions.Exception_Information (E));
end Multiple;
