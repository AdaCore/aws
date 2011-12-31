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
with AWS.MIME;
with AWS.Response;
with AWS.Server.Status;
with AWS.Status;
with AWS.Utils;

procedure HostIP is

   use Ada;
   use AWS;

   WS : Server.HTTP;

   function CB (Request : Status.Data) return Response.Data is
   begin
      return Response.Build (MIME.Text_HTML, "Ok, got it");
   end CB;

   procedure Call_It is
      R : Response.Data;
      Port : constant String := Utils.Image (Server.Status.Port (WS));
   begin
      R := Client.Get ("http://localhost:" & Port & "/zero");
      Text_IO.Put_Line (Response.Message_Body (R));

      R := Client.Get ("http://127.0.0.1:" & Port & "/zero");
      Text_IO.Put_Line (Response.Message_Body (R));
   end Call_It;

begin
   Server.Start
     (WS, "hostip", CB'Unrestricted_Access, Port => 0, Max_Connection => 5);
   Text_IO.Put_Line ("started"); Ada.Text_IO.Flush;

   Call_It;

   Server.Shutdown (WS);
   Text_IO.Put_Line ("shutdown");
end HostIP;
