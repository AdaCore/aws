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
with AWS.Config.Set;
with AWS.MIME;
with AWS.Net;
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
   begin
      R := Client.Get
             ("http://localhost:" & Utils.Image (Server.Status.Port (WS))
              & "/zero");
      Text_IO.Put_Line (Response.Message_Body (R));

      R := Client.Get (Server.Status.Local_URL (WS) & "/zero");
      Text_IO.Put_Line (Response.Message_Body (R));
   end Call_It;

   CFG : Config.Object;

begin
   Config.Set.Server_Host     (CFG, "localhost");
   Config.Set.Server_Name     (CFG, "hostip");
   Config.Set.Server_Port     (CFG, 0);
   Config.Set.Max_Connection  (CFG, 5);

   Server.Start (WS, CB'Unrestricted_Access, CFG);

   Text_IO.Put_Line ("started"); Ada.Text_IO.Flush;

   Call_It;

   Server.Shutdown (WS);

   Text_IO.Put_Line ("shutdown");
end HostIP;
