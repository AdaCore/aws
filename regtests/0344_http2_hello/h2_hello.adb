------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                       Copyright (C) 2021, AdaCore                        --
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

with Ada.Strings.Unbounded;
with Ada.Text_IO;

with AWS.Client;
with AWS.Config.Set;
with AWS.MIME;
with AWS.Response;
with AWS.Server.Log;
with AWS.Server.Status;
with AWS.Status;
with AWS.Utils;

procedure H2_Hello is

   use Ada.Strings.Unbounded;
   use AWS;

   WS : Server.HTTP;

   function CB (Request : Status.Data) return Response.Data is
      Mes : constant Unbounded_String := Status.Binary_Data (Request);
   begin
      if Length (Mes) /= 0 then
         return Response.Build
           (MIME.Text_HTML, "Hello World to H2! (" & To_String (Mes) & ')');
      else
         return Response.Build (MIME.Text_HTML, "Hello World to H2!");
      end if;
   end CB;

   Conf : Config.Object := Config.Get_Current;
   R    : Response.Data;

begin
   Config.Set.Server_Name (Conf, "HTTP/2 Server");
   Config.Set.Server_Port (Conf, 0);
   Config.Set.Max_Connection (Conf, 5);
   Config.Set.HTTP2_Activated (Conf, True);

   AWS.Server.Log.Start_Error (WS);

   Server.Start (WS, CB'Unrestricted_Access, Conf);
   Ada.Text_IO.Put_Line ("started"); Ada.Text_IO.Flush;

   delay 1.0;

   R := Client.Get (Server.Status.Local_URL (WS),
                    HTTP_Version => HTTPv2);
   Ada.Text_IO.Put_Line ("R-GET  : " & Response.Message_Body (R));

   R := Client.Post (Server.Status.Local_URL (WS),
                    Data         => "Some text as message",
                    HTTP_Version => HTTPv2);
   Ada.Text_IO.Put_Line ("R-POST : " & Response.Message_Body (R));

   Server.Shutdown (WS);
   Ada.Text_IO.Put_Line ("shutdown");
end H2_Hello;
