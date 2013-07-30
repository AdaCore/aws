------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                       Copyright (C) 2013, AdaCore                        --
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

procedure Cfilename is

   use AWS;

   WS : Server.HTTP;

   function CB (Request : Status.Data) return Response.Data is
   begin
      return Response.File
        (MIME.Text_HTML, "content.html", Disposition => Response.Inline);
   end CB;

   R : Response.Data;

begin
   Server.Start
     (WS, "cfilename", CB'Unrestricted_Access, Port => 0, Max_Connection => 5);
   Ada.Text_IO.Put_Line ("started"); Ada.Text_IO.Flush;

   delay 1.0;

   R := Client.Get (Server.Status.Local_URL (WS));
   Ada.Text_IO.Put_Line ("R : " & Response.Filename (R));

   Server.Shutdown (WS);
   Ada.Text_IO.Put_Line ("shutdown");
end Cfilename;
