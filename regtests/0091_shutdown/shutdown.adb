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
with AWS.Server;

with Srv;

procedure Shutdown is
   use Ada;
   use AWS;

   WS : Server.HTTP;

begin
   Text_IO.Put_Line ("start"); Text_IO.Flush;

   Server.Start (Srv.WS, "demo", Srv.CB'Access, Port => 0);

   Server.Start (WS, "demo", Srv.CB'Access, Port => 0);

   delay 2.0;

   Text_IO.Put_Line ("shutdown"); Text_IO.Flush;
   Server.Shutdown (Srv.WS);
   Server.Shutdown (WS);

   Text_IO.Put_Line ("wait..."); Text_IO.Flush;
   Server.Wait (Server.No_Server);
end Shutdown;
