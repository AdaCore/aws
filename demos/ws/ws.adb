------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2000-2012, AdaCore                     --
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

--  This is a way to build a simple HTML page server. Just a part of a full
--  Web server.

with Ada.Text_IO;

with AWS.Server;

with WS_CB;

procedure WS is

   use Ada;

begin
   Text_IO.Put_Line ("AWS " & AWS.Version);
   Text_IO.Put_Line ("Kill me when you want me to stop...");
   Text_IO.Put_Line ("I will stop in 5 minutes anyway !");

   AWS.Server.Start (WS_CB.WS, "WS Demo",
                     Port           => 1234,
                     Callback       => WS_CB.Service'Access,
                     Max_Connection => 5);

   delay 5 * 60.0;

   Text_IO.Put_Line ("ok, let's shutdown...");

   AWS.Server.Shutdown (WS_CB.WS);
   WS_CB.Stop_Push_Server;
end WS;
