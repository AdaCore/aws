------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2000-2020, AdaCore                     --
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

with AWS.Config.Set;
with AWS.Log;
with AWS.Server.Log;

with WS_CB.WebSockets;

procedure WS is

   use Ada;
   Config : AWS.Config.Object;
begin
   Text_IO.Put_Line ("AWS " & AWS.Version);
   Text_IO.Put_Line ("Kill me when you want me to stop...");
   Text_IO.Put_Line ("I will stop in 10 minutes anyway !");

   Text_IO.Put_Line
     ("For the WebSocket demo, open in browser websocket-messages-test.Html");
   Text_IO.Put_Line ("   firefox websocket-messages-test.html");

   AWS.Config.Set.Reuse_Address (Config, True);
   AWS.Config.Set.Server_Host (Config, "127.0.0.1");
   AWS.Config.Set.Server_Port (Config, 1234);
   AWS.Config.Set.Server_Name (Config, "WS Demo");
   AWS.Config.Set.Max_Connection (Config, 5);

   WS_CB.WebSockets.Start;

   AWS.Server.Start
     (WS_CB.WS,
      Config   => Config,
      Callback => WS_CB.Service'Access);

   AWS.Server.Log.Start_Error
     (WS_CB.WS, AWS.Log.Daily, Filename_Prefix => "tlog_error");

   delay 10 * 60.0;

   Text_IO.Put_Line ("ok, let's shutdown...");

   AWS.Server.Shutdown (WS_CB.WS);
   WS_CB.Stop_Push_Server;
end WS;
