------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2013-2016, AdaCore                     --
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

--  A simple WebSocket demo using AWS framework

with Ada.Text_IO;

with AWS.Config.Set;
with AWS.Default;
with AWS.Net.Log;
with AWS.Net.WebSocket.Registry.Control;
with AWS.Server;
with AWS.Status;
with AWS.Templates;

with WebSock_CB;

with Candy_Mine;
with Notification_Center;

procedure Candy is
   use Ada;
   use AWS;
   use AWS.Config;
   use type AWS.Net.Socket_Access;

   WS           : Server.HTTP;
   Config       : AWS.Config.Object;

   Candy_Miners : array (1 .. 15) of Candy_Mine.Candy_Miner;

begin
   --  To analyse the send/received data uncomment the line below
   --  Net.Log.Start (WebSock_CB.W_Log'Access);

   Candy_Miners (1).Start ("Candy");
   Candy_Miners (2).Start ("Lollipop");
   Candy_Miners (3).Start ("Snickers");
   Candy_Miners (4).Start ("Bubble Tape");
   Candy_Miners (5).Start ("Gummi Bears");
   Candy_Miners (6).Start ("Corn Candy");
   Candy_Miners (7).Start ("Cherry Ripe");
   Candy_Miners (8).Start ("Chocolate");
   Candy_Miners (9).Start ("Skittles");
   Candy_Miners (10).Start ("Bullets");
   Candy_Miners (11).Start ("Minties");
   Candy_Miners (12).Start ("Kool Mints");
   Candy_Miners (13).Start ("Jaffas");
   Candy_Miners (14).Start ("Fantales");
   Candy_Miners (15).Start ("Clinkers");

   AWS.Config.Set.Reuse_Address (Config, True);
   Net.WebSocket.Registry.Control.Start;
   Net.WebSocket.Registry.Register ("/candy", WebSock_CB.Create'Access);

   Server.Start (WS, Config => Config, Callback => WebSock_CB.HW_CB'Access);

   Text_IO.Put_Line
     ("Call me on port" & Positive'Image (AWS.Default.Server_Port));
   Text_IO.Put_Line ("You can now press Q to exit.");
   Server.Wait (Server.Q_Key_Pressed);

   Server.Shutdown (WS);
end Candy;
