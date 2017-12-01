------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2013-2017, AdaCore                     --
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

with AWS.Config.Set;
with AWS.Default;
with AWS.Net.WebSocket.Registry.Control;
with AWS.Server;

with Autobahn_CB;

procedure Autobahn is

   use Ada;
   use AWS;

   WS     : Server.HTTP;
   Config : AWS.Config.Object;
begin
   AWS.Config.Set.Reuse_Address (Config, True);

   Text_IO.Put_Line
     ("Call me on port" & Positive'Image (AWS.Default.Server_Port));

   Net.WebSocket.Registry.Control.Start;
   Net.WebSocket.Registry.Register ("/echo", Autobahn_CB.Create'Access);

   Server.Start (WS, Config   => Config, Callback => Autobahn_CB.HW_CB'Access);

   Text_IO.Put_Line ("You can now press Q to exit.");
   Server.Wait (Server.Q_Key_Pressed);

   Server.Shutdown (WS);
end Autobahn;
