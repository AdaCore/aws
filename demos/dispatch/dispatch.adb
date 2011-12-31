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

with Ada.Text_IO;

with AWS.Config;
with AWS.Server;
with AWS.Services.Dispatchers.URI;

with Dispatch_CB;

procedure Dispatch is

   use Ada;
   use AWS.Services;

   H  : AWS.Services.Dispatchers.URI.Handler;

   WS : AWS.Server.HTTP;

begin
   Text_IO.Put_Line ("AWS " & AWS.Version);
   Text_IO.Put_Line ("Enter 'q' key to exit...");

   Dispatchers.URI.Register (H, "/disp", Dispatch_CB.HW_CB'Access);

   AWS.Server.Start (WS, Dispatcher => H, Config => AWS.Config.Get_Current);

   --  Wait for 'q' key pressed...

   AWS.Server.Wait (AWS.Server.Q_Key_Pressed);

   --  Close servers.

   AWS.Server.Shutdown (WS);
end Dispatch;
