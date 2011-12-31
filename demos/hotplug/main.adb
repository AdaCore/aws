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

--  How to run this demo ?
--
--  On the server side:
--  $ main
--
--  On the same computer, or another one:
--  $ hotplug <main_hostname>
--
--  On the client side:
--  * launch your Web Browser (this should work with any browser)
--  * enter the URL : http://<servername>:1234/
--
--  You can ask for whatever URI
--  http://<servername>:1234/abc
--
--  If you can ask for whatever URI containing string "AWS" the hotplug server
--  will be triggered:
--  http://<servername>:1234/AWS_hotplug_is_great
--

with Ada.Text_IO;

with AWS.Server.Hotplug;
with AWS.Server.Log;

with Hotplug_CB;

procedure Main is

   use Ada;

   WS : aliased AWS.Server.HTTP;

begin
   Text_IO.Put_Line ("AWS " & AWS.Version);
   Text_IO.Put_Line ("Kill me when you want me to stop...");

   AWS.Server.Start
     (WS, "Main",
      Admin_URI      => "/Admin-Page",
      Port           => 1234,
      Max_Connection => 3,
      Callback       => Hotplug_CB.Main'Access);

   AWS.Server.Log.Start (WS);
   AWS.Server.Log.Start_Error (WS);

   AWS.Server.Hotplug.Activate
     (WS'Unchecked_Access, 2222, "hotplug_module.ini");

   AWS.Server.Wait;

exception
   when others =>
      Text_IO.Put_Line ("Main error...");
      AWS.Server.Hotplug.Shutdown;
      AWS.Server.Shutdown (WS);
end Main;
