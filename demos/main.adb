------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2000-2001                          --
--                                ACT-Europe                                --
--                                                                          --
--  Authors: Dmitriy Anisimkov - Pascal Obry                                --
--                                                                          --
--  This library is free software; you can redistribute it and/or modify    --
--  it under the terms of the GNU General Public License as published by    --
--  the Free Software Foundation; either version 2 of the License, or (at   --
--  your option) any later version.                                         --
--                                                                          --
--  This library is distributed in the hope that it will be useful, but     --
--  WITHOUT ANY WARRANTY; without even the implied warranty of              --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU       --
--  General Public License for more details.                                --
--                                                                          --
--  You should have received a copy of the GNU General Public License       --
--  along with this library; if not, write to the Free Software Foundation, --
--  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.          --
--                                                                          --
--  As a special exception, if other files instantiate generics from this   --
--  unit, or you link this unit with other files to produce an executable,  --
--  this  unit  does not  by itself cause  the resulting executable to be   --
--  covered by the GNU General Public License. This exception does not      --
--  however invalidate any other reasons why the executable file  might be  --
--  covered by the  GNU Public License.                                     --
------------------------------------------------------------------------------

--  $Id$

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

with Hotplug_CB;

procedure Main is

   use Ada;

   WS : aliased AWS.Server.HTTP;

begin
   Text_IO.Put_Line ("AWS " & AWS.Version);
   Text_IO.Put_Line ("Kill me when you want me to stop...");

   AWS.Server.Start (WS, "Main",
                     Admin_URI      => "/Admin-Page",
                     Port           => 1234,
                     Max_Connection => 3,
                     Callback       => Hotplug_CB.Main'Access);

   AWS.Server.Hotplug.Activate (WS'Unchecked_Access, 2222);

   AWS.Server.Start_Log (WS);

   AWS.Server.Wait;
end Main;
