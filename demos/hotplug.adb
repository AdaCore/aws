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

--  See main.adb on how to run this demo.

with Ada.Command_Line;
with Ada.Text_IO;

with AWS.Communication.Client;
with AWS.Net;
with AWS.Response;
with AWS.Server.Hotplug;

with Hotplug_CB;

procedure Hotplug is

   use Ada;

   procedure Wait_Terminate;
   --  Wait for module to terminate and unregister it.

   Response : AWS.Response.Data;

   Filter   : constant String := ".*AWS.*";

   --------------------
   -- Wait_Terminate --
   --------------------

   procedure Wait_Terminate is
      C : Character;
   begin
      loop
         Text_IO.Get_Immediate (C);
         exit when C = 'T';
      end loop;

      Response := AWS.Communication.Client.Send_Message
        (Command_Line.Argument (1), 2222,
         AWS.Server.Hotplug.Unregister_Message,
         AWS.Communication.Parameters (Filter));
   end Wait_Terminate;

   WS : AWS.Server.HTTP;

begin
   if Command_Line.Argument_Count /= 1 then
      Text_IO.Put_Line ("Syntax: hotplug <main_server_hostname>");
      Text_IO.New_Line;
      return;
   end if;

   Text_IO.Put_Line ("AWS " & AWS.Version);
   Text_IO.Put_Line ("Enter T to terminate...");
   Text_IO.Put_Line ("Hotplug module linked to server " &
                     Command_Line.Argument (1));

   AWS.Server.Start (WS, "Hotplug",
                     Admin_URI      => "/Admin-Page",
                     Port           => 1235,
                     Max_Connection => 3,
                     Callback       => Hotplug_CB.Hotplug'Access);

   Response := AWS.Communication.Client.Send_Message
     (Command_Line.Argument (1), 2222,
      AWS.Server.Hotplug.Register_Message,
      AWS.Communication.Parameters
       (Filter, "http://" & AWS.Net.Host_Name & ":1235/"));

   Wait_Terminate;

   AWS.Server.Shutdown (WS);
end Hotplug;
