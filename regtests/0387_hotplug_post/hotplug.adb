------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                       Copyright (C) 2025, AdaCore                        --
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

with AWS.Client.Hotplug;
with AWS.Messages;
with AWS.Net;
with AWS.Response;
with AWS.Server;

with Data;
with Hotplug_CB;

procedure Hotplug is

   use Ada;
   use type AWS.Messages.Status_Code;

   Response : AWS.Response.Data;

begin
   Text_IO.Put_Line ("Hotplug - AWS");

   AWS.Server.Start
     (Data.H_WS, "Hotplug",
      Port           => 1235,
      Max_Connection => 3,
      Callback       => Hotplug_CB.Hotplug'Access);

   Response := AWS.Client.Hotplug.Register
     ("hp_test", Data.Password,
      "http://localhost:2222",
      Data.Filter,
      "http://" & AWS.Net.Host_Name & ":1235/");

   if AWS.Response.Status_Code (Response) /= AWS.Messages.S200 then
      Text_IO.Put_Line
        ("Register Error : " & AWS.Response.Message_Body (Response));
   end if;
end Hotplug;
