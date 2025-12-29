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

with AWS.Net;
with AWS.Server.Hotplug;

with Data;
with Hotplug_CB;

procedure Main is

   use Ada;

   WS : aliased AWS.Server.HTTP;

begin
   Text_IO.Put_Line ("Main - AWS");

   AWS.Server.Start
     (Data.M_WS, "Main",
      Port           => 0,
      Max_Connection => 3,
      Callback       => Hotplug_CB.Main'Access);

   Create_Module_Ini : declare
      File : Text_IO.File_Type;
   begin
      Text_IO.Create (File, Text_IO.Out_File, "hotplug_module.ini");
      Text_IO.Put_Line (File,
                        "hp_test:f8de61f1f97df3613fbe29b031eb52c6:"
                        & AWS.Net.Host_Name & ":1235");
      Text_IO.Close (File);
   end Create_Module_Ini;

   AWS.Server.Hotplug.Activate
     (Data.M_WS'Unchecked_Access, 2222, "hotplug_module.ini");
end Main;
