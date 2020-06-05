------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                       Copyright (C) 2020, AdaCore                        --
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
with AWS.Utils;

procedure Prog_Config is

   use Ada;
   use AWS;

   Conf : constant Config.Object := Config.Get_Current;
   --  Config as read from the ini files

   -------------
   -- Display --
   -------------

   procedure Display (O : Config.Object) is
   begin
      Text_IO.Put_Line
        ("Disable Program Ini : "
         & Boolean'Image (Config.Disable_Program_Ini));
      Text_IO.Put_Line (Config.Server_Name (O));
      Text_IO.Put_Line (Config.WWW_Root (O));
      Text_IO.Put_Line (Utils.Image (Config.Server_Port (O)));
      Text_IO.Put_Line (Utils.Image (Config.Max_Connection (O)));
      Text_IO.Put_Line (Config.Directory_Browser_Page (O));
      Text_IO.New_Line;
   end Display;

begin
   Display (Conf);
end Prog_Config;
