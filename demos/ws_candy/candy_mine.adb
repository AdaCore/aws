------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                        Copyright (C) 2013, AdaCore                       --
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

with Ada.Strings.Unbounded;
with Ada.Text_IO;

with Notification_Center;

package body Candy_Mine is

   use Ada.Strings.Unbounded;

   -----------------
   -- Candy_Miner --
   -----------------

   task body Candy_Miner is
      Key_US : Unbounded_String;
   begin
      accept Start (Key : String) do
         Key_US := To_Unbounded_String (Key);
      end Start;

      loop
         delay 0.1;
         --  Notification_Center.Protected_Center.Notify (To_String (Key_US));
         Notification_Center.Unprotected_Notify (To_String (Key_US));
      end loop;
   end Candy_Miner;

end Candy_Mine;
