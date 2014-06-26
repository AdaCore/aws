------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2009-2014, AdaCore                     --
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

with Ada.Command_Line;
with Interfaces.C.Strings;

procedure Mzlib is
   use Ada;

   function zlibVersion return Interfaces.C.Strings.chars_ptr
     with Import, Convention => C, External_Name => "zlibVersion";

   Min : constant String := "1.2.1";
   Ver : constant String := Interfaces.C.Strings.Value (zlibVersion);

begin
   Command_Line.Set_Exit_Status (Command_Line.Success);

   if Ver'Length >= Min'Length then
      Check_Ver : for K in 1 .. Min'Length loop
         if Ver (Ver'First + K - 1) < Min (Min'First + K - 1) then
            Command_Line.Set_Exit_Status (Command_Line.Failure);
            exit Check_Ver;
         end if;
      end loop Check_Ver;
   end if;
end Mzlib;
