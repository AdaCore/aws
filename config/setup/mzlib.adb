------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2009-2011, AdaCore                     --
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

with Ada.Command_Line;
with Interfaces.C.Strings;

procedure Mzlib is
   use Ada;

   function zlibVersion return Interfaces.C.Strings.chars_ptr;
   pragma Import (C, zlibVersion, "zlibVersion");

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
