------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                            Copyright (C) 2000                            --
--                               Pascal Obry                                --
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

with Ada.Strings;
with Ada.Characters.Handling;

package body AWS.Translater is

   use Ada;

   ----------------
   -- Decode_URL --
   ----------------

   function Decode_URL (Str : in String) return String is
      I, K   : Positive := Str'First;
      Result : String (Str'Range);
   begin
      loop
         if Str (I) = '+' then
            Result (K) := ' ';
            I := I + 1;

         elsif Str (I) = '%'
           and then I + 2 <= Str'Last
           and then Characters.Handling.Is_Hexadecimal_Digit (Str (I + 1))
           and then Characters.Handling.Is_Hexadecimal_Digit (Str (I + 2))
         then
            declare
               Hex_Num : constant String := "16#" & Str (I + 1 .. I + 2) & '#';
            begin
               Result (K) := Character'Val (Natural'Value (Hex_Num));
               I := I + 3;
            end;

         else
            Result (K) := Str (I);
            I := I + 1;
         end if;

         K := K + 1;

         exit when I > Str'Last;
      end loop;

      return Result (Result'First .. K - 1);
   end Decode_URL;

end AWS.Translater;

