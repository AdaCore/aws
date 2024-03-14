------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2000-2013, AdaCore                     --
--                                                                          --
--  This library is free software;  you can redistribute it and/or modify   --
--  it under terms of the  GNU General Public License  as published by the  --
--  Free Software  Foundation;  either version 3,  or (at your  option) any --
--  later version. This library is distributed in the hope that it will be  --
--  useful, but WITHOUT ANY WARRANTY;  without even the implied warranty of --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                    --
--                                                                          --
--  As a special exception under Section 7 of GPL version 3, you are        --
--  granted additional permissions described in the GCC Runtime Library     --
--  Exception, version 3.1, as published by the Free Software Foundation.   --
--                                                                          --
--  You should have received a copy of the GNU General Public License and   --
--  a copy of the GCC Runtime Library Exception along with this program;    --
--  see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see   --
--  <http://www.gnu.org/licenses/>.                                         --
--                                                                          --
--  As a special exception, if other files instantiate generics from this   --
--  unit, or you link this unit with other files to produce an executable,  --
--  this  unit  does not  by itself cause  the resulting executable to be   --
--  covered by the GNU General Public License. This exception does not      --
--  however invalidate any other reasons why the executable file  might be  --
--  covered by the  GNU Public License.                                     --
------------------------------------------------------------------------------

pragma Ada_2012;

function AWS.Hotplug.Get_Status
  (Filters : Filter_Set) return Templates_Parser.Translate_Set
is

   use Templates_Parser;

   --  Avoid : may be referenced before it has a value
   Regexp : Vector_Tag with Warnings => Off;
   URL    : Vector_Tag with Warnings => Off;

   Result : Translate_Set;

begin
   for K in 1 .. Filter_Table.Length (Filters.Set) loop
      declare
         Item : constant Filter_Data
           := Filter_Table.Element (Filters.Set, Positive (K));
      begin
         Regexp := Regexp & Item.Regexp_Str;
         URL    := URL    & Item.URL;
      end;
   end loop;

   Insert (Result, Assoc ("HP_REGEXP_V", Regexp));
   Insert (Result, Assoc ("HP_URL_V",    URL));

   return Result;
end AWS.Hotplug.Get_Status;
