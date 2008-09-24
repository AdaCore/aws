------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2000-2008, AdaCore                     --
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

function AWS.Hotplug.Get_Status
  (Filters : in Filter_Set) return Templates_Parser.Translate_Set
is

   use Templates_Parser;

   Regexp : Vector_Tag;
   URL    : Vector_Tag;

   --  Avoid : may be referenced before it has a value
   pragma Warnings (Off, Regexp);
   pragma Warnings (Off, URL);

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
