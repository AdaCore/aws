------------------------------------------------------------------------------
--                             Strings  Cutter                              --
--                                                                          --
--                        Copyright (C) 1995 - 2007                         --
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

package Strings_Cutter is

   type Cut_String is private;

   procedure Create
     (S          :    out Cut_String;
      From       : in     String;
      Separators : in     String);
   --  Create a Cut_String object from the String and using the set of
   --  separators specified.

   procedure Destroy (S : in out Cut_String);
   --  Release all memory associated with a S

   procedure Set
     (S          : in out Cut_String;
      Separators : in     String);
   --  Change the set of separators and recreate the Cut_String object based
   --  on this new set of separators.

   subtype Index_Values is Natural range 0 .. 1_000;

   function Field_Count (S : in Cut_String) return  Index_Values;
   --  Returns the number of fields in S

   function Field
     (S     : in Cut_String;
      Index : in Index_Values) return String;
   --  Returns the Nth field in S. Returns the original string if Index is
   --  set to 0.

private

   type Cut_String_Record;

   type Cut_String is access Cut_String_Record;

end Strings_Cutter;
