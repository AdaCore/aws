------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2000-2001                          --
--                                ACT-Europe                                --
--                                                                          --
--  Authors: Dmitriy Anisimkov - Pascal Obry                                --
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

--  Fast convertion between String and Stream_Element_Array.
--  Only for Ada compilers and platforms, where it is possible.

with Ada.Unchecked_Conversion;

separate (AWS.Translator)

package body Conversion is

   -----------------------------
   -- To_Stream_Element_Array --
   -----------------------------

   function To_Stream_Element_Array
     (Data : in String)
     return Stream_Element_Array
   is

      subtype Fixed_String is String (Data'First .. Data'Last);

      subtype Fixed_Array is Stream_Element_Array
        (Stream_Element_Offset (Data'First)
         .. Stream_Element_Offset (Data'Last));

      function To_Stream_Elements is
        new Ada.Unchecked_Conversion (Fixed_String, Fixed_Array);

   begin
      return To_Stream_Elements (Data);
   end To_Stream_Element_Array;

   ---------------
   -- To_String --
   ---------------

   function To_String
     (Data : in Stream_Element_Array)
     return String
   is

      subtype Fixed_String is String (Integer (Data'First)
        .. Integer (Data'Last));

      subtype Fixed_Array is Stream_Element_Array
         (Data'First .. Data'Last);

      function To_Characters is
        new Ada.Unchecked_Conversion (Fixed_Array, Fixed_String);

   begin
      return To_Characters (Data);
   end To_String;

end Conversion;