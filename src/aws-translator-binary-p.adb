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

--  Portable convertion between String and Stream_Element_Array.

separate (AWS.Translator)

package body Binary is

   -----------------------------
   -- To_Stream_Element_Array --
   -----------------------------

   function To_Stream_Element_Array
     (Data : in String)
     return Stream_Element_Array
   is
      Result : Stream_Element_Array
        (Stream_Element_Offset (Data'First)
         .. Stream_Element_Offset (Data'Last));
   begin
      for K in Data'Range loop
         Result (Stream_Element_Offset (K)) := Character'Pos (Data (K));
      end loop;
      return Result;
   end To_Stream_Element_Array;

   ---------------
   -- To_String --
   ---------------

   function To_String
     (Data : in Stream_Element_Array)
     return String
   is
      Result : String (Integer (Data'First) .. Integer (Data'Last));
   begin
      for K in Data'Range loop
         Result (Integer (K)) := Character'Val (Data (K));
      end loop;
      return Result;
   end To_String;

end Binary;