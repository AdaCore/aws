------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                            Copyright (C) 2003                            --
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

with Ada.Text_IO;

package body WSDL_3 is

   use Ada;

   ----------------
   -- Image_Rec1 --
   ----------------

   function Image_Rec1 (Rec : in Rec1) return String is
   begin
      return "(" & Integer'Image (Rec.Item1)
        & ", " & Natural'Image (Rec.Item2)
        & ", " & Positive'Image (Rec.Item3)
        & ")";
   end Image_Rec1;

   ----------------
   -- Image_Rec2 --
   ----------------

   function Image_Rec2 (Rec : in Rec2) return String is
   begin
      return "(" & Image_Rec1 (Rec.Field1)
--        & ", " & Rec.Field2
        & ", " & To_String (Rec.Field3)
        & ", " & Long_Float'Image (Rec.Field4)
        & ")";
   end Image_Rec2;

end WSDL_3;
