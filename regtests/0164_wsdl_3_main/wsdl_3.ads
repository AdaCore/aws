------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2003-2009, AdaCore                     --
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

with Ada.Strings.Unbounded;

with SOAP.Utils;

package WSDL_3 is

   use Ada.Strings.Unbounded;

   type Rec1 is record
      Item1 : Integer;
      Item2 : Natural;
      Item3 : Positive;
   end record;

   type Rec2 is record
      Field1 : Rec1;
      Field2 : Character;
      Field3 : Unbounded_String;
      Field4 : Long_Float;
   end record;

   type My_Set is array (Positive range <>) of Integer;
   type My_Set_Access is access My_Set;

   package My_Set_Safe_Pointer is
      new SOAP.Utils.Safe_Pointers (My_Set, My_Set_Access);

   type Rec3 is record
      S : My_Set_Safe_Pointer.Safe_Pointer;
   end record;

   function Image_Rec1 (Rec : Rec1) return String;

   function Image_Rec2 (Rec : Rec2) return String;

   function Image_Rec3 (Rec : Rec3) return String;

end WSDL_3;
