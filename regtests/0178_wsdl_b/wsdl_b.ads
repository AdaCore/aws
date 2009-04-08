------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2008-2009, AdaCore                     --
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
--  covered by the GNU Public License.                                      --
------------------------------------------------------------------------------

with WSDL_B_Pck;

package WSDL_B is

   type Toto is array (Positive range 1 .. 12) of Integer;

   type Set_Of_Int is array (Positive range <>) of Integer;

   type XYZ is new Set_Of_Int;

   type ABC is new Set_Of_Int (1 .. 2);

   type Set_Of_Int_10 is new Set_Of_Int (1 .. 10);

   subtype Set_Of_Int_20 is Set_Of_Int (1 .. 20);

   type Rec is record
      C : ABC;
      D : WSDL_B_Pck.Arr1;
      E : WSDL_B_Pck.Arr2;
   end record;

   type Complex_Rec is record
      R   : Wsdl_B_Pck.Rec;
      SI  : Set_Of_Int (1 .. 10);
      SI2 : Set_Of_Int_10;
      SI3 : Set_Of_Int_20;
   end record;

   function Echo_Complex_Rec (C_Rec : Complex_Rec) return Complex_Rec;

end WSDL_B;
