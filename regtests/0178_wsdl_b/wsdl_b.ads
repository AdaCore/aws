------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2008-2012, AdaCore                     --
--                                                                          --
--  This is free software;  you can redistribute it  and/or modify it       --
--  under terms of the  GNU General Public License as published  by the     --
--  Free Software  Foundation;  either version 3,  or (at your option) any  --
--  later version.  This software is distributed in the hope  that it will  --
--  be useful, but WITHOUT ANY WARRANTY;  without even the implied warranty --
--  of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU     --
--  General Public License for  more details.                               --
--                                                                          --
--  You should have  received  a copy of the GNU General  Public  License   --
--  distributed  with  this  software;   see  file COPYING3.  If not, go    --
--  to http://www.gnu.org/licenses for a complete copy of the license.      --
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
