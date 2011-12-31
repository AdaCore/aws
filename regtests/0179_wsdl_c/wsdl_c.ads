------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2007-2012, AdaCore                     --
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

package WSDL_C is

   type My_Int is range 1 .. 100;
   subtype My_Number is My_Int range 1 .. 2;
   subtype Big_Number is Long_Long_Integer;

   type S_Int is new Positive range 1 .. 4;
   type L_Int is new Long_Integer  range 1 .. Long_Integer'Last;

   type My_Float is digits 6;
   type My_Double is digits 15;
   type My_Float2 is new Float;

   type Mod1 is mod 10;
   type Mod2 is mod 6543;
   type Mod3 is mod 1_987_987_543;

   type R is record
      C1 : My_Int;
      C2 : Positive;
      C3 : Natural;
      C4 : My_Number;
      C5 : Big_Number;
      C6 : My_Float;
      C7 : My_Float2;
      C8 : S_Int;
      C9 : Float;
   end record;

   procedure P
     (A : R;
      B : My_Int;
      C : My_Float;
      D : My_Number;
      E : Big_Number;
      F : S_int);

end WSDL_C;
