------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2007-2009, AdaCore                     --
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
