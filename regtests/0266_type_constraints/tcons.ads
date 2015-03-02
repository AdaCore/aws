------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                       Copyright (C) 2015, AdaCore                        --
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

package TCONS is

   type T1 is new Integer range 1 .. 9345;
   procedure Print_T1 (X : T1);

   type T2 is range 1 .. 127;
   procedure Print_T2 (X : T2);

   type T3 is range -128 .. 127;
   procedure Print_T3 (X : T3);

   type T4 is range 0 .. 896;
   procedure Print_T4 (X : T4);

   type T5 is new Integer range 1 .. Integer'Last;
   procedure Print_T5 (X : T5);

   type T6 is new Float range 0.0 .. 1.0;
   procedure Print_T6 (X : T6);

   type T7 is digits 10 range -1.0 .. Long_Float'Last;
   procedure Print_T7 (X : T7);

   type T8 is new String (1 .. 10);
   procedure Print_T8 (X : T8);

   type T9 is new String;
   procedure Print_T9 (X : T9);

   type TA is mod 14;
   procedure Print_TA (X : TA);

   type TB is mod 2**16;
   procedure Print_TB (X : TB);

end TCONS;
