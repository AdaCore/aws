------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                       Copyright (C) 2017, AdaCore                        --
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

package TCONS2 is

   type T1 is range -20_000 .. +20_000;
   type T2 is range -20_000 ..  20_000;
   type T3 is new Integer range -20_000 .. +20_000;
   type T4 is new Float range -1.5 .. +1.5;

   type Rec is record
      V1 : T1;
      V2 : T2;
      V3 : T3;
      V4 : T4;
   end record;

   procedure Print (R : Rec);

end TCONS2;
