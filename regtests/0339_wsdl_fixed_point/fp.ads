------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                      Copyright (C) 2021, AdaCore                         --
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

package Fp is

   type Toy_Type is record
      One : Integer;
      Two : Integer;
   end record;

   type Fixed_Type is
     delta 1.0 / 400.0 range -32_768.0 / 400.0 .. 32_767.0 / 400.0;

   type Float_Type is digits 3 range 1.0 .. 2.0;

   function Get_V (P1 : Fixed_Type; P2 : Float_Type) return Toy_Type;

end Fp;
