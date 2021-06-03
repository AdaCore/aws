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

package Demo is

   L1 : constant := 2;
   U1 : constant := 6;

   type T1 is array (L1 .. U1) of Boolean;

   L2 : constant Integer := 1;
   U2 : constant Integer := 3;

   type T2 is array (L2 .. U2) of Integer;

   L3 : Integer := 7;
   U3 : Integer := 18 + 1;

   type T3 is array (L3 .. U3) of Float;

end Demo;
