------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2003-2012, AdaCore                     --
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

with Ada.Strings.Unbounded;

package WSDL_4 is

   use Ada.Strings.Unbounded;

   type My_Int is new Integer;
   type My_Float is new Long_Float;

   subtype S_My_Int is Integer;
   subtype S_My_Float is Long_Float;

   type Rec is record
      Item1 : My_Int;
      Item2 : My_Float;
      Item3 : S_My_Int;
      Item4 : S_My_Float;
      Item5 : Unbounded_String;
      Item6 : Character;
   end record;

   procedure Try
     (Param1 : My_Int;
      Param2 : My_Float;
      Param3 : S_My_Int;
      Param4 : S_My_Float;
      Param5 : Rec);

   function Try2 (Param1 : Integer; Param2 : String) return Rec;

   function Try3 (Param1 : My_Float; Param2 : S_My_Int) return S_My_Float;

   function Try4 return My_Int;

end WSDL_4;
