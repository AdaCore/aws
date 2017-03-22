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

with Enumeration_Types;

package Enumeration_Test is

   use Enumeration_Types;

   procedure Test_0 (Item : Boolean);

   procedure Test_1 (Item : Enumeration_Record_Type);

   procedure Test_2 (Item : Enumeration_Type);

   procedure Test_3
     (Value_Record : Enumeration_Record_Type;
      Value_Param  : Enumeration_Type);

   function Test_4 (Item : Enumeration_Record_Type) return Enumeration_Type;

   function Test_5 (Item : Enumeration_Type) return Enumeration_Record_Type;

   procedure Test_6 (Item : Character);

   function Test_7 (Item : Character) return Character;

end Enumeration_Test;
