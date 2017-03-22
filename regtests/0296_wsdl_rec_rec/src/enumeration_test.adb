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

package body Enumeration_Test is

   procedure Test_0 (Item : Boolean) is
   begin
      null;
   end Test_0;

   procedure Test_1 (Item : Enumeration_Record_Type) is
   begin
      null;
   end Test_1;

   procedure Test_2 (Item : Enumeration_Type) is
   begin
      null;
   end Test_2;

   procedure Test_3
     (Value_Record : Enumeration_Record_Type;
      Value_Param  : Enumeration_Type) is
   begin
      null;
   end Test_3;

   function Test_4 (Item : Enumeration_Record_Type) return Enumeration_Type is
   begin
      return Item.Value;
   end Test_4;

   function Test_5 (Item : Enumeration_Type) return Enumeration_Record_Type is
   begin
      return (Value   => Item,
              Sub_Rec => (Value => Item));
   end Test_5;

   procedure Test_6 (Item : Character) is
   begin
      null;
   end Test_6;

   function Test_7 (Item : Character) return Character is
   begin
      return (case Item is
         when 'a' .. 'y' | 'A' .. 'Y' => Character'Succ (Item),
         when 'z'                     => 'a',
         when 'Z'                     => 'A',
         when others                  => Item);
   end Test_7;

end Enumeration_Test;
