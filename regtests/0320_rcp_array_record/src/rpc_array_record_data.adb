------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                       Copyright (C) 2019, AdaCore                        --
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

with Ada.Text_IO;

package body RPC_Array_Record_Data is

   use Ada;

   ------------
   -- Test_1 --
   ------------

   function Test_1 (Item : Enumeration_Record_Type) return Enumeration_Type is
   begin
      Text_IO.Put_Line
        ("Test 1: "
         & Enumeration_Type'Image (Item.Value)
         & ", "
         & Enumeration_Type'Image (Item.List.Item (1).Value));
      return Item.Value;
   end Test_1;

   ------------
   -- Test_2 --
   ------------

   function Test_2 (Item : Enumeration_Type) return Enumeration_Record_Type is
      L : Sub_Record_List_Type (1 .. 2) := (1 => (Value => Third),
                                            2 => (Value => Item));
   begin
      Text_IO.Put_Line ("Test 2: " & Enumeration_Type'Image (Item));
      return (Value => Item,
              List  => Sub_Record_List_Type_Safe_Pointer.To_Safe_Pointer (L));
   end Test_2;

end RPC_Array_Record_Data;
