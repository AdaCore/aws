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

with Ada.Text_IO;

package body Array_Rec_Enum_Data is

   use Ada;

   -------------
   -- Test_AL --
   -------------

   procedure Test_AL (Par1 : Array_Enum_Type; Par2 : Enum_List_Type) is
   begin
      null;
   end Test_AL;

   ----------------
   -- Test_Array --
   ----------------

   procedure Test_Array (Value : Array_Enum_Type) is
   begin
      for C in Value.Value.Item'Range loop
         Text_IO.Put_Line
           (Integer'Image (C) & " => "
            & Enum'Image (Value.Value.Item (C)));
      end loop;
   end Test_Array;

   ---------------
   -- Test_List --
   ---------------

   procedure Test_List (Value : Enum_List_Type) is
   begin
      null;
   end Test_List;

end Array_Rec_Enum_Data;
