------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                       Copyright (C) 2018, AdaCore                        --
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

package body AWS_Test is

   use Ada.Text_IO;

   -------------------
   -- Get_Test_Name --
   -------------------

   function Get_Test_Name (Data : Data_Rec_Type) return String is
   begin
      Put_Line ("int: " & Integer'Image (Data.A_Int));
      Put_Line ("flt: " & Float'Image (Data.A_Flt));
      Put_Line ("str: " & To_String (Data.A_Str));

      return "AWS_TEST";
   end Get_Test_Name;

   --------------------
   -- Get_Test_Name2 --
   --------------------

   function Get_Test_Name2 (Data : Data_Rec_Array_Type) return String is
   begin
      for The_Item in Data'Range loop
         Put_Line ("int: " & Integer'Image (Data(The_Item).A_Int));
         Put_Line ("flt: " & Float'Image (Data(The_Item).A_Flt));
         Put_Line ("str: " & To_String (Data(The_Item).A_Str));
      end loop;

      return "AWS_TEST2";
   end Get_Test_Name2;

end AWS_Test;
