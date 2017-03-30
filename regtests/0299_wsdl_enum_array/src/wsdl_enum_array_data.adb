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

package body WSDL_Enum_Array_Data is

   use Ada;

   ------------
   -- Test_1 --
   ------------

   function Test_1 (Item : Record_Type) return Record_Type is
      R : Record_Type;
   begin
      R.Value := Item.Value + 1;
      R.C     := Character'Succ (Item.C);
      R.L     := Item.L;

      R.L.Item (1) := Second;
      return R;
   end Test_1;

end WSDL_Enum_Array_Data;
