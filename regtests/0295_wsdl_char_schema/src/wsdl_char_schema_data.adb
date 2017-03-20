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

package body WSDL_Char_Schema_Data is

   use Ada.Text_IO;

   ------------
   -- Update --
   ------------

   function Update (V : Data) return Return_Type is
      use Ada.Text_IO;
   begin
      Put_Line ("D : " & Return_Type'Image (V.D));
      return One;
   end Update;

   ----------
   -- Call --
   ----------

   function Call (V : Character) return Character is
   begin
      Put_Line ("Call.V : " & V);
      return Character'Succ (V);
   end Call;

end WSDL_Char_Schema_Data;
