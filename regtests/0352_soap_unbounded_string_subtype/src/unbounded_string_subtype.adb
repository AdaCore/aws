------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                       Copyright (C) 2022, AdaCore                        --
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

package body Unbounded_String_Subtype is

   use Ada;

   ----------------
   -- Test_Rec --
   ----------------

   procedure Test_Rec (R : Rec) is
   begin
      Text_IO.Put_Line ("N " & R.N'Img);
      Text_IO.Put_Line ("K " & To_String (R.Key));
      Text_IO.Put_Line ("V " & To_String (R.Value));
   end Test_Rec;

end Unbounded_String_Subtype;
