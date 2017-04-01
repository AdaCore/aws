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

package body Types is

   use Ada;

   -----------
   -- Print --
   -----------

   procedure Print (X : Integer) is
   begin
      Text_IO.Put_Line (Integer'Image (X));
   end Print;

   -----------------
   -- Print_Small --
   -----------------

   procedure Print_Small (X : Small_Integer) is
   begin
      Text_IO.Put_Line (Small_Integer'Image (X));
   end Print_Small;

   -----------
   -- Image --
   -----------

   function Image (X : Integer) return String is
   begin
      return Integer'Image (X);
   end Image;

end Types;
