------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                       Copyright (C) 2015, AdaCore                        --
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

package body TCONS is

   use Ada;

   --------------
   -- Print_T1 --
   --------------

   procedure Print_T1 (X : T1) is
   begin
      Text_IO.Put_Line (T1'Image (X));
   end Print_T1;

   --------------
   -- Print_T2 --
   --------------

   procedure Print_T2 (X : T2) is
   begin
      Text_IO.Put_Line (T2'Image (X));
   end Print_T2;

   --------------
   -- Print_T3 --
   --------------

   procedure Print_T3 (X : T3) is
   begin
      Text_IO.Put_Line (T3'Image (X));
   end Print_T3;

   --------------
   -- Print_T4 --
   --------------

   procedure Print_T4 (X : T4) is
   begin
      Text_IO.Put_Line (T4'Image (X));
   end Print_T4;

   --------------
   -- Print_T5 --
   --------------

   procedure Print_T5 (X : T5) is
   begin
      Text_IO.Put_Line (T5'Image (X));
   end Print_T5;

   --------------
   -- Print_T6 --
   --------------

   procedure Print_T6 (X : T6) is
   begin
      Text_IO.Put_Line (T6'Image (X));
   end Print_T6;

   --------------
   -- Print_T7 --
   --------------

   procedure Print_T7 (X : T7) is
   begin
      Text_IO.Put_Line (T7'Image (X));
   end Print_T7;

   --------------
   -- Print_T8 --
   --------------

   procedure Print_T8 (X : T8) is
   begin
      Text_IO.Put_Line (String (X));
   end Print_T8;

   --------------
   -- Print_T9 --
   --------------

   procedure Print_T9 (X : T9) is
   begin
      Text_IO.Put_Line (String (X));
   end Print_T9;

   --------------
   -- Print_TA --
   --------------

   procedure Print_TA (X : TA) is
   begin
      Text_IO.Put_Line (TA'Image (X));
   end Print_TA;

   --------------
   -- Print_TB --
   --------------

   procedure Print_TB (X : TB) is
   begin
      Text_IO.Put_Line (TB'Image (X));
   end Print_TB;

end TCONS;
