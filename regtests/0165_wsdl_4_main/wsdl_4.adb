------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2003-2012, AdaCore                     --
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

with Ada.Strings.Unbounded;

package body WSDL_4 is

   L_Integer : My_Int;
   L_Float   : My_Float;

   ---------
   -- Try --
   ---------

   procedure Try
     (Param1 : My_Int;
      Param2 : My_Float;
      Param3 : S_My_Int;
      Param4 : S_My_Float;
      Param5 : Rec) is
   begin
      L_Integer := Param1;
      L_Float   := Param2;
   end Try;

   ----------
   -- Try2 --
   ----------

   function Try2 (Param1 : Integer; Param2 : String) return Rec is
      R : constant Rec
        := (L_Integer, L_Float, Param1, 23.67,
            To_Unbounded_String (Param2), '@');
   begin
      return R;
   end Try2;

   ----------
   -- Try3 --
   ----------

   function Try3 (Param1 : My_Float; Param2 : S_My_Int) return S_My_Float is
   begin
      return Long_Float (Param1) + Long_Float (Param2);
   end Try3;

   ----------
   -- Try4 --
   ----------

   function Try4 return My_Int is
   begin
      return 432;
   end Try4;

end WSDL_4;
