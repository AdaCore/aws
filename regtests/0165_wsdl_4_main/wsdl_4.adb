------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2003-2008, AdaCore                     --
--                                                                          --
--  This library is free software; you can redistribute it and/or modify    --
--  it under the terms of the GNU General Public License as published by    --
--  the Free Software Foundation; either version 2 of the License, or (at   --
--  your option) any later version.                                         --
--                                                                          --
--  This library is distributed in the hope that it will be useful, but     --
--  WITHOUT ANY WARRANTY; without even the implied warranty of              --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU       --
--  General Public License for more details.                                --
--                                                                          --
--  You should have received a copy of the GNU General Public License       --
--  along with this library; if not, write to the Free Software Foundation, --
--  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.          --
--                                                                          --
--  As a special exception, if other files instantiate generics from this   --
--  unit, or you link this unit with other files to produce an executable,  --
--  this  unit  does not  by itself cause  the resulting executable to be   --
--  covered by the GNU General Public License. This exception does not      --
--  however invalidate any other reasons why the executable file  might be  --
--  covered by the  GNU Public License.                                     --
------------------------------------------------------------------------------

with Ada.Strings.Unbounded;

package body WSDL_4 is

   L_Integer : My_Int;
   L_Float   : My_Float;

   ---------
   -- Try --
   ---------

   procedure Try
     (Param1 : in My_Int;
      Param2 : in My_Float;
      Param3 : in S_My_Int;
      Param4 : in S_My_Float;
      Param5 : in Rec) is
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
