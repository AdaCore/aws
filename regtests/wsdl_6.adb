------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                            Copyright (C) 2003                            --
--                                ACT-Europe                                --
--                                                                          --
--  Authors: Dmitriy Anisimkov - Pascal Obry                                --
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

--  $Id$

package body WSDL_6 is

   ----------------
   -- Echo_Float --
   ----------------

   function Echo_Float (V : in Long_Float) return Long_Float is
   begin
      return V;
   end Echo_Float;

   ----------------------
   -- Echo_Complex_Rec --
   ----------------------

   function Echo_Complex_Rec (C_Rec : in Complex_Rec) return Complex_Rec is
   begin
      return C_Rec;
   end Echo_Complex_Rec;

   -----------------
   -- Echo_Double --
   -----------------

   function Echo_Double (V : in Long_Long_Float) return Long_Long_Float is
   begin
      return V;
   end Echo_Double;

   ------------------
   -- Echo_Boolean --
   ------------------

   function Echo_Boolean (V : in Boolean) return Boolean is
   begin
      return V;
   end Echo_Boolean;

   --------------
   -- Echo_Rec --
   --------------

   function Echo_Rec (V : in Rec) return Rec is
   begin
      return V;
   end Echo_Rec;

   --------------
   -- Echo_Int --
   --------------

   function Echo_Int (V : in Integer) return Integer is
   begin
      return V;
   end Echo_Int;

   ------------------
   -- Echo_New_Rec --
   ------------------

   function Echo_New_Rec (V : in New_Rec) return New_Rec is
   begin
      return V;
   end Echo_New_Rec;

   --------------
   -- Echo_Set --
   --------------

   function Echo_Set (Set : in Set_Of_Int) return Set_Of_Int is
   begin
      return Set;
   end Echo_Set;

   ------------------
   -- Echo_Set_Rec --
   ------------------

   function Echo_Set_Rec (Set : in Set_Of_Rec) return Set_Of_Rec is
   begin
      return Set;
   end Echo_Set_Rec;

   ----------
   -- Next --
   ----------

   function Next (Col : in Color) return Color is
   begin
      if Col = Color'Last then
         return Color'First;
      else
         return Color'Succ (Col);
      end if;
   end Next;

   ----------
   -- Plus --
   ----------

   function Plus (Value : in Natural) return Natural is
   begin
      return Value + 1;
   end Plus;

end WSDL_6;
