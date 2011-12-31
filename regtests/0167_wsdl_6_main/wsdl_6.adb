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

package body WSDL_6 is

   ----------------
   -- Echo_Float --
   ----------------

   function Echo_Float (V : Float) return Float is
   begin
      return V;
   end Echo_Float;

   ----------------------
   -- Echo_Complex_Rec --
   ----------------------

   function Echo_Complex_Rec (C_Rec : Complex_Rec) return Complex_Rec is
   begin
      return C_Rec;
   end Echo_Complex_Rec;

   -----------------
   -- Echo_Double --
   -----------------

   function Echo_Double (V : Long_Float) return Long_Float is
   begin
      return V;
   end Echo_Double;

   ------------------
   -- Echo_Boolean --
   ------------------

   function Echo_Boolean (V : Boolean) return Boolean is
   begin
      return V;
   end Echo_Boolean;

   --------------
   -- Echo_Rec --
   --------------

   function Echo_Rec (V : Rec) return Rec is
   begin
      return V;
   end Echo_Rec;

   --------------
   -- Echo_Int --
   --------------

   function Echo_Int (V : Integer) return Integer is
   begin
      return V;
   end Echo_Int;

   ----------------
   -- Echo_Short --
   ----------------

   function Echo_Short (V : SOAP.Types.Short) return SOAP.Types.Short is
   begin
      return V;
   end Echo_Short;

   ---------------
   -- Echo_Long --
   ---------------

   function Echo_Long (V : SOAP.Types.Long) return SOAP.Types.Long is
   begin
      return V;
   end Echo_Long;

   ---------------
   -- Echo_Byte --
   ---------------

   function Echo_Byte (V : SOAP.Types.Byte) return SOAP.Types.Byte is
   begin
      return V;
   end Echo_Byte;

   ------------------------
   -- Echo_Unsigned_Long --
   ------------------------

   function Echo_Unsigned_Long
     (V : SOAP.Types.Unsigned_Long) return SOAP.Types.Unsigned_Long is
   begin
      return V;
   end Echo_Unsigned_Long;

   -----------------------
   -- Echo_Unsigned_Int --
   -----------------------

   function Echo_Unsigned_Int
     (V : SOAP.Types.Unsigned_Int) return SOAP.Types.Unsigned_Int is
   begin
      return V;
   end Echo_Unsigned_Int;

   -------------------------
   -- Echo_Unsigned_Short --
   -------------------------

   function Echo_Unsigned_Short
     (V : SOAP.Types.Unsigned_Short) return SOAP.Types.Unsigned_Short is
   begin
      return V;
   end Echo_Unsigned_Short;

   ------------------------
   -- Echo_Unsigned_Byte --
   ------------------------

   function Echo_Unsigned_Byte
     (V : SOAP.Types.Unsigned_Byte) return SOAP.Types.Unsigned_Byte is
   begin
      return V;
   end Echo_Unsigned_Byte;

   ------------------
   -- Echo_New_Rec --
   ------------------

   function Echo_New_Rec (V : New_Rec) return New_Rec is
   begin
      return V;
   end Echo_New_Rec;

   --------------
   -- Echo_Set --
   --------------

   function Echo_Set (Set : Set_Of_Int) return Set_Of_Int is
   begin
      return Set;
   end Echo_Set;

   ------------------
   -- Echo_Set_Rec --
   ------------------

   function Echo_Set_Rec (Set : Set_Of_Rec) return Set_Of_Rec is
   begin
      return Set;
   end Echo_Set_Rec;

   ----------
   -- Next --
   ----------

   function Next (Col : Color) return Color is
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

   function Plus (Value : Natural) return Natural is
   begin
      return Value + 1;
   end Plus;

end WSDL_6;
