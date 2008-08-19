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

with SOAP.Types;
with SOAP.Utils;

package WSDL_6 is

   use Ada.Strings.Unbounded;

   type Color is (Red, GrEEn, Blue);

   type Rec is record
      A : Integer;
      B : Float;
      C : Long_Float;
      D : Character;
      E : Unbounded_String;
      F : Boolean;
   end record;

   type New_Rec is record
      NC : Color;
      NR : Rec;
   end record;

   type Set_Of_Int is array (Positive range <>) of Integer;

   type Set_Of_Rec is array (Positive range <>) of Rec;

   type Set_Of_Int_Access is access Set_Of_Int;

   package Set_Of_Int_Safe_Pointer is
      new SOAP.Utils.Safe_Pointers (Set_Of_Int, Set_Of_Int_Access);

   type Complex_Rec is record
      SI : Set_Of_Int_Safe_Pointer.Safe_Pointer;
   end record;

   function Plus (Value : in Natural) return Natural;

   function Next (Col : in Color) return Color;

   function Echo_Int (V : in Integer) return Integer;

   function Echo_Short (V : in SOAP.Types.Short) return SOAP.Types.Short;

   function Echo_Long (V : in SOAP.Types.Long) return SOAP.Types.Long;

   function Echo_Byte (V : in SOAP.Types.Byte) return SOAP.Types.Byte;

   function Echo_Float (V : in Float) return Float;

   function Echo_Boolean (V : in Boolean) return Boolean;

   function Echo_Double (V : in Long_Float) return Long_Float;

   function Echo_Unsigned_Long
     (V : in SOAP.Types.Unsigned_Long) return SOAP.Types.Unsigned_Long;

   function Echo_Unsigned_Int
     (V : in SOAP.Types.Unsigned_Int) return SOAP.Types.Unsigned_Int;

   function Echo_Unsigned_Short
     (V : in SOAP.Types.Unsigned_Short) return SOAP.Types.Unsigned_Short;

   function Echo_Unsigned_Byte
     (V : in SOAP.Types.Unsigned_Byte) return SOAP.Types.Unsigned_Byte;

   function Echo_Rec (V : in Rec) return Rec;

   function Echo_New_Rec (V : in New_Rec) return New_Rec;

   function Echo_Set (Set : in Set_Of_Int) return Set_Of_Int;

   function Echo_Set_Rec (Set : in Set_Of_Rec) return Set_Of_Rec;

   function Echo_Complex_Rec (C_Rec : in Complex_Rec) return Complex_Rec;

end WSDL_6;
