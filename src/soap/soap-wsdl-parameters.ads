------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2003-2010, AdaCore                     --
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

with SOAP.Name_Space;

package SOAP.WSDL.Parameters is

   use Ada.Strings.Unbounded;

   type Kind is (K_Record, K_Array, K_Derived, K_Simple, K_Enumeration);
   subtype Compound_Type is Kind range K_Record .. K_Array;

   --  Enumeration values

   type E_Node;
   type E_Node_Access is access E_Node;

   type E_Node is record
      Value : Unbounded_String;
      Next  : E_Node_Access;
   end record;

   --  Parameter

   type Parameter;
   type P_Set is access Parameter;

   type Parameter (Mode : Kind) is record
      Name : Unbounded_String;
      NS   : Name_Space.Object;
      Next : P_Set;

      case Mode is
         when K_Simple =>
            P_Type : Parameter_Type;

         when K_Derived =>
            Parent_Type : Parameter_Type;   -- Parent type
            D_Name      : Unbounded_String; -- Derived type name

         when K_Array | K_Record =>
            T_Name : Unbounded_String; -- Type name
            E_Type : Unbounded_String; -- Array element's type
            Length : Natural;          -- Number of items (0 = unbounded)
            P      : P_Set;

         when K_Enumeration =>
            E_Name : Unbounded_String; -- Enumeration type name
            E_Def  : E_Node_Access;
      end case;
   end record;

   function Type_Name (P : WSDL.Parameters.P_Set) return String;
   --  Returns the type name for the given parameter

   procedure Append (P : in out P_Set; Param : Parameter);
   --  Add Param at the end of P

   function Length (P : P_Set) return Natural;
   --  Returns the number of items in P

   procedure Output (P : P_Set);
   --  Output parameter set, this is to be used for debugging purpose

   procedure Release (P : in out P_Set);
   --  Release memory associated the the parameter set

end SOAP.WSDL.Parameters;
