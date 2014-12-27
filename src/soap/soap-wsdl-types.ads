------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                        Copyright (C) 2015, AdaCore                       --
--                                                                          --
--  This library is free software;  you can redistribute it and/or modify   --
--  it under terms of the  GNU General Public License  as published by the  --
--  Free Software  Foundation;  either version 3,  or (at your  option) any --
--  later version. This library is distributed in the hope that it will be  --
--  useful, but WITHOUT ANY WARRANTY;  without even the implied warranty of --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                    --
--                                                                          --
--  As a special exception under Section 7 of GPL version 3, you are        --
--  granted additional permissions described in the GCC Runtime Library     --
--  Exception, version 3.1, as published by the Free Software Foundation.   --
--                                                                          --
--  You should have received a copy of the GNU General Public License and   --
--  a copy of the GCC Runtime Library Exception along with this program;    --
--  see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see   --
--  <http://www.gnu.org/licenses/>.                                         --
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

package SOAP.WSDL.Types is

   use Ada.Strings.Unbounded;

   type Kind is (K_Record, K_Array, K_Derived, K_Enumeration, K_Simple);

   subtype Compound_Type is Kind range K_Record .. K_Array;

   --  Enumeration values

   type E_Node;
   type E_Node_Access is access E_Node;

   type E_Node is record
      Value : Unbounded_String;
      Next  : E_Node_Access;
   end record;

   --  Parameter

   type Definition (Mode : Kind) is record
      Name : Unbounded_String;
      NS   : Name_Space.Object;

      case Mode is
         when K_Derived =>
            Parent_Name : Unbounded_String; -- Derived type name

         when K_Simple | K_Record =>
            null;

         when K_Array =>
            E_Type : Unbounded_String; -- Array element's type

         when K_Enumeration =>
            E_Def  : E_Node_Access;
      end case;
   end record;

   No_Definition : constant Definition;

   procedure Register (Def : Definition) with
     Post => Count >= Count'Old;
   --  Add Param at the end of P

   function Image (K : Kind) return String;
   --  Returns a string representation of Kind type

   function Find
     (Type_Name : String; NS : Name_Space.Object) return Definition;
   --  Returns the type definition for the given type name and name-space
   --  or No_Definition if not found. Note that the standard xsd types are
   --  not registered their, only the types as found in the schema.

   function Count return Natural;
   --  Returns the number of type registered

   procedure Output (Def : Definition);
   --  Output parameter set, this is to be used for debugging purpose

   procedure Release with
     Post => Count = 0;
   --  Release memory associated the type definitions

   function Root_Type_For (Def : Definition) return String;
   --  Returns the root type (XSD type) for the given defintion

private

   No_Definition : constant Definition :=
                     (K_Enumeration, Null_Unbounded_String,
                      Name_Space.No_Name_Space, null);

end SOAP.WSDL.Types;
