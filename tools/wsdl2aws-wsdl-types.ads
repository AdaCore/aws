------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2015-2018, AdaCore                     --
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
with SOAP.WSDL.Schema;

package WSDL2AWS.WSDL.Types is

   use Ada.Strings.Unbounded;

   --  A type object

   type Object is private;

   function Create
     (Name : String; NS : SOAP.Name_Space.Object) return Object;
   --  Create a full reference for a type

   function Name (O : Object; NS : Boolean := False) return String;
   --  Returns the name of the type, qualified with the name-space if NS
   --  is true.

   function NS (O : Object) return SOAP.Name_Space.Object;
   --  Retrurns the name-space for the type

   --  Kind of types

   type Kind is (K_Record, K_Array, K_Derived, K_Enumeration, K_Simple);

   subtype Compound_Type is Kind range K_Record .. K_Array;

   --  Enumeration values

   type E_Node;
   type E_Node_Access is access E_Node;

   type E_Node is record
      Value : Unbounded_String;
      Next  : E_Node_Access;
   end record;

   --  Constraints

   Unset : constant Natural := Natural'Last;

   type Constraints_Def is record
      Min_Inclusive : Unbounded_String;
      Min_Exclusive : Unbounded_String;
      Max_Inclusive : Unbounded_String;
      Max_Exclusive : Unbounded_String;
      Pattern       : Unbounded_String;
      Length        : Natural := Unset;
      Min_Length    : Natural := Unset;
      Max_Length    : Natural := Unset;
   end record;

   procedure Get_Constraint_Integer
     (Constraints : Constraints_Def;
      Lower       : in out Long_Long_Integer;
      L_Set       : out Boolean;
      Upper       : in out Long_Long_Integer;
      U_Set       : out Boolean);
   --  Returns the lower and upper bounds if defined otherwise Set is False

   procedure Get_Constraint_Float
     (Constraints : Constraints_Def;
      Lower       : in out Float;
      L_Set       : out Boolean;
      Upper       : in out Float;
      U_Set       : out Boolean);
   --  Returns the lower and upper bounds if defined otherwise Set is False

   procedure Get_Constraint_Double
     (Constraints : Constraints_Def;
      Lower       : in out Long_Float;
      L_Set       : out Boolean;
      Upper       : in out Long_Float;
      U_Set       : out Boolean);
   --  Returns the lower and upper bounds if defined otherwise Set is False

   --  Parameter

   type Definition (Mode : Kind) is record
      Ref : Object;

      case Mode is
         when K_Derived =>
            Constraints : Constraints_Def;
            Parent      : Object;

         when K_Simple =>
            null;

         when K_Record =>
            Is_Choice : Boolean;

         when K_Array =>
            E_Name : Unbounded_String; -- array's element name
            E_Type : Object;           -- array's element's type

         when K_Enumeration =>
            E_Def  : E_Node_Access;
      end case;
   end record;

   function Is_Constrained (Def : Definition) return Boolean;
   --  Returns True is the type is constrained and so can appear as-is in a
   --  record component for example.

   function Is_Character (Def : Definition) return Boolean;
   --  Returns True if the definition corresponds to a character (type derived
   --  from xsd:string with a constraint of length 1).

   No_Definition : constant Definition;

   procedure Register (Def : Definition) with
     Post => Count >= Count'Old;
   --  Add Param at the end of P

   function Image (Def : Definition) return String;
   --  Returns a string representation of Kind type

   function Find (O : Object; Registered : Boolean := False) return Definition;
   --  Returns the type definition for the given type name and name-space or
   --  No_Definition if not found. Note that the standard xsd types are not
   --  registered there, only the types as found in the schema. If Registered
   --  is true only check for registered type into this package.

   function Count return Natural;
   --  Returns the number of type registered

   procedure Output (Def : Definition);
   --  Output parameter set, this is to be used for debugging purpose

   procedure Release with
     Post => Count = 0;
   --  Release memory associated the type definitions

   function Root_Type_For
     (Def        : Definition;
      Registered : Boolean := False) return String;
   --  Returns the root type (XSD type) for the given defintion. If registered
   --  is true check only for registered types into this package.

   procedure Get_Constraints
     (Def         : Definition;
      Constraints : out Constraints_Def);
   --  Set constaints from the type hierarchy

   function From_SOAP
     (Def          : Definition;
      Object       : String;
      Type_Name    : String := "";
      Is_SOAP_Type : Boolean := False;
      Is_Uniq      : Boolean := True) return String
     with Pre => Def.Mode in Compound_Type xor Type_Name = "";
   --  Is_SOAP_Type is true if Object is alreay a SOAP types object. So there
   --  is no need for a convertion in this context.

   type Ref_Kind is (Name_Var, Type_Var, Both_Var, Both_Value);
   --  Whether the reference for Name and Type_Name are values or variables

   function To_SOAP
     (Def          : WSDL.Types.Definition;
      Object, Name : String;
      Type_Name    : String := "";
      Name_Kind    : Ref_Kind := Both_Value;
      Is_Uniq      : Boolean := True;
      NS           : String := "") return String;
   --  Returns the code to create a SOAP parameter with given Name. Object is
   --  the reference to the object to convert. Type_Name is the name of the
   --  type to convert to/from. If NS is empty a Name_Space.Object will be
   --  generated otherwise it is the variable name visible in the scope of
   --  the generated code.

   function Get_Schema_Definition return SOAP.WSDL.Schema.Definition;
   --  Get the schema definition part for types defined in the WSDL

private

   type Object is record
      Name : Unbounded_String;
      NS   : SOAP.Name_Space.Object;
   end record;

   No_Type : constant Object :=
               (Null_Unbounded_String, SOAP.Name_Space.No_Name_Space);

   No_Definition : constant Definition :=
                     (K_Enumeration, No_Type, null);

end WSDL2AWS.WSDL.Types;
