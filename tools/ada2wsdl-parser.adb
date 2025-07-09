----------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2003-2025, AdaCore                     --
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

with Ada.Characters.Handling;
with Ada.Exceptions;
with Ada.Strings;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with Langkit_Support.Errors;
with Langkit_Support.Text;
with Libadalang.Analysis;
with Libadalang.Common;
with Libadalang.Project_Provider;

with GNATCOLL.Projects;
with GNATCOLL.VFS;

with AWS.Utils;
with SOAP.Name_Space;
with SOAP.Types;

with Ada2WSDL.Generator;
with Ada2WSDL.Options;

package body Ada2WSDL.Parser is

   use Ada;
   use Ada.Exceptions;
   use Ada.Strings.Unbounded;

   use Libadalang.Analysis;
   use Libadalang.Common;

   use type SOAP.Types.Decimal;
   use type SOAP.Types.Unsigned_Long;

   subtype Internal_Integer is Long_Long_Long_Integer;
   --  The type used for internal computation of range, value... So we can
   --  safely check range of Long_Integer or Long_Long_Integer on standard
   --  x86_64 Linux targets where those types are 64bits and Internal_Integer
   --  will be 128bits. For the later we special case the computation to avoid
   --  overflow, see in Type_Range_G.

   package TxT renames Langkit_Support.Text;

   function "+"
     (Str : String) return Unbounded_String renames To_Unbounded_String;

   -----------------------------------
   -- Deferred LaL types to analyse --
   -----------------------------------

   Max_Deferred_Types : constant := 1_024;

   type Element_Set is array (Positive range 1 .. Max_Deferred_Types)
     of Ada_Node;

   Deferred_Types : Element_Set;
   --  Records all types tha can't be analysed at some point. For example we
   --  can't parse a type while parsing the spec.

   Index          : Natural := 0;
   --  Current Index in the Deferred_Types array

   procedure Append_Deferred (Node : Ada_Node'Class);
   --  Append a new element into the list of deferred types

   function Get_Vector_Type
     (Node : Type_Decl) return Base_Type_Decl;
   --  Returns the Element_Type of an instanciation of a Containers.Vectors
   --  This is very dependant on the Ada.Containers.Vector implementation.

   function Name_Space (Node : Ada_Node'Class) return String;
   --  Returns the name space for element Node. Name space is defined as
   --  follow: http://soapaws/<unit_name>_pkg/

   function Img
     (Name       : Ada_Node'Class;
      Lower_Case : Boolean := False) return String is
     (if Lower_Case
      then Characters.Handling.To_Lower (TxT.Image (Name.Text))
      else TxT.Image (Name.Text));
   --  Return string representation of Name

   procedure Get_Range_Derived
     (Node : Base_Type_Decl; Min, Max : out Unbounded_String);
   --  Get range Min and Max for a derived type

   function Get_Range_Expr
     (Node     : Ada_Node'Class;
      Top_Decl : Boolean := False) return Bin_Op;
   --  Return the range expression for Node, either the top one found in
   --  Node or the one in the base type going up into the derived/subtype
   --  definitions.

   function Get_Base_Type
     (Node : Derived_Type_Def'Class) return Base_Type_Decl;
   --  Get the base type for Node

   function Get_Base_Type
     (Node : Subtype_Decl'Class) return Base_Type_Decl;
   --  Likewise for a subtype

   function Get_Root_Type (Node : Subtype_Decl'Class) return Base_Type_Decl;
   --  For a subtype returns the root type

   function Unit_Name (Node : Ada_Node'Class) return String is
     (Img (Node.P_Top_Level_Decl (Node.Unit).P_Defining_Name));
   --  Name of the enclosing unit where Node is declared

   function Is_Standard (Node : Ada_Node'Class) return Boolean is
     (Characters.Handling.To_Lower (Unit_Name (Node)) = "standard");
   --  True if Node is declared into the standard Ada package

   function Is_Calendar (Node : Ada_Node'Class) return Boolean is
     (Characters.Handling.To_Lower (Unit_Name (Node)) = "ada.calendar");
   --  True if Node is declared into the Ada.Calendar package

   function Type_Definition
     (Node : Ada_Node;
      Name : String;
      Decl : Ada_Node'Class;
      Base : Boolean) return Generator.Type_Data;
   --  Get the type data for the given Node. Decl is the point of declaration
   --  for the type/object and so used to get the name space. If Base is set
   --  to True we resolve all the derived/subtype definition to get information
   --  about the base type.

   function Type_Definition
     (Node : Type_Decl'Class;
      Base : Boolean) return Generator.Type_Data;
   --  Likewise for a type-decl

   function Type_Definition
     (Node : Type_Expr'Class;
      Base : Boolean) return Generator.Type_Data;
   --  Likewise for a type-expr

   procedure Analyze_Type (Node : Ada_Node'Class);
   --  Analyze a type declaration (array, derived, etc.)

   procedure Analyze_Subtype (Node : Subtype_Decl'Class);
   --  Analyze a subtype declaration

   procedure Analyze_Array
     (T_Decl : Base_Type_Decl;
      T_Name : String;
      Decl   : Ada_Node'Class;
      Node   : Array_Type_Def'Class);
   --  Analyze an array type declaration

   function Analyze_Array_Component
     (Node : Component_Def'Class) return Generator.Type_Data;
   --  Analyze an array component

   procedure Array_Type_Suffix
     (Lower, Upper : Internal_Integer;
      Type_Suffix  : out Unbounded_String;
      Length       : out Natural);

   package Compute is

      --  Compute package is used to compute the range and values for
      --  Bin_Op and Expr nodes.

      generic
         type T is private;
         Zero : T;
         with function Value (Str : String) return T;
         with function "+" (Left, Right : T) return T is <>;
         with function "-" (Left, Right : T) return T is <>;
         with function "**" (Left, Right : T) return T is <>;
         with function "*" (Left, Right : T) return T is <>;
         with function "/" (Left, Right : T) return T is <>;
      function Value_G (Node : Expr) return T;
      --  Compute the value for expression Node

      generic
         type T is private;
         with function Compute_Value (Node : Expr) return T is <>;
      procedure Range_G (Node : Bin_Op; Lower, Upper : out T);
      --  Get the Lower and Upper bounds of a range expression

      generic
         type T is private;
         First : T;
         Last  : T;
         with procedure Get_Range (Node : Bin_Op; Lower, Upper : out T) is <>;
      procedure Type_Range_G
        (Node         : Ada_Node'Class;
         Lower, Upper : out T;
         Top_Decl     : Boolean := False);
      --  Get the range of a given type, either the range of the top
      --  declaration or the one of the base type if Top_Decl isi False.

   end Compute;

   package body Compute is

      -----------------
      -- Get_Range_G --
      -----------------

      procedure Range_G (Node : Bin_Op; Lower, Upper : out T)  is
         Left  : constant Expr := Node.F_Left;
         Right : constant Expr := Node.F_Right;
      begin
         Lower := Compute_Value (Left);
         Upper := Compute_Value (Right);
      end Range_G;

      ----------------------
      -- Get_Type_Range_G --
      ----------------------

      procedure Type_Range_G
        (Node         : Ada_Node'Class;
         Lower, Upper : out T;
         Top_Decl     : Boolean := False)
      is
         E         : constant Bin_Op := Get_Range_Expr (Node, Top_Decl);
         T_Name    : constant String :=
                       (if Node.Kind in Ada_Type_Decl
                        then Img (Node.As_Base_Type_Decl.F_Name,
                          Lower_Case => True)
                        else "");
         Is_Std_LL : constant Boolean :=
                       Node.Kind in Ada_Type_Decl
                           and then Is_Standard (Node)
                           and then T_Name = "long_long_long_integer";
      begin
         Lower := Last;
         Upper := First;

         if E /= No_Bin_Op and then E.Kind = Ada_Bin_Op then
            --  Do not try to compute range for Long_Long_Long_Integer as this
            --  will overflow in Get_Range while computing last (2**127 -
            --  1).

            if Is_Std_LL then
               Lower := First;
               Upper := Last;
            else
               Get_Range (E.As_Bin_Op, Lower, Upper);
            end if;
         end if;
      end Type_Range_G;

      -------------
      -- Value_G --
      -------------

      function Value_G (Node : Expr) return T is
         Result : T := Zero;
      begin
         case Node.Kind is
            when Ada_Bin_Op =>
               declare
                  Op    : constant Bin_Op := Node.As_Bin_Op;
                  Left  : constant T := Value_G (Op.F_Left);
                  Right : constant T := Value_G (Op.F_Right);
               begin
                  case Op.F_Op.Kind is
                     when Ada_Op_Plus =>
                        Result := Left + Right;

                     when Ada_Op_Minus =>
                        Result := Left - Right;

                     when Ada_Op_Pow =>
                        Result := Left ** Right;

                     when Ada_Op_Div =>
                        Result := Left / Right;

                     when Ada_Op_Mult =>
                        Result := Left * Right;

                     when others =>
                        null;
                  end case;
               end;

            when Ada_Un_Op =>
               declare
                  Op  : constant Un_Op := Node.As_Un_Op;
                  Val : constant T := Value_G (Op.F_Expr);
               begin
                  case Op.F_Op.Kind is
                     when Ada_Op_Plus =>
                        Result := Val;

                     when Ada_Op_Minus =>
                        Result := Zero - Val;

                     when others =>
                        null;
                  end case;
               end;

            when Ada_Paren_Expr =>
               Result := Value_G (Node.As_Paren_Expr.F_Expr);

            when Ada_Int_Literal | Ada_Real_Literal =>
               Result := Value (Img (Node));

            when Ada_Identifier =>
               declare
                  R : constant Basic_Decl :=
                        Node.As_Identifier.P_Referenced_Decl;
               begin
                  case R.Kind is
                     when Ada_Number_Decl =>
                        --  For : N : constant := 9;
                        return Value_G (R.As_Number_Decl.F_Expr);

                     when Ada_Object_Decl =>
                        --  For : N : [constant] Integer := 1;
                        return Value_G (R.As_Object_Decl.F_Default_Expr);

                     when others =>
                        null;
                  end case;
               end;

            when Ada_Attribute_Ref =>
               --  Handle 'First and 'Last only to get a range expressed
               --  using Integer'First for example.
               declare
                  A     : constant Attribute_Ref := Node.As_Attribute_Ref;
                  T     : constant Name := A.F_Prefix;
                  N     : constant Identifier := A.F_Attribute;
                  N_Img : constant String := Img (N);
                  E     : constant Bin_Op :=
                            Get_Range_Expr (T.P_Name_Designated_Type);
               begin
                  if E.Kind = Ada_Bin_Op then
                     if N_Img = "First" then
                        Result := Value_G (E.As_Bin_Op.F_Left);
                     elsif N_Img = "Last" then
                        Result := Value_G (E.As_Bin_Op.F_Right);
                     end if;
                  end if;
               end;

            when others =>
               null;
         end case;

         return Result;
      end Value_G;

   end Compute;

   -------------------
   -- Compute_Value --
   -------------------

   function "**" (Left, Right : Internal_Integer) return Internal_Integer
     is (Left ** Integer (Right));

   function Compute_Value is new Compute.Value_G
     (T     => Internal_Integer,
      Zero  => 0,
      Value => Internal_Integer'Value);

   function "**"
     (Left, Right : SOAP.Types.Unsigned_Long) return SOAP.Types.Unsigned_Long
   is (Left ** Integer (Right));

   function Compute_Value is new Compute.Value_G
     (T     => SOAP.Types.Unsigned_Long,
      Zero  => 0,
      Value => SOAP.Types.Unsigned_Long'Value);

   function "**" (Left, Right : Long_Float) return Long_Float
     is (Left ** Integer (Right));

   function Compute_Value is new Compute.Value_G
     (T     => Long_Float,
      Zero  => 0.0,
      Value => Long_Float'Value);

   function "**" (Left, Right : SOAP.Types.Decimal) return SOAP.Types.Decimal
     is (SOAP.Types.Decimal (Long_Float (Left) ** Integer (Right)));

   function "/" (Left, Right : SOAP.Types.Decimal) return SOAP.Types.Decimal
     is (SOAP.Types.Decimal (Long_Float (Left) / Long_Float (Right)));

   function "*" (Left, Right : SOAP.Types.Decimal) return SOAP.Types.Decimal
     is (SOAP.Types.Decimal (Long_Float (Left) * Long_Float (Right)));

   function Compute_Value is new Compute.Value_G
     (T     => SOAP.Types.Decimal,
      Zero  => 0.0,
      Value => SOAP.Types.Decimal'Value);

   ---------------
   -- Get_Range --
   ---------------

   procedure Get_Range is new Compute.Range_G (T => Internal_Integer);
   procedure Get_Range is new Compute.Range_G (T => Long_Float);
   procedure Get_Range is new Compute.Range_G (T => SOAP.Types.Decimal);

   procedure Get_Range is new Compute.Type_Range_G
     (T     => Internal_Integer,
      First => Internal_Integer'First,
      Last  => Internal_Integer'Last);

   procedure Get_Range is new Compute.Type_Range_G
     (T     => Long_Float,
      First => Long_Float'First,
      Last  => Long_Float'Last);

   procedure Get_Range is new Compute.Type_Range_G
     (T     => SOAP.Types.Decimal,
      First => SOAP.Types.Decimal'First,
      Last  => SOAP.Types.Decimal'Last);

   -------------------
   -- Analyze_Array --
   -------------------

   procedure Analyze_Array
     (T_Decl : Base_Type_Decl;
      T_Name : String;
      Decl   : Ada_Node'Class;
      Node   : Array_Type_Def'Class)
   is
      Components   : constant Component_Def := Node.F_Component_Type;
      Array_Len    : Natural := 0;
      Type_Suffix  : Unbounded_String;
      Lower, Upper : Internal_Integer;
   begin
      if not Options.Quiet then
         Text_IO.Put_Line
           (Location (T_Decl)
            & "warning: array " & T_Name
            & " not supported by wsdl2aws, use Containers.Vector");
      end if;

      Get_Range (T_Decl, Lower, Upper);
      Array_Type_Suffix (Lower, Upper, Type_Suffix, Array_Len);

      declare
         E_Type : constant Generator.Type_Data :=
                    Analyze_Array_Component (Components);
      begin
         Generator.Start_Array
           (Name_Space (Decl), T_Name,
            To_String (E_Type.NS),
            To_String (E_Type.Name),
            Array_Len);
      end;
   end Analyze_Array;

   -----------------------------
   -- Analyze_Array_Component --
   -----------------------------

   function Analyze_Array_Component
     (Node : Component_Def'Class) return Generator.Type_Data
   is
      C_Type : constant Base_Type_Decl :=
                 Node.F_Type_Expr.P_Designated_Type_Decl;
      C_Name : constant String := Img (C_Type.F_Name);
      E_Type : constant Generator.Type_Data :=
                 Type_Definition (C_Type.As_Ada_Node, C_Name, C_Type, False);
      T_Decl : constant Base_Type_Decl :=
                 (if C_Type.Kind = Ada_Subtype_Decl
                  then C_Type.As_Subtype_Decl.F_Subtype.P_Designated_Type_Decl
                  else C_Type.As_Base_Type_Decl);

   begin
      Analyze_Type (T_Decl.As_Type_Decl);

      return E_Type;
   end Analyze_Array_Component;

   ---------------------
   -- Analyze_Subtype --
   ---------------------

   procedure Analyze_Subtype (Node : Subtype_Decl'Class) is

      T_Name       : constant String := Img (Node.F_Name);
      T_Decl       : constant Base_Type_Decl :=
                       Get_Root_Type (Node);
      NS           : constant String := Name_Space (Node);
      T_Def        : constant Type_Def := T_Decl.As_Type_Decl.F_Type_Def;

      Lower, Upper : Internal_Integer;
      Type_Suffix  : Unbounded_String;
      Array_Len    : Natural;
   begin
      if not Generator.Type_Exists (NS, T_Name) then
         if T_Def.Kind = Ada_Array_Type_Def then
            Get_Range (Node, Lower, Upper);
            Array_Type_Suffix (Lower, Upper, Type_Suffix, Array_Len);

            declare
               A_Def      : constant Array_Type_Def :=
                              T_Def.As_Array_Type_Def;
               Components : constant Component_Def :=
                              A_Def.F_Component_Type;
               E_Type     : constant Generator.Type_Data :=
                              Analyze_Array_Component (Components);
            begin
               Generator.Start_Array
                 (Name_Space (T_Decl), T_Name,
                  To_String (E_Type.NS),
                  To_String (E_Type.Name),
                  Array_Len);
            end;

         else
            Generator.Register_Derived
              (Name_Space (Node), T_Name,
               Type_Definition (T_Decl.As_Type_Decl, Base => True));
         end if;
      end if;
   end Analyze_Subtype;

   ------------------
   -- Analyze_Type --
   ------------------

   procedure Analyze_Type (Node : Ada_Node'Class) is

      NS : constant String := Name_Space (Node);

      type U_Array_Def is record
         Name, NS           : Unbounded_String;
         Comp_NS, Comp_Type : Unbounded_String;
         Length             : Natural;
      end record;

      Deferred_U_Arrays : array (1 .. 100) of U_Array_Def;
      U_Array_Index     : Natural := 0;
      Def               : Generator.Type_Data;

      procedure Analyze_Derived (Node : Derived_Type_Def'Class);
      --  Analyze a derived type definition

      procedure Analyze_Subtype (Node : Subtype_Indication'Class);
      --  Analyze a subtype

      procedure Analyze_Enumeration (Node : Enum_Type_Def'Class);
      --  Analyzye an enumeration definition

      procedure Analyze_Numeric (Node : Type_Def'Class);
      --  Analyze a numeric type (Integer, Float)

      procedure Analyze_Record (Node : Record_Type_Def'Class);
      --  Analyze a record type definition

      function T_Name return String is
        (Img
           (if Node.Kind = Ada_Subtype_Indication
            then Node.As_Subtype_Indication.F_Name
            else Node.As_Type_Decl.F_Name));
      --  Return the Node's type name

      function T_Def return Type_Def is (Node.As_Type_Decl.F_Type_Def);

      function T_Decl return Base_Type_Decl is (Node.As_Base_Type_Decl);

      ---------------------
      -- Analyze_Derived --
      ---------------------

      procedure Analyze_Derived (Node : Derived_Type_Def'Class) is
         B_Type : constant Base_Type_Decl := Get_Base_Type (Node);
         B_Def  : constant Type_Def := B_Type.As_Type_Decl.F_Type_Def;
         B_Name : constant String := Img (B_Type.F_Name);
      begin
         if B_Def.Kind = Ada_Array_Type_Def then
            declare
               A_Def        : constant Array_Type_Def :=
                                B_Def.As_Array_Type_Def;
               Components   : constant Component_Def :=
                                A_Def.F_Component_Type;
               Len          : Unbounded_String;
               Lower, Upper : Internal_Integer;
               Type_Suffix  : Unbounded_String;
               Array_Len    : Integer := 0;
            begin
               Get_Range (T_Decl, Lower, Upper);

               if Lower /= Internal_Integer'Last then
                  Array_Type_Suffix (Lower, Upper, Type_Suffix, Array_Len);
                  Len := To_Unbounded_String (AWS.Utils.Image (Array_Len));
               end if;

               if B_Name = "String" then
                  Generator.Register_Derived
                    (NS, T_Name,
                     (To_Unbounded_String
                          (SOAP.Name_Space.Value
                             (SOAP.Name_Space.XSD)),
                      To_Unbounded_String ("string"),
                      Null_Unbounded_String,
                      Null_Unbounded_String,
                      Len));

                  return;

               else
                  --  We have constraint, register an array, otherwise we
                  --  will register a derived type.

                  if Array_Len /= 0 then
                     if not Generator.Type_Exists
                       (Name_Space (Components), T_Name)
                     then
                        declare
                           E_Type : constant Generator.Type_Data :=
                                      Analyze_Array_Component (Components);
                        begin
                           Generator.Start_Array
                             (Name_Space (Components), T_Name,
                              To_String (E_Type.NS),
                              To_String (E_Type.Name),
                              Array_Len);
                        end;
                     end if;

                     return;
                  end if;
               end if;
            end;
         end if;

         if not Generator.Type_Exists (NS, T_Name) then
            Def := Type_Definition
              (Analyze_Type.Node.As_Type_Decl, Base => True);
            Generator.Register_Derived (NS, T_Name, Def);
         end if;
      end Analyze_Derived;

      ------------------
      -- Analyze_Enum --
      ------------------

      procedure Analyze_Enumeration
        (Node : Enum_Type_Def'Class) is
      begin
         Generator.Start_Enumeration (NS, T_Name);

         for Literal of Node.F_Enum_Literals loop
            Generator.New_Literal (Img (Literal));
         end loop;
      end Analyze_Enumeration;

      ---------------------
      -- Analyze_Numeric --
      ---------------------

      procedure Analyze_Numeric
        (Node : Type_Def'Class) is
      begin
         if not Is_Standard (Node) then
            Def := Type_Definition (T_Decl.As_Type_Decl, Base => True);
            Generator.Register_Type (NS, T_Name, Def);
         end if;
      end Analyze_Numeric;

      --------------------
      -- Analyze_Record --
      --------------------

      procedure Analyze_Record (Node : Record_Type_Def'Class) is

         procedure Analyze_Field (Node : Component_Decl);
         --  Analyze every record fields

         -------------------
         -- Analyze_Field --
         -------------------

         procedure Analyze_Field (Node : Component_Decl) is
            C_Def  : constant Component_Def := Node.F_Component_Def;
            F_Decl : Base_Type_Decl :=
                       C_Def.F_Type_Expr.P_Designated_Type_Decl;
            V_NS   : Ada_Node := No_Ada_Node;
            T_Name : Unbounded_String;
         begin
            if Node.Kind = Ada_Anonymous_Type_Decl
              or else C_Def.F_Type_Expr.Kind = Ada_Anonymous_Type
            then
               Raise_Spec_Error
                 (C_Def,
                  Message => "anonymous access type unsupported in WSDL");
            end if;

            --  Append the type of the field into the list of deferred type
            --  to analyse later if needed. Indeed if this type is only used
            --  into the record and is defined into a separate package we
            --  need to analyse it to get the corresponding WSDL definition.

            if F_Decl.Kind in Ada_Type_Decl
              and then F_Decl.As_Type_Decl.F_Type_Def.Kind = Ada_Array_Type_Def
            then
               --  An array
               declare
                  A_Def               : constant Array_Type_Def :=
                                          F_Decl.As_Type_Decl
                                            .F_Type_Def.As_Array_Type_Def;
                  Components          : constant Component_Def :=
                                          A_Def.F_Component_Type;
                  E_Type              : constant Generator.Type_Data :=
                                          Analyze_Array_Component (Components);
                  Lower, Upper        : Internal_Integer;
                  Type_Suffix         : Unbounded_String;
                  Has_Decl_Constraint : Boolean;
               begin
                  U_Array_Index := U_Array_Index + 1;

                  --  Set array's component type information

                  Deferred_U_Arrays (U_Array_Index).Comp_NS :=
                    E_Type.NS;
                  Deferred_U_Arrays (U_Array_Index).Comp_Type :=
                    E_Type.Name;

                  Deferred_U_Arrays (U_Array_Index).NS :=
                    To_Unbounded_String (Name_Space (F_Decl));

                  --  Set array's type name

                  Deferred_U_Arrays (U_Array_Index).Name :=
                    To_Unbounded_String (Img (F_Decl.F_Name));

                  --  Do we have constraints on the base type

                  Get_Range
                    (C_Def.F_Type_Expr, Lower, Upper, Top_Decl => True);

                  Has_Decl_Constraint :=
                    Lower /= Internal_Integer'Last
                    and then Upper /= Internal_Integer'First;

                  Get_Range
                    (C_Def.F_Type_Expr, Lower, Upper, Top_Decl => False);

                  Array_Type_Suffix
                    (Lower, Upper,
                     Type_Suffix,
                     Deferred_U_Arrays (U_Array_Index).Length);

                  if Has_Decl_Constraint then
                     Append (Deferred_U_Arrays (U_Array_Index).Name,
                             Type_Suffix);
                  end if;

                  T_Name := Deferred_U_Arrays (U_Array_Index).Name;
               end;

            --  Check for a private Vector instantiation
            elsif Img (F_Decl.F_Name, Lower_Case => True) = "vector"
                 and then
               F_Decl.As_Type_Decl.F_Type_Def.Kind = Ada_Private_Type_Def
            then
               --  The name of the array type is the package instance name
               declare
                  V_Type : constant Base_Type_Decl :=
                             Get_Vector_Type (F_Decl.As_Type_Decl);
                  E_Type : constant Generator.Type_Data :=
                             Type_Definition
                               (V_Type.As_Type_Decl,
                                Base => False);
                  A_Inst : constant Name :=
                             C_Def.F_Type_Expr
                               .As_Subtype_Indication.F_Name
                               .As_Dotted_Name.F_Prefix;
               begin
                  U_Array_Index := U_Array_Index + 1;

                  --  Set array's component type information

                  Deferred_U_Arrays (U_Array_Index).Comp_NS :=
                    E_Type.NS;
                  Deferred_U_Arrays (U_Array_Index).Comp_Type :=
                    E_Type.Name;

                  V_NS := A_Inst.P_Referenced_Decl.As_Ada_Node;

                  Deferred_U_Arrays (U_Array_Index).NS :=
                    To_Unbounded_String (Name_Space (V_NS));

                  --  Set array's type name

                  Deferred_U_Arrays (U_Array_Index).Name :=
                    To_Unbounded_String (Img (A_Inst));

                  Deferred_U_Arrays (U_Array_Index).Length := 0;

                  T_Name := Deferred_U_Arrays (U_Array_Index).Name;
               end;
            end if;

            --  Check for a subtype

            if F_Decl.Kind = Ada_Subtype_Decl then
               declare
                  S_Decl : constant Base_Type_Decl :=
                             Get_Base_Type (F_Decl.As_Subtype_Decl);
               begin
                  --  If a subtype of Unbounded_String we record the field as a
                  --  string.
                  if Img (S_Decl.F_Name) = "Unbounded_String" then
                     T_Name := To_Unbounded_String ("string");
                  else
                     Append_Deferred (S_Decl);
                     T_Name := To_Unbounded_String (Img (F_Decl.F_Name));
                  end if;
               end;

            else
               if Img (F_Decl.F_Name, Lower_Case => True) = "vector" then
                  Append_Deferred (C_Def.F_Type_Expr);
               else
                  Append_Deferred (F_Decl);
               end if;
            end if;

            --  If type-name still not known, compute it now

            if T_Name = Null_Unbounded_String then
               T_Name := Type_Definition
                 (F_Decl.As_Type_Decl, Base => False).Name;
            end if;

            for C_Name of Node.F_Ids loop
               Generator.New_Component
                 (NS        => Name_Space (if V_NS /= No_Ada_Node
                                           then V_NS
                                           else F_Decl),
                  Comp_Name => Img (C_Name),
                  Comp_Type => To_String (T_Name));
            end loop;
         end Analyze_Field;

      begin
         Generator.Start_Record (NS, T_Name);

         for Field of Node.F_Record_Def.F_Components.F_Components loop
            Analyze_Field (Field.As_Component_Decl);
         end loop;

         --  Create now all deferred arrays

         for K in 1 .. U_Array_Index loop
            Generator.Start_Array
              (To_String (Deferred_U_Arrays (K).NS),
               To_String (Deferred_U_Arrays (K).Name),
               To_String (Deferred_U_Arrays (K).Comp_NS),
               To_String (Deferred_U_Arrays (K).Comp_Type),
               Deferred_U_Arrays (K).Length);
         end loop;
      end Analyze_Record;

      ---------------------
      -- Analyze_Subtype --
      ---------------------

      procedure Analyze_Subtype (Node : Subtype_Indication'Class) is
      begin
         Analyze_Type (Node.P_Designated_Type_Decl);
      end Analyze_Subtype;

   begin
      if not Generator.Type_Exists (NS, T_Name) then
         case Node.Kind is
            when Ada_Subtype_Indication =>
               Analyze_Subtype (Node.As_Subtype_Indication);

            when Ada_Concrete_Type_Decl =>
               case T_Def.Kind is
                  when Ada_Derived_Type_Def =>
                     Analyze_Derived (T_Def.As_Derived_Type_Def);

                  when Ada_Signed_Int_Type_Def
                     | Ada_Floating_Point_Def
                     | Ada_Mod_Int_Type_Def
                     | Ada_Ordinary_Fixed_Point_Def
                     | Ada_Decimal_Fixed_Point_Def
                     =>
                     Analyze_Numeric (T_Def);

                  when Ada_Enum_Type_Def =>
                     Analyze_Enumeration (T_Def.As_Enum_Type_Def);

                  when Ada_Record_Type_Def =>
                     Analyze_Record (T_Def.As_Record_Type_Def);

                  when Ada_Array_Type_Def =>
                     Analyze_Array
                       (T_Decl, T_Name, T_Decl, T_Def.As_Array_Type_Def);

                  when others =>
                     null;
               end case;

            when others =>
               null;
         end case;
      end if;
   end Analyze_Type;

   ---------------------
   -- Append_Deferred --
   ---------------------

   procedure Append_Deferred (Node : Ada_Node'Class) is
   begin
      Index := Index + 1;
      Deferred_Types (Index) := Node.As_Ada_Node;
   end Append_Deferred;

   -----------------------
   -- Array_Type_Suffix --
   -----------------------

   procedure Array_Type_Suffix
     (Lower, Upper : Internal_Integer;
      Type_Suffix  : out Unbounded_String;
      Length       : out Natural)
   is
      function I (N : Internal_Integer) return String
        is (AWS.Utils.Image (Natural (N)));
   begin
      if Lower /= 0
        and then Upper /= 0
        and then Lower /= Internal_Integer'Last
      then
         Length := Natural (Upper - Lower + 1);

         Type_Suffix :=
           To_Unbounded_String ('_' & I (Lower) & '_' & I (Upper));

      else
         Length := 0;
      end if;
   end Array_Type_Suffix;

   -------------------
   -- Get_Base_Type --
   -------------------

   function Get_Base_Type
     (Node : Derived_Type_Def'Class) return Base_Type_Decl
   is
      Base : Base_Type_Decl;
   begin
      Base := Node.F_Subtype_Indication.P_Designated_Type_Decl;

      if Base.Kind = Ada_Subtype_Decl then
         Base := Base.As_Subtype_Decl.F_Subtype.P_Designated_Type_Decl;
      end if;

      return Base;
   end Get_Base_Type;

   function Get_Base_Type
     (Node : Subtype_Decl'Class) return Base_Type_Decl
   is
      Base : Base_Type_Decl;
   begin
      Base := Node.F_Subtype.P_Designated_Type_Decl;

      if Base.Kind = Ada_Subtype_Decl then
         Base := Base.As_Subtype_Decl.F_Subtype.P_Designated_Type_Decl;
      end if;

      return Base;
   end Get_Base_Type;

   -----------------------
   -- Get_Range_Derived --
   -----------------------

   procedure Get_Range_Derived
     (Node : Base_Type_Decl; Min, Max : out Unbounded_String)
   is
      T_Def  : constant Type_Def := Node.As_Type_Decl.F_Type_Def;
      P_Type : constant Base_Type_Decl :=
                 Get_Base_Type (T_Def.As_Derived_Type_Def);
   begin
      if T_Def.As_Derived_Type_Def.F_Subtype_Indication.F_Constraint
        = No_Constraint
      then
         return;
      end if;

      case P_Type.As_Type_Decl.F_Type_Def.Kind is
         when Ada_Signed_Int_Type_Def =>
            declare
               Lower, Upper : Internal_Integer;
            begin
               Get_Range (Node, Lower, Upper);

               Min := +Internal_Integer'Image (Lower);
               Max := +Internal_Integer'Image (Upper);
            end;

         when Ada_Floating_Point_Def =>
            declare
               Lower, Upper : Long_Float;
            begin
               Get_Range (Node, Lower, Upper);

               Min := +Long_Float'Image (Lower);
               Max := +Long_Float'Image (Upper);
            end;

         when others =>
            null;
      end case;
   end Get_Range_Derived;

   --------------------
   -- Get_Range_Expr --
   --------------------

   function Get_Range_Expr
     (Node     : Ada_Node'Class;
      Top_Decl : Boolean := False) return Bin_Op
   is

      function From_Subtype (Node : Subtype_Indication) return Bin_Op;
      --  Get Bin_Op from the subtype indication

      ------------------
      -- From_Subtype --
      ------------------

      function From_Subtype (Node : Subtype_Indication) return Bin_Op is
         R : constant Constraint'Class := Node.F_Constraint;
      begin
         if R = No_Constraint then
            if Top_Decl then
               return No_Bin_Op;
            else
               return Get_Range_Expr (Node.P_Designated_Type_Decl);
            end if;

         else
            if R.Kind = Ada_Range_Constraint then
               return R.As_Range_Constraint.F_Range.F_Range.As_Bin_Op;

            elsif R.Kind = Ada_Composite_Constraint
              and then R.As_Composite_Constraint.P_Is_Index_Constraint
            then
               return R.As_Composite_Constraint.F_Constraints.Child (1)
                   .As_Composite_Constraint_Assoc.F_Constraint_Expr.As_Bin_Op;
            else
               return No_Bin_Op;
            end if;
         end if;
      end From_Subtype;

      ------------------
      -- Get_Range_Op --
      ------------------

      function Get_Range_Op (R : Range_Spec) return Bin_Op is
        (if R = No_Range_Spec
         then No_Bin_Op
         else R.F_Range.As_Bin_Op);

   begin
      case Node.Kind is
         when Ada_Bin_Op =>
            return Node.As_Bin_Op;

         when Ada_Type_Decl =>
            declare
               T : constant Type_Decl := Node.As_Type_Decl;
               D : constant Type_Def := T.F_Type_Def;
            begin
               case D.Kind is
                  when Ada_Signed_Int_Type_Def =>
                     return Get_Range_Op (D.As_Signed_Int_Type_Def.F_Range);

                  when Ada_Real_Type_Def =>
                     if D.Kind = Ada_Ordinary_Fixed_Point_Def then
                        return Get_Range_Op
                                 (D.As_Ordinary_Fixed_Point_Def.F_Range);
                     elsif D.Kind = Ada_Decimal_Fixed_Point_Def then
                        return Get_Range_Op
                                 (D.As_Decimal_Fixed_Point_Def.F_Range);
                     else
                        return Get_Range_Op (D.As_Floating_Point_Def.F_Range);
                     end if;

                  when Ada_Derived_Type_Def =>
                     declare
                        T_Der : constant Derived_Type_Def :=
                                  D.As_Derived_Type_Def;
                     begin
                        return From_Subtype (T_Der.F_Subtype_Indication);
                     end;

                  when Ada_Array_Type_Def =>
                     declare
                        Indices : constant Array_Indices :=
                                    D.As_Array_Type_Def.F_Indices;
                     begin
                        if Indices.Kind = Ada_Constrained_Array_Indices then
                           declare
                              List : constant Constraint_List :=
                                       Indices.As_Constrained_Array_Indices
                                         .F_List;
                           begin
                              if List.Children_Count > 1 then
                                 Raise_Spec_Error
                                   (Node,
                                    Message =>
                                      "Arrays with multiple"
                                    & " dimentsion not supported.");
                              end if;

                              return Get_Range_Expr (List.Child (1)).As_Bin_Op;
                           end;

                        else
                           return No_Bin_Op;
                        end if;
                     end;

                  when others =>
                     return No_Bin_Op;
               end case;
            end;

         when Ada_Subtype_Decl =>
            return Get_Range_Expr (Node.As_Subtype_Decl.F_Subtype);

         when Ada_Subtype_Indication =>
            return From_Subtype (Node.As_Subtype_Indication);

         when others =>
            return No_Bin_Op;
      end case;
   end Get_Range_Expr;

   -------------------
   -- Get_Root_Type --
   -------------------

   function Get_Root_Type (Node : Subtype_Decl'Class) return Base_Type_Decl is
      T : constant Base_Type_Decl := Node.F_Subtype.P_Designated_Type_Decl;
   begin
      if T.Kind = Ada_Subtype_Decl then
         return Get_Root_Type (T.As_Subtype_Decl);
      else
         return T;
      end if;
   end Get_Root_Type;

   ----------------------
   --  Get_Vector_Type --
   ----------------------

   function Get_Vector_Type
     (Node : Type_Decl) return Base_Type_Decl
   is
      --  See Vector private tagged type implementation in a-convec

      P_Part : constant Base_Type_Decl :=
                 Node.As_Base_Type_Decl.P_Private_Completion;
      --  The private part of the record completion
      F_Def  : constant Type_Def := P_Part.As_Type_Decl.F_Type_Def;

      --  type Vector is new Controlled with record
      --    Elements : Elements_Access := null;
      --    Last     : Extended_Index := No_Index;
      --    TC       : aliased Tamper_Counts;
      --  end record with Put_Image => Put_Image;

      --  The record defintion
      R_Def  : constant Base_Record_Def :=
                 F_Def.As_Derived_Type_Def.F_Record_Extension;
      --  The record extension
      R_Comp : constant Component_List := R_Def.F_Components;
      --  All components of the record
      Comp_1 : constant Component_Decl :=
                 R_Comp.F_Components.Child (1).As_Component_Decl;
      --  First component is the access type to a record containing
      --  the element's array of type T
      T_Comp : constant Type_Expr :=
                 Comp_1.F_Component_Def.F_Type_Expr;
      --  Subtype of the access type
      R_Type : constant Base_Type_Decl := T_Comp.P_Designated_Type_Decl;
      --  This is the access type to the record

      E_Def  : constant Type_Def := R_Type.As_Type_Decl.F_Type_Def;
      --  The type of the record of element's array

      --  type Elements_Type (Last : Extended_Index) is limited record
      --    EA : Elements_Array (Index_Type'First .. Last);
      --  end record;

      E_Rec : constant Base_Type_Decl :=
                E_Def.As_Type_Access_Def
                  .F_Subtype_Indication.P_Designated_Type_Decl;
      --  The Elements_Type record
      A_Def : constant Type_Def := E_Rec.As_Type_Decl.F_Type_Def;
      --  The Elements_Type type definition
      A_Rec : constant Base_Record_Def :=
                A_Def.As_Record_Type_Def.F_Record_Def;
      --  The record components
      A_Comp : constant Component_List := A_Rec.F_Components;
      A_C1   : constant Component_Decl :=
                 A_Comp.F_Components.Child (1).As_Component_Decl;
      --  The first component (EA above)
      A_CDef : constant Type_Expr :=
                 A_C1.F_Component_Def.F_Type_Expr;
      A_Type : constant Base_Type_Decl := A_CDef.P_Designated_Type_Decl;
      --  This Element_Array subtype

      --  type Elements_Array is
      --    array (Index_Type range <>) of aliased Element_Type;

      El_Typ : constant Type_Expr :=
                 A_Type.As_Type_Decl.F_Type_Def
                   .As_Array_Type_Def.F_Component_Type.F_Type_Expr;
      --  The subtype of the array's component
      El_Dcl : constant Base_Type_Decl :=
                 El_Typ.As_Subtype_Indication.P_Designated_Type_Decl;
      --  The actual array's element type
   begin
      --  Get the type for this access type
      return El_Dcl;
   end Get_Vector_Type;

   ----------------
   -- Name_Space --
   ----------------

   function Name_Space (Node : Ada_Node'Class) return String is
      NS : String := Unit_Name (Node);
   begin
      Strings.Fixed.Translate (NS, Strings.Maps.To_Mapping (".", "/"));

      declare
         Res : String (1 .. NS'Length + Strings.Fixed.Count (NS, "/") * 4);
         I   : Natural := 0;
      begin
         for K in NS'Range loop
            I := I + 1;
            if NS (K) = '/' then
               Res (I .. I + 4) := "_pkg/";
               I := I + 4;
            else
               Res (I) := NS (K);
            end if;
         end loop;

         return SOAP.Name_Space.Value (SOAP.Name_Space.AWS) & Res & "_pkg/";
      end;
   end Name_Space;

   -----------
   -- Start --
   -----------

   procedure Start is

      function Load_Project return LaL.Unit_Provider_Reference;
      --  Load the project file designated by the first command-line argument

      function Parser (Node : Ada_Node'Class) return Visit_Status;
      --  Main LaL parser callback

      ------------------
      -- Load_Project --
      ------------------

      function Load_Project return LaL.Unit_Provider_Reference is
         package GPR renames GNATCOLL.Projects;
         package LAL_GPR renames Libadalang.Project_Provider;

         use type GNATCOLL.VFS.Filesystem_String;

         Project          : constant GPR.Project_Tree_Access :=
                              new GPR.Project_Tree;
         Project_Filename : constant String :=
                              To_String (Options.Project_Filename);
         Project_File     : constant GNATCOLL.VFS.Virtual_File :=
                              GNATCOLL.VFS.Create (+Project_Filename);
         Env              : GPR.Project_Environment_Access;
      begin
         GPR.Initialize (Env);
         Project.Load (Project_File, Env, Report_Missing_Dirs => False);
         return LAL_GPR.Create_Project_Unit_Provider (Project, Env => Env);
      end Load_Project;

      ------------
      -- Parser --
      ------------

      function Parser (Node : Ada_Node'Class) return Visit_Status is

         Result : constant Visit_Status := Into;

         procedure Analyze_Package (Node : Ada_Node'Class);
         --  A package declaration, the main goal is to create the
         --  corresponding name-space.

         procedure Analyze_Routine (Node : Ada_Node'Class);
         --  A procedure or function declaration, analyse the parameters

         procedure Analyze_Package_Instantiation (Node : Ada_Node'Class);
         --  Analyze a package instantiation in the spec

         ---------------------
         -- Analyze_Package --
         ---------------------

         procedure Analyze_Package (Node : Ada_Node'Class) is
            Self : constant Package_Decl := As_Package_Decl (Node);
         begin
            if Options.WS_Name = Null_Unbounded_String then
               Options.WS_Name := To_Unbounded_String
                 (Strings.Fixed.Translate
                    (Img (Self.F_Package_Name),
                     Strings.Maps.To_Mapping (".", "-")));
            end if;
         end Analyze_Package;

         -----------------------------------
         -- Analyze_Package_Instantiation --
         -----------------------------------

         procedure Analyze_Package_Instantiation (Node : Ada_Node'Class) is

            function T_Decl (E : Expr) return Ada_Node'Class;
            --  Return the type declaration to use for the name-space

            function T_Name (E : Expr) return Ada_Node'Class;
            --  Return the type name

            ------------
            -- T_Decl --
            ------------

            function T_Decl (E : Expr) return Ada_Node'Class is
            begin
               if E.Kind = Ada_Dotted_Name then
                  return E.As_Dotted_Name.P_Name_Designated_Type;
               else
                  return E;
               end if;
            end T_Decl;

            ------------
            -- T_Name --
            ------------

            function T_Name (E : Expr) return Ada_Node'Class is
            begin
               if E.Kind = Ada_Dotted_Name then
                  return E.As_Dotted_Name.P_Name_Designated_Type.F_Name;
               else
                  return E.As_Identifier;
               end if;
            end T_Name;

            G_Pck  : constant Generic_Package_Instantiation :=
                       Node.As_Generic_Package_Instantiation;
            G_Name : constant String :=
                       Img (G_Pck.F_Generic_Pkg_Name, Lower_Case => True);
         begin
            if G_Name = "ada.containers.vectors" then
               --  This is the instance of a standard vector container. The
               --  name of the package is the name of the array type in WSDL
               --  and the second parameter the array items' type.

               declare
                  Params : constant Assoc_List := G_Pck.F_Params;
               begin
                  if Params.Children_Count = 2 then
                     declare
                        P : constant array (1 .. 2) of Param_Assoc :=
                              [Params.List_Child (1).As_Param_Assoc,
                               Params.List_Child (2).As_Param_Assoc];
                        E : constant Expr := P (2).F_R_Expr;
                     begin
                        Generator.Start_Array
                          (NS             => Name_Space (Node),
                           Name           => Img (G_Pck.F_Name),
                           Component_NS   => Name_Space (T_Decl (E)),
                           Component_Type => Img (T_Name (E)),
                           Length         => 0);
                     end;
                  end if;
               end;
            end if;
         end Analyze_Package_Instantiation;

         ---------------------
         -- Analyze_Routine --
         ---------------------

         procedure Analyze_Routine (Node : Ada_Node'Class) is

            procedure Analyze_Profile (Node : Subp_Spec'Class);

            ---------------------
            -- Analyze_Profile --
            ---------------------

            procedure Analyze_Profile (Node : Subp_Spec'Class) is
               Parameters : constant Param_Spec_Array := Node.P_Params;
            begin
               for P of Parameters loop
                  declare
                     P_Names : constant Defining_Name_List := P.F_Ids;
                     P_Mode  : constant Ada_Mode := P.F_Mode;
                     P_Type  : constant Type_Expr := P.F_Type_Expr;
                  begin
                     if not (P_Mode = Ada_Mode_In
                             or else P_Mode = Ada_Mode_Default)
                     then
                        Raise_Spec_Error
                          (P, Message => "only in mode supported.");
                     end if;

                     --  Iterate over all parameters

                     for Name of P_Names loop
                        declare
                           Def : constant Generator.Type_Data :=
                                   Type_Definition (P_Type, Base => False);
                        begin
                           Generator.New_Formal
                             (NS       => To_String (Def.NS),
                              Var_Name => Img (Name),
                              Var_Type => To_String (Def.Name));
                        end;
                     end loop;
                  end;
               end loop;

               if Node.F_Subp_Kind.Kind = Ada_Subp_Kind_Function then
                  declare
                     R_Name    : constant String :=
                                   Img (Node.F_Subp_Name);
                     P_Returns : constant Type_Expr := Node.P_Returns;
                     Def       : constant Generator.Type_Data :=
                                   Type_Definition (P_Returns, Base => False);
                  begin
                     Generator.Return_Type
                       (To_String (Def.NS), To_String (Def.Name), R_Name);
                  end;
               end if;
            end Analyze_Profile;

            Self         : constant Subp_Spec := As_Subp_Spec (Node);
            Routine_Kind : constant Ada_Node_Kind_Type :=
                             Self.F_Subp_Kind.Kind;

         begin
            begin
               Generator.Start_Routine
                 (Name_Space (Node),
                  Img (Self.F_Subp_Name),
                  (if Routine_Kind = Ada_Subp_Kind_Function
                   then "function "
                   else "procedure"));
            exception
               when E : Spec_Error =>
                  Raise_Spec_Error (Node, Exception_Message (E));
            end;

            Analyze_Profile (Self);
         end Analyze_Routine;

      begin
         case Node.Kind is
            when Ada_Package_Decl =>
               Analyze_Package (Node);

            when Ada_Subp_Spec =>
               Analyze_Routine (Node);

            when Ada_Type_Decl =>
               Analyze_Type (Node.As_Type_Decl);

            when Ada_Subtype_Decl =>
               Analyze_Subtype (Node.As_Subtype_Decl);

            when Ada_Generic_Package_Instantiation =>
               Analyze_Package_Instantiation (Node);

            when others =>
               null;
         end case;

         return Result;

      exception
         when Langkit_Support.Errors.Precondition_Failure =>
            Raise_Spec_Error (Node, "syntax error");
      end Parser;

      Context  : constant Analysis_Context :=
                   Create_Context
                     (Unit_Provider =>
                        (if Options.Project_Filename = Null_Unbounded_String
                         then No_Unit_Provider_Reference
                         else Load_Project));
      Unit     : Analysis_Unit;

   begin
      Unit := Get_From_File (Context, To_String (Options.File_Name));

      --  check for diagnostic infor in unit

      if Unit.Has_Diagnostics then
         for D of Unit.Diagnostics loop
            Text_IO.Put_Line (Unit.Format_GNU_Diagnostic (D));
         end loop;

         raise Fatal_Error;

      else
         Traverse (Root (Unit), Parser'Access);

         declare
            Prev_Index : Natural;
            First      : Positive := Deferred_Types'First;
         begin
            --  When analysing the deferred types we could have some
            --  more types discoverred. Do the analyse of the
            --  defintions until there is no more added into the
            --  deferred list.

            loop
               Prev_Index := Index;

               for K in First .. Index loop
                  Analyze_Type (Deferred_Types (K));
               end loop;

               exit when Prev_Index = Index;

               First := Prev_Index;
            end loop;
         end;

         if not Options.Quiet then
            Text_IO.New_Line;
            Text_IO.Put_Line
              ("WSDL document " & To_String (Options.WSDL_File_Name)
               & " is created for " & To_String (Options.File_Name) & '.');
         end if;
      end if;
   end Start;

   --------------
   -- Type_Def --
   --------------

   function Type_Definition
     (Node : Ada_Node;
      Name : String;
      Decl : Ada_Node'Class;
      Base : Boolean) return Generator.Type_Data
   is

      function Register_Deferred
        (Node : Base_Type_Decl'Class) return Generator.Type_Data;
      --  Register a deferred type to be generated after first
      --  pass. Returns the name of the type.

      function Register_Deferred_I
        (Node  : Base_Type_Decl'Class;
         First : Internal_Integer := Internal_Integer'Last;
         Last  : Internal_Integer := Internal_Integer'First)
         return Generator.Type_Data;
      --  Same as above for integer and optional range

      function Register_Deferred_F
        (Node  : Base_Type_Decl'Class;
         First : Long_Float := Long_Float'Last;
         Last  : Long_Float := Long_Float'First)
         return Generator.Type_Data;
      --  Same as above for float and optional range

      function Register_Deferred_D
        (Node  : Base_Type_Decl'Class;
         First : SOAP.Types.Decimal := SOAP.Types.Decimal'Last;
         Last  : SOAP.Types.Decimal := SOAP.Types.Decimal'First)
         return Generator.Type_Data;
      --  Same as above for decimal and optional range

      function Build_Type
        (Name  : String;
         NS    : String := SOAP.Name_Space.Value (SOAP.Name_Space.XSD);
         First : Internal_Integer := Internal_Integer'Last;
         Last  : Internal_Integer := Internal_Integer'First)
         return Generator.Type_Data
      is (+NS, +Name,
          (if First = Internal_Integer'Last
           then Null_Unbounded_String
           else +Internal_Integer'Image (First)),
          (if Last = Internal_Integer'First
           then Null_Unbounded_String
           else +Internal_Integer'Image (Last)),
          Null_Unbounded_String);

      function Build_Type_F
        (Name  : String;
         NS    : String := SOAP.Name_Space.Value (SOAP.Name_Space.XSD);
         First : Long_Float := Long_Float'Last;
         Last  : Long_Float := Long_Float'First)
         return Generator.Type_Data
      is (+NS, +Name,
          (if First = Long_Float'Last
           then Null_Unbounded_String
           else +Long_Float'Image (First)),
          (if Last = Long_Float'First
           then Null_Unbounded_String
           else +Long_Float'Image (Last)),
          Null_Unbounded_String);

      function Build_Type_D
        (Name  : String;
         NS    : String := SOAP.Name_Space.Value (SOAP.Name_Space.XSD);
         First : SOAP.Types.Decimal := SOAP.Types.Decimal'Last;
         Last  : SOAP.Types.Decimal := SOAP.Types.Decimal'First)
         return Generator.Type_Data
      is (+NS, +Name,
          (if First = SOAP.Types.Decimal'Last
           then Null_Unbounded_String
           else +SOAP.Types.Decimal'Image (First)),
          (if Last = SOAP.Types.Decimal'First
           then Null_Unbounded_String
           else +SOAP.Types.Decimal'Image (Last)),
          Null_Unbounded_String);

      function Build_Array
        (Node : Base_Type_Decl) return Generator.Type_Data;
      --  Build an array

      function Build_Private
        (Node : Base_Type_Decl) return Generator.Type_Data;
      --  Build a private part

      function Build_Integer
        (Node : Base_Type_Decl) return Generator.Type_Data;
      --  Build an integer

      function Build_Float
        (Node : Base_Type_Decl) return Generator.Type_Data;
      --  Build a floating point

      function Build_Enumeration
        (Node : Base_Type_Decl) return Generator.Type_Data;
      --  Build an enumeration

      function Build_Fixed_Point
        (Node : Base_Type_Decl) return Generator.Type_Data;
      --  Build a fixed point

      function Build_Decimal_Fixed_Point
        (Node : Base_Type_Decl) return Generator.Type_Data;
      --  Build a decimal fixed point

      function Build_Modular
        (Node : Base_Type_Decl) return Generator.Type_Data;
      --  Build an unsigned type

      function Build_Derived
        (Node : Base_Type_Decl) return Generator.Type_Data;
      --  Build a derived type

      -----------------
      -- Build_Array --
      -----------------

      function Build_Array
        (Node : Base_Type_Decl) return Generator.Type_Data
      is
         T_Name : constant String := Characters.Handling.To_Lower (Name);
      begin
         if T_Name = "string" then
            return Build_Type ("string");

         elsif T_Name = "soap.types.token" then
            return Build_Type ("token");

         elsif T_Name = "soap.types.normalized_string" then
            return Build_Type ("normalizedString");

         elsif T_Name = "soap.types.any_uri" then
            return Build_Type ("any_uri");

         elsif T_Name in "soap_base64"
                       | "utils.soap_base64"
                       | "soap.utils.soap_base64"
         then
            return Build_Type ("SOAP_Base64");

         else
            if not Options.Quiet then
               Text_IO.Put_Line
                 (Location (Node)
                  & "warning: array " & T_Name
                  & " not supported by wsdl2aws, use Containers.Vector");
            end if;

            return Register_Deferred (Node);
         end if;
      end Build_Array;

      -------------------------------
      -- Build_Decimal_Fixed_Point --
      -------------------------------

      function Build_Decimal_Fixed_Point
        (Node : Base_Type_Decl) return Generator.Type_Data
      is
         T_Def    : constant Decimal_Fixed_Point_Def :=
                      Node.As_Type_Decl.F_Type_Def.As_Decimal_Fixed_Point_Def;
         T_Name   : constant String := Characters.Handling.To_Lower (Name);
         Ilb, Iub : SOAP.Types.Decimal;
      begin
         Get_Range (Node, Ilb, Iub);

         if T_Name = "soap.types.decimal" then
            return Build_Type ("decimal");

         elsif Base then
            return Build_Type_D ("decimal", First => Ilb, Last => Iub);

         else
            declare
               NS      : constant String := Name_Space (Decl);
               NS_Type : constant String := Name_Space (T_Def);
            begin
               --  If the type is not in the current package (so in
               --  different name space). We need to analyse it later
               --  so, we do register a differred analysis for this type.

               if NS = NS_Type then
                  return Build_Type_D (Name, NS, Ilb, Iub);
               else
                  return Register_Deferred_D (Node, Ilb, Iub);
               end if;
            end;
         end if;
      end Build_Decimal_Fixed_Point;

      -------------------
      -- Build_Derived --
      -------------------

      function Build_Derived
        (Node : Base_Type_Decl) return Generator.Type_Data
      is
         T_Name : constant String := Characters.Handling.To_Lower (Name);
      begin
         if T_Name = "soap.types.local_date"
           or else T_Name = "local_date"
         then
            return Build_Type ("date");

         elsif T_Name = "soap.types.local_time"
           or else T_Name = "local_time"
         then
            return Build_Type ("time");

         else
            if Base then
               declare
                  T_Def  : constant Type_Def :=
                             Node.As_Type_Decl.F_Type_Def;
                  P_Type : constant Base_Type_Decl :=
                             Get_Base_Type (T_Def.As_Derived_Type_Def);
                  Name   : constant String := Img (P_Type.F_Name);
                  Def    : Generator.Type_Data :=
                             Type_Definition
                               (P_Type.As_Ada_Node, Name, P_Type, False);
               begin
                  Get_Range_Derived (Node, Def.Min, Def.Max);

                  return Def;
               end;

            else
               return Register_Deferred (Node);
            end if;
         end if;
      end Build_Derived;

      -----------------------
      -- Build_Enumeration --
      -----------------------

      function Build_Enumeration
        (Node : Base_Type_Decl) return Generator.Type_Data
      is
         T_Name : constant String := Characters.Handling.To_Lower (Name);
      begin
         if T_Name = "character" then
            return Build_Type ("character");

         elsif T_Name = "boolean" then
            return Build_Type ("boolean");

         else
            if Options.Enum_To_String then
               return Build_Type ("string");
            else
               return Register_Deferred (Node);
            end if;
         end if;
      end Build_Enumeration;

      -----------------------
      -- Build_Fixed_Point --
      -----------------------

      function Build_Fixed_Point
        (Node : Base_Type_Decl) return Generator.Type_Data
      is
         T_Def    : constant Ordinary_Fixed_Point_Def :=
                      Node.As_Type_Decl.F_Type_Def.As_Ordinary_Fixed_Point_Def;
         T_Name   : constant String := Characters.Handling.To_Lower (Name);
         Ilb, Iub : Long_Float;
      begin
         Get_Range (Node, Ilb, Iub);

         if T_Name = "duration" then
            return Build_Type ("duration");

         elsif Base then
            if Ilb = Long_Float (Float'First)
              and then Iub = Long_Float (Float'Last)
            then
               return Build_Type_F ("float");

            elsif Ilb = Long_Float'First and then Iub = Long_Float'Last then
               return Build_Type_F ("long_float");

            elsif Ilb >=  Long_Float (Float'First)
              and then Iub <= Long_Float (Float'Last)
            then
               return Build_Type_F
                 ("float", First => Ilb, Last => Iub);

            else
               return Build_Type_F
                 ("long_float", First => Ilb, Last => Iub);
            end if;

         else
            declare
               NS      : constant String := Name_Space (Decl);
               NS_Type : constant String := Name_Space (T_Def);
            begin
               --  If the type is not in the current package (so in
               --  different name space). We need to analyse it later
               --  so, we do register a differred analysis for this type.

               if NS = NS_Type then
                  return Build_Type_F (Name, NS);
               else
                  return Register_Deferred_F (Node, Ilb, Iub);
               end if;
            end;
         end if;
      end Build_Fixed_Point;

      -----------------
      -- Build_Float --
      -----------------

      function Build_Float
        (Node : Base_Type_Decl) return Generator.Type_Data
      is
         T_Def    : constant Floating_Point_Def :=
                      Node.As_Type_Decl.F_Type_Def.As_Floating_Point_Def;
         Dig      : constant Integer :=
                      Integer'Value (Img (T_Def.F_Num_Digits));
         Ilb, Iub : Long_Float;
      begin
         Get_Range (Node, Ilb, Iub);

         if Base then
            if Dig <= Float'Digits then
               if Ilb = Long_Float (Float'First)
                 and then Iub = Long_Float (Float'Last)
               then
                  return Build_Type_F ("float");
               else
                  return Build_Type_F
                    ("float", First => Ilb, Last => Iub);
               end if;

            else
               if Ilb = Long_Float'First and then Iub = Long_Float'Last then
                  return Build_Type_F ("long_float");
               else
                  return Build_Type_F
                    ("long_float", First => Ilb, Last => Iub);
               end if;
            end if;

         else
            declare
               NS      : constant String := Name_Space (Decl);
               NS_Type : constant String := Name_Space (Node);
            begin
               --  If the type is not un the current package (so in
               --  different name space). We need to analyse it later
               --  so, we do register a differred analysis for this type.

               if NS = NS_Type or else Is_Standard (Node) then
                  return Build_Type_F (Name, NS);
               else
                  return Register_Deferred_F (Node, Ilb, Iub);
               end if;
            end;
         end if;
      end Build_Float;

      -------------------
      -- Build_Integer --
      -------------------

      function Build_Integer
        (Node : Base_Type_Decl) return Generator.Type_Data
      is
         Ilb, Iub : Internal_Integer;
      begin
         Get_Range (Node, Ilb, Iub);

         --  Special case for SOAP.Types

         if Base then
            if Ilb = Internal_Integer (SOAP.Types.Byte'First)
              and then Iub = Internal_Integer (SOAP.Types.Byte'Last)
            then
               return Build_Type ("byte");

            elsif Ilb >= Internal_Integer (SOAP.Types.Byte'First)
              and then Iub <= Internal_Integer (SOAP.Types.Byte'Last)
            then
               return Build_Type ("byte", First => Ilb, Last => Iub);

            elsif Ilb = Internal_Integer (SOAP.Types.Short'First)
              and then
                Iub = Internal_Integer (SOAP.Types.Short'Last)
            then
               return Build_Type ("short");

            elsif Ilb >= Internal_Integer (SOAP.Types.Short'First)
              and then
                Iub <= Internal_Integer (SOAP.Types.Short'Last)
            then
               return Build_Type ("short", First => Ilb, Last => Iub);

            elsif Ilb = Internal_Integer (Integer'First)
              and then Iub = Internal_Integer (Integer'Last)
            then
               return Build_Type ("integer");

            elsif Ilb >= Internal_Integer (Integer'First)
              and then Iub <= Internal_Integer (Integer'Last)
            then
               return Build_Type
                 ("integer", First => Ilb, Last => Iub);

            elsif Ilb = Internal_Integer (Long_Integer'First)
              and then Iub = Internal_Integer (Long_Integer'Last)
            then
               return Build_Type ("long");

            else
               return Build_Type ("long", First => Ilb, Last => Iub);
            end if;

         else
            declare
               NS      : constant String := Name_Space (Decl);
               NS_Type : constant String := Name_Space (Node);
            begin
               --  If the type is not in the current package (so in
               --  different name space). We need to analyse it later
               --  so, we do register a differred analysis for this type.

               if NS = NS_Type or else Is_Standard (Node) then
                  return Build_Type (Name, NS);
               else
                  return Register_Deferred_I (Node, Ilb, Iub);
               end if;
            end;
         end if;
      end Build_Integer;

      -------------------
      -- Build_Modular --
      -------------------

      function Build_Modular
        (Node : Base_Type_Decl) return Generator.Type_Data
      is
         T_Def   : constant Type_Def := Node.As_Type_Decl.F_Type_Def;
         Modulus : constant SOAP.Types.Unsigned_Long :=
                     Compute_Value (T_Def.As_Mod_Int_Type_Def.F_Expr);

         --------------
         -- Get_Last --
         --------------

         function Get_Last return Internal_Integer is
           (if Modulus = 0
            then Internal_Integer'First
            else Internal_Integer (Modulus - 1));

      begin
         if Base then
            if Modulus = SOAP.Types.Unsigned_Long
              (SOAP.Types.Unsigned_Byte'Modulus)
            then
               return Build_Type ("unsigned_byte");

            elsif Modulus < SOAP.Types.Unsigned_Long
              (SOAP.Types.Unsigned_Byte'Modulus)
            then
               return Build_Type ("unsigned_byte", Last => Get_Last);

            elsif Modulus = SOAP.Types.Unsigned_Long
              (SOAP.Types.Unsigned_Short'Modulus)
            then
               return Build_Type ("unsigned_short");

            elsif Modulus < SOAP.Types.Unsigned_Long
              (SOAP.Types.Unsigned_Short'Modulus)
            then
               return Build_Type ("unsigned_short", Last => Get_Last);

            elsif Modulus = SOAP.Types.Unsigned_Long
              (SOAP.Types.Unsigned_Int'Modulus)
            then
               return Build_Type ("unsigned_int");

            elsif Modulus < SOAP.Types.Unsigned_Long
              (SOAP.Types.Unsigned_Int'Modulus)
            then
               return Build_Type ("unsigned_int", Last => Get_Last);

            elsif Modulus < SOAP.Types.Unsigned_Long
              (SOAP.Types.Unsigned_Long'Modulus - 1)
            then
               return Build_Type ("unsigned_long", Last => Get_Last);

            else
               return Build_Type ("unsigned_long");
            end if;

         else
            declare
               NS      : constant String := Name_Space (Decl);
               NS_Type : constant String := Name_Space (T_Def);
            begin
               --  If the type is not in the current package (so in
               --  different name space). We need to analyse it later
               --  so, we do register a differred analysis for this type.

               if NS = NS_Type or else Is_Standard (Node) then
                  return Build_Type (Name, NS_Type, Last => Get_Last);
               else
                  return Register_Deferred (Node);
               end if;
            end;
         end if;
      end Build_Modular;

      -------------------
      -- Build_Private --
      -------------------

      function Build_Private
        (Node : Base_Type_Decl) return Generator.Type_Data
      is
         T_Name : constant String := Characters.Handling.To_Lower (Name);
      begin
         if T_Name in "unbounded_string"
                    | "unbounded.unbounded_string"
                    | "strings.unbounded.unbounded_string"
                    | "ada.strings.unbounded.unbounded_string"
         then
            if Base then
               return Build_Type ("string");
            else
               return Build_Type ("unbounded_string");
            end if;

         elsif T_Name in "big_integer"
           | "ada.numerics.big_numbers.big_integers.big_integer"
         then
            return Build_Type ("big_integer");

         elsif T_Name in "time" | "calendar.time" | "ada.calendar.time"
           and then Is_Calendar (Node)
         then
            return Build_Type ("datetime");

         elsif T_Name = "soap.types.local_date_time" then
            return Build_Type ("datetime");

         elsif T_Name = "soap.types.local_time" then
            return Build_Type ("time");

         elsif T_Name = "soap.types.local_date" then
            return Build_Type ("date");

         elsif T_Name = "soap.types.token" then
            return Build_Type ("token");

         elsif Decl.Kind = Ada_Subtype_Indication
           and then Img (Node.As_Concrete_Type_Decl.F_Name, True) = "vector"
         then
            declare
               S_Type : constant Libadalang.Analysis.Name :=
                          Decl.As_Subtype_Indication.F_Name
                            .As_Dotted_Name.F_Prefix;
            begin
               return Build_Type
                 (Img (S_Type), Name_Space (S_Type.P_Referenced_Decl));
            end;

         else
            Raise_Spec_Error
              (Node, Message => "unsupported private type " & Name);
         end if;
      end Build_Private;

      -----------------------
      -- Register_Deferred --
      -----------------------

      function Register_Deferred
        (Node : Base_Type_Decl'Class) return Generator.Type_Data
      is
         T_Name : constant String := Img (Node.F_Name);
      begin
         Append_Deferred (Node);
         return Build_Type (T_Name, Name_Space (Node));
      end Register_Deferred;

      -------------------------
      -- Register_Deferred_D --
      -------------------------

      function Register_Deferred_D
        (Node  : Base_Type_Decl'Class;
         First : SOAP.Types.Decimal := SOAP.Types.Decimal'Last;
         Last  : SOAP.Types.Decimal := SOAP.Types.Decimal'First)
         return Generator.Type_Data
      is
         T_Name : constant String := Img (Node.F_Name);
      begin
         Append_Deferred (Node);
            return Build_Type_D (T_Name, Name_Space (Node), First, Last);
      end Register_Deferred_D;

      -------------------------
      -- Register_Deferred_F --
      -------------------------

      function Register_Deferred_F
        (Node  : Base_Type_Decl'Class;
         First : Long_Float := Long_Float'Last;
         Last  : Long_Float := Long_Float'First) return Generator.Type_Data
      is
         T_Name : constant String := Img (Node.F_Name);
      begin
         Append_Deferred (Node);
         return Build_Type_F (T_Name, Name_Space (Node), First, Last);
      end Register_Deferred_F;

      -------------------------
      -- Register_Deferred_I --
      -------------------------

      function Register_Deferred_I
        (Node  : Base_Type_Decl'Class;
         First : Internal_Integer := Internal_Integer'Last;
         Last  : Internal_Integer := Internal_Integer'First)
         return Generator.Type_Data
      is
         T_Name : constant String := Img (Node.F_Name);
      begin
         Append_Deferred (Node);
         return Build_Type (T_Name, Name_Space (Node), First, Last);
      end Register_Deferred_I;

      T_Decl : constant Base_Type_Decl :=
                 (if Node.Kind = Ada_Subtype_Decl
                  then Node.As_Subtype_Decl.F_Subtype.P_Designated_Type_Decl
                  else Node.As_Base_Type_Decl);

      T_Def : constant Type_Def := T_Decl.As_Type_Decl.F_Type_Def;

   begin
      case T_Def.Kind is
         when Ada_Signed_Int_Type_Def =>
            return Build_Integer (Node.As_Base_Type_Decl);

         when Ada_Floating_Point_Def =>
            return Build_Float (Node.As_Base_Type_Decl);

         when Ada_Derived_Type_Def =>
            return Build_Derived (Node.As_Base_Type_Decl);

         when Ada_Record_Type_Def =>
            return Register_Deferred (Node.As_Base_Type_Decl);

         when Ada_Array_Type_Def =>
            return Build_Array (T_Decl);

         when Ada_Private_Type_Def =>
            return Build_Private (Node.As_Base_Type_Decl);

         when Ada_Enum_Type_Def =>
            return Build_Enumeration (Node.As_Base_Type_Decl);

         when Ada_Ordinary_Fixed_Point_Def =>
            return Build_Fixed_Point (Node.As_Base_Type_Decl);

         when Ada_Decimal_Fixed_Point_Def =>
            return Build_Decimal_Fixed_Point (Node.As_Base_Type_Decl);

         when Ada_Mod_Int_Type_Def =>
            return Build_Modular (Node.As_Base_Type_Decl);

         when Ada_Type_Access_Def =>
            Raise_Spec_Error
              (Node,
               Message => "access type " & Name & " unsupported in WSDL");

         when others =>
            Raise_Spec_Error
              (Node,
               Message => "unsupported element kind " & Name);
      end case;
   end Type_Definition;

   function Type_Definition
     (Node : Type_Decl'Class;
      Base : Boolean) return Generator.Type_Data
   is
      E_Name : constant Defining_Name := Node.As_Base_Type_Decl.F_Name;
      T_Name : constant String := Img (E_Name);
   begin
      return Type_Definition (Node.As_Ada_Node, T_Name, Node, Base);
   end Type_Definition;

   function Type_Definition
     (Node : Type_Expr'Class;
      Base : Boolean) return Generator.Type_Data
   is
      E_Name : constant Name := Node.P_Type_Name;
      T_Name : constant String := Img (E_Name);
      Decl   : Base_Type_Decl := Node.P_Designated_Type_Decl;
   begin
      if Decl.Kind = Ada_Subtype_Decl then
         Decl := Decl.As_Subtype_Decl.F_Subtype.P_Designated_Type_Decl;
      end if;

      return Type_Definition (Decl.As_Ada_Node, T_Name, Node, Base);
   end Type_Definition;

end Ada2WSDL.Parser;
