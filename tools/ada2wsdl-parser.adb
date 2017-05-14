------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2003-2017, AdaCore                     --
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

with Ada.Characters.Conversions;
with Ada.Characters.Handling;
with Ada.Directories;
with Ada.Exceptions;
with Ada.Strings;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Ada.Unchecked_Deallocation;

with GNAT.OS_Lib;

with Asis;
with Asis.Ada_Environments;
with Asis.Compilation_Units;
with Asis.Declarations;
with Asis.Definitions;
with Asis.Elements;
with Asis.Errors;
with Asis.Exceptions;
with Asis.Expressions;
with Asis.Extensions.Flat_Kinds;
with Asis.Implementation;
with Asis.Iterator;
with Asis.Text;

with A4G.GNAT_Int;

with AWS.Utils;
with SOAP.Name_Space;
with SOAP.Types;

with Ada2WSDL.Generator;
with Ada2WSDL.Options;

package body Ada2WSDL.Parser is

   use Ada;
   use Ada.Exceptions;
   use Ada.Strings.Unbounded;
   use Asis;
   use GNAT;

   --  The Compile routine is defined in Asis.Extensions in recent ASIS
   --  version, it was defined in A4G.GNAT_Int in older ones.

   use A4G.GNAT_Int;
   use Asis.Extensions;
   pragma Warnings (Off, A4G.GNAT_Int);

   use type Asis.Errors.Error_Kinds;

   subtype String_Access is OS_Lib.String_Access;

   ------------------------------
   -- File and Directory names --
   ------------------------------

   Tree_Name    : String_Access;
   --  We need it in more, then one routine, so we define it here

   Max_Argument : constant := 1_024;

   Arg_List     : OS_Lib.Argument_List (1 .. Max_Argument);
   --  -I options from the Ada2WSDL command line transformed into the
   --  form appropriate for calling gcc to create the tree file.

   Arg_Index    : Natural := 0;

   GPRBUILD     : constant String_Access :=
                    OS_Lib.Locate_Exec_On_Path ("gprbuild");

   ----------------------
   -- Status variables --
   ----------------------

   My_Context : Asis.Context;

   Spec_File  : Text_IO.File_Type;

   -----------------------
   -- Local subprograms --
   -----------------------

   procedure Create_Tree;
   --  Creates a tree file or checks if the tree file already exists,
   --  depending on options

   type Element_Node;
   type Link is access all Element_Node;

   type Element_Node is record
      Spec      : Asis.Element := Nil_Element;
      Spec_Name : String_Access;
      --  Not used for incomplete type declarations
      Up        : Link;
      Down      : Link;
      Prev      : Link;
      Next      : Link;
      Last      : Link;
   end record;
   --  An element of a dynamic structure representing a "skeleton" of the body
   --  to be generated
   --
   --  Logically this structure is a list of elements representing local
   --  bodies and sublists representing the bodies which are a components of
   --  some local body. Each list and sublist is represented by its first
   --  element. For this first list element, the field Last is used to point
   --  to the last element in this list to speed up adding the new element if
   --  we do not have to order alphabetically the local bodies.

   Body_Structure : aliased Element_Node;
   --  This is a "design" for a body to generate. It contains references
   --  to the elements from the argument spec for which body samples should
   --  be generated, ordered alphabetically. The top of this link structure
   --  is the Element representing a unit declaration from the argument
   --  compilation unit.

   ------------------------------------------------
   -- Actuals for Traverse_Element instantiation --
   ------------------------------------------------

   type Body_State is record
      Argument_Spec   : Boolean := True;
      --  Flag indicating if we are in the very beginning (very top)
      --  of scanning the argument library unit declaration

      Current_List    : Link;
      --  Declaration list in which a currently processed spec
      --  should be inserted;

      Last_Top        : Link;
      --  An element which represents a declaration from which the currently
      --  processed sublist was originated

      New_List_Needed : Boolean := False;
      --  Flag indication if a new sublist should be created
   end record;

   procedure Create_Element_Node
     (Element : Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Body_State);
   --  When visiting an Element representing something for which a body
   --  sample may be required, we check if the body is really required
   --  and insert the corresponding Element on the right place in Body_State
   --  if it is.

   procedure Go_Up
     (Element : Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Body_State);
   --  When leaving a [generic] package declaration or a protected [type]
   --  declaration, we have to go one step up in Body_State structure.

   procedure Create_Structure is new Iterator.Traverse_Element
     (State_Information => Body_State,
      Pre_Operation     => Create_Element_Node,
      Post_Operation    => Go_Up);
   --  Creates Body_Structure by traversing an argument spec and choosing
   --  specs to create body samples for

   --------------------
   -- Local Routines --
   --------------------

   function Name (Elem : Asis.Element) return String;
   --  Returns a defining name string image for a declaration which
   --  defines exactly one name. This should definitely be made an extension
   --  query

   function Image (Str : Wide_String) return String;
   --  Returns the trimed string representation of Str

   procedure Analyse_Structure;
   --  Go through all entities and generate WSDL

   procedure Emergency_Clean_Up;
   --  Does clean up actions in case if an exception was raised during
   --  creating a body sample (closes a Context, dissociates it, finalizes
   --  ASIS, closes and deletes needed files.

   ------------------------------------
   -- Deferred Asis types to analyse --
   ------------------------------------

   type Element_Set is array (Positive range <>) of Asis.Element;

   Max_Deferred_Types : constant := 1_024;

   Deferred_Types : Element_Set (1 .. Max_Deferred_Types);
   --  Records all types tha can't be analysed at some point. For example we
   --  can't parse a type while parsing the spec.

   Index          : Natural := 0;
   --  Current Index in the Deferred_Types array

   ----------------
   -- Add_Option --
   ----------------

   procedure Add_Option (Option : String) is
   begin
      if Option (Option'First .. Option'First + 1) /= "-I" then
         Arg_Index := Arg_Index + 1;
         Arg_List (Arg_Index) := new String'(Option);
      end if;
   end Add_Option;

   -----------------------
   -- Analyse_Structure --
   -----------------------

   procedure Analyse_Structure is

      procedure Analyse_Package (Node : Link);
      --  Analyse a package declaration, the package name is used as
      --  the Web Service name.

      procedure Analyse_Package_Instantiation (Node : Link);
      --  Checks if this instantiation is to create a safe pointer, in this
      --  case records the type and access type for later used.

      procedure Analyse_Routine (Node : Link);
      --  Node is a procedure or function, analyse its spec profile

      procedure Analyse_Type (Elem : Asis.Element);
      --  Node is a subtype or type, analyse its definition

      procedure Analyse_Profile (Node : Link);
      --  Generates an entry_body_formal_part, parameter or parameter
      --  and result profile for the body of a program unit
      --  represented by Node. Upon exit, sets Change_Line is set True
      --  if the following "is" for the body should be generated on a new line

      procedure Array_Type_Suffix
        (E           : Asis.Element;
         Type_Suffix : out Unbounded_String;
         Length      : out Positive);

      function Type_Def
        (Elem : Asis.Element;
         Base : Boolean) return Generator.Type_Data;
      --  Returns the type name for Elem. If Base is true the returned name is
      --  the base type for the given element.

      procedure Analyse_Node (Node : Link);
      --  Analyse a Node, handles procedure or function only

      procedure Analyse_Node_List (List : Link);
      --  Call Analyse_Node for each element in List

      function Name_Space (E : Asis.Element) return String;
      --  Returns the name space for element E. Name space is defined as
      --  follow: http://soapaws/<unit_name>_pkg/

      ------------------
      -- Analyse_Node --
      ------------------

      procedure Analyse_Node (Node : Link) is
         use Extensions.Flat_Kinds;

         Arg_Kind : constant Flat_Element_Kinds :=
                      Flat_Element_Kind (Node.Spec);
      begin
         case Arg_Kind is

            when A_Function_Declaration
              | A_Procedure_Declaration
              =>
               Analyse_Routine (Node);

            when An_Entry_Declaration
              | A_Single_Protected_Declaration
              | A_Protected_Type_Declaration
              | A_Single_Task_Declaration
              | A_Task_Type_Declaration
              | A_Generic_Package_Declaration
              | An_Incomplete_Type_Declaration
              | A_Generic_Function_Declaration
              | A_Generic_Procedure_Declaration
              =>
               null;

            when A_Package_Declaration =>
               Analyse_Package (Node);

            when A_Package_Instantiation =>
               Analyse_Package_Instantiation (Node);

            when A_Subtype_Declaration | An_Ordinary_Type_Declaration =>
               Analyse_Type (Node.Spec);

            when others =>
               Text_IO.Put_Line
                 (Text_IO.Standard_Error,
                  "ada2wsdl: unexpected element in the body structure");
               Text_IO.Put
                 (Text_IO.Standard_Error,
                  "ada2wsdl: " & Arg_Kind'Img);
               raise Fatal_Error;
         end case;

         if Node.Down /= null then
            Analyse_Node_List (Node.Down);
         end if;
      end Analyse_Node;

      -----------------------
      -- Analyse_Node_List --
      -----------------------

      procedure Analyse_Node_List (List : Link) is
         Next_Node  : Link;
         List_Start : Link := List;
      begin
         --  Here we have to go to the beginning of the list

         while List_Start.Prev /= null loop
            List_Start := List_Start.Prev;
         end loop;

         Next_Node := List_Start;

         loop
            Analyse_Node (Next_Node);

            if Next_Node.Next /= null then
               Next_Node := Next_Node.Next;
            else
               exit;
            end if;
         end loop;
      end Analyse_Node_List;

      ---------------------
      -- Analyse_Package --
      ---------------------

      procedure Analyse_Package (Node : Link) is
      begin
         if Options.WS_Name = Null_Unbounded_String then
            Options.WS_Name := To_Unbounded_String
              (Strings.Fixed.Translate
                 (Node.Spec_Name.all,
                  Strings.Maps.To_Mapping (".", "-")));
         end if;
      end Analyse_Package;

      -----------------------------------
      -- Analyse_Package_Instantiation --
      -----------------------------------

      procedure Analyse_Package_Instantiation (Node : Link) is
         use Extensions.Flat_Kinds;
         G_Unit : constant Asis.Expression :=
                    Declarations.Generic_Unit_Name (Node.Spec);
         --  The generic unit name (name after the reserved word is)
         G_Name : constant String := Image (Text.Element_Image (G_Unit));
      begin
         if G_Name = "SOAP.Utils.Safe_Pointers" then
            --  This is the safe pointer AWS/SOAP runtime type support

            declare
               Actual : Asis.Association_List :=
                          Declarations.Generic_Actual_Part (Node.Spec);
            begin
               if Actual'Length = 2 then
                  --  There is only two formal parameters, the first one is
                  --  the type, the second the access type to the first one.

                  for K in Actual'Range loop
                     Actual (K) := Expressions.Actual_Parameter (Actual (K));
                  end loop;

                  Generator.Register_Safe_Pointer
                    (Name        => Node.Spec_Name.all,
                     Type_Name   => Image (Text.Element_Image (Actual (1))),
                     Access_Name => Image (Text.Element_Image (Actual (2))));
               end if;
            end;
         end if;
      end Analyse_Package_Instantiation;

      ----------------------
      -- Analyse_Profile --
      ----------------------

      procedure Analyse_Profile (Node : Link) is

         use Extensions.Flat_Kinds;

         Arg_Kind   : constant Flat_Element_Kinds :=
                        Flat_Element_Kind (Node.Spec);
         Parameters : constant Asis.Element_List :=
                        Declarations.Parameter_Profile (Node.Spec);
      begin
         if not Elements.Is_Nil (Parameters) then

            for I in Parameters'Range loop
               declare
                  Elem  : constant Asis.Element :=
                            Declarations.Declaration_Subtype_Mark
                              (Parameters (I));
                  Mode  : constant Asis.Mode_Kinds :=
                            Elements.Mode_Kind (Parameters (I));
                  Names : constant Defining_Name_List :=
                            Declarations.Names (Parameters (I));
                  E     : Asis.Element := Elem;
               begin
                  --  For each name create a new formal parameter

                  if not (Mode = An_In_Mode
                          or else Mode = A_Default_In_Mode)
                  then
                     Raise_Spec_Error
                       (Parameters (I),
                        Message => "only in mode supported.");
                  end if;

                  if Elements.Expression_Kind (E) = A_Selected_Component then
                     E := Expressions.Selector (E);
                  end if;

                  E := Expressions.Corresponding_Name_Declaration (E);

                  E := Declarations.Type_Declaration_View (E);

                  for K in Names'Range loop
                     Generator.New_Formal
                       (NS       => Name_Space (E),
                        Var_Name => Image (Text.Element_Image (Names (K))),
                        Var_Type => To_String
                          (Type_Def (Elem, Base => False).Name));
                  end loop;
               end;
            end loop;
         end if;

         if Arg_Kind = A_Function_Declaration
           or else Arg_Kind = A_Generic_Function_Declaration
         then
            declare
               Elem : constant Asis.Element :=
                        Declarations.Result_Profile (Node.Spec);
               E    : Asis.Element := Elem;
            begin
               if Elements.Expression_Kind (E) = A_Selected_Component then
                  E := Expressions.Selector (E);
               end if;

               E := Expressions.Corresponding_Name_Declaration (E);

               E := Declarations.Type_Declaration_View (E);

               Generator.Return_Type
                 (Name_Space (E),
                  To_String (Type_Def (Elem, Base => False).Name));
            end;
         end if;
      end Analyse_Profile;

      ---------------------
      -- Analyse_Routine --
      ---------------------

      procedure Analyse_Routine (Node : Link) is
         use Extensions.Flat_Kinds;

         Arg_Kind : constant Flat_Element_Kinds :=
                      Flat_Element_Kind (Node.Spec);
      begin
         begin
            if Arg_Kind = A_Function_Declaration then
               Generator.Start_Routine (Node.Spec_Name.all, "function ");
            else
               Generator.Start_Routine (Node.Spec_Name.all, "procedure");
            end if;
         exception
            when E : Spec_Error =>
               Raise_Spec_Error (Node.Spec, Exception_Message (E));
         end;

         Analyse_Profile (Node);
      end Analyse_Routine;

      ------------------
      -- Analyse_Type --
      ------------------

      procedure Analyse_Type (Elem : Asis.Definition) is
         use Extensions.Flat_Kinds;

         type U_Array_Def is record
            Name, NS, Comp_Type : Unbounded_String;
            Length              : Positive;
         end record;

         Deferred_U_Arrays : array (1 .. 100) of U_Array_Def;
         U_Array_Index     : Natural := 0;
         Def               : Generator.Type_Data;

         procedure Analyse_Field (Component : Asis.Element);
         --  Analyse a field from the record

         procedure Analyse_Array_Component (Component : Asis.Element);
         --  Analyse an array component

         function Is_Standard (T : Asis.Declaration) return Boolean;
         --  Returns True if type T is declared in Standard

         -----------------------------
         -- Analyse_Array_Component --
         -----------------------------

         procedure Analyse_Array_Component (Component : Asis.Element) is
            E : Asis.Element :=
                  (Definitions.Subtype_Mark
                     (Definitions.Component_Subtype_Indication (Component)));
         begin
            if Elements.Expression_Kind (E) = A_Selected_Component then
               E := Expressions.Selector (E);
            end if;

            E := Declarations.Corresponding_First_Subtype
              (Expressions.Corresponding_Name_Declaration (E));

            Analyse_Type (E);
         end Analyse_Array_Component;

         -------------------
         -- Analyse_Field --
         -------------------

         procedure Analyse_Field (Component : Asis.Element) is

            ODV   : constant Asis.Element :=
                      Declarations.Object_Declaration_View (Component);
            Elem  : constant Asis.Element :=
                      Definitions.Subtype_Mark
                        (Definitions.Component_Subtype_Indication (ODV));
            Names : constant Defining_Name_List :=
                      Declarations.Names (Component);
            E         : Asis.Element := Elem;
            CND       : Asis.Element;
            Type_Name : Unbounded_String;

         begin
            if Elements.Expression_Kind (E) = A_Selected_Component then
               E := Expressions.Selector (E);
            end if;

            CND := Expressions.Corresponding_Name_Declaration (E);

            E := Declarations.Type_Declaration_View (CND);

            if Flat_Element_Kind (E) = An_Unconstrained_Array_Definition then
               declare
                  Type_Suffix : Unbounded_String;
               begin
                  U_Array_Index := U_Array_Index + 1;

                  --  Set array's component type information

                  E := Definitions.Array_Component_Definition (E);

                  Deferred_U_Arrays (U_Array_Index).Comp_Type :=
                    To_Unbounded_String (Image (Text.Element_Image (E)));

                  Deferred_U_Arrays (U_Array_Index).NS :=
                    To_Unbounded_String (Name_Space (E));

                  --  Set array's type name

                  Deferred_U_Arrays (U_Array_Index).Name :=
                    To_Unbounded_String
                      (Image
                        (Text.Element_Image
                          (Declarations.Names (CND) (1))));

                  --  Get array's constraint, an array inside a record must be
                  --  constrained.

                  E := Definitions.Subtype_Constraint
                         (Definitions.Component_Subtype_Indication (ODV));

                  declare
                     R : constant Asis.Discrete_Range_List :=
                           Definitions.Discrete_Ranges (E);
                  begin
                     if R'Length /= 1 then
                        Raise_Spec_Error
                          (E,
                           Message => "Arrays with multiple dimensions not "
                             & "supported.");
                     end if;

                     --  Add array's name suffix

                     Array_Type_Suffix
                       (R (1), Type_Suffix,
                        Deferred_U_Arrays (U_Array_Index).Length);

                     Append (Deferred_U_Arrays (U_Array_Index).Name,
                             Type_Suffix);
                  end;

                  Type_Name := Deferred_U_Arrays (U_Array_Index).Name;
               end;
            end if;

            declare
               T : constant Flat_Element_Kinds :=
                     Flat_Element_Kind
                       (Declarations.Type_Declaration_View
                          (Declarations.Corresponding_First_Subtype (CND)));
            begin
               if Flat_Element_Kind (CND) = A_Subtype_Declaration
                 and then T /= A_Signed_Integer_Type_Definition
                 and then T /= A_Floating_Point_Definition
               then
                  --  This is a subtype field
                  Type_Name := To_Unbounded_String
                    (Image (Text.Element_Image (ODV)));
               end if;
            end;

            if Type_Name = Null_Unbounded_String then
               --  If type name not set, then compute it now
               Type_Name := Type_Def (Elem, Base => False).Name;
            end if;

            for K in Names'Range loop
               Generator.New_Component
                 (NS        => Name_Space
                    (Declarations.Type_Declaration_View (CND)),
                  Comp_Name => Image (Text.Element_Image (Names (K))),
                  Comp_Type => To_String (Type_Name));
            end loop;
         end Analyse_Field;

         -----------------
         -- Is_Standard --
         -----------------

         function Is_Standard (T : Asis.Declaration) return Boolean is
         begin
            return Characters.Handling.To_Lower
              (Image
                 (Compilation_Units.Unit_Full_Name
                    (Elements.Enclosing_Compilation_Unit (T)))) = "standard";
         end Is_Standard;

         Name      : constant String :=
                        Image
                          (Declarations.Defining_Name_Image
                             (Declarations.Names (Elem) (1)));
         E         : Asis.Definition :=
                       Declarations.Type_Declaration_View (Elem);
         Type_Kind : constant Flat_Element_Kinds := Flat_Element_Kind (E);

      begin
         case Type_Kind is
            when A_Record_Type_Definition =>

               if not Generator.Type_Exists (Name_Space (E), Name) then
                  Generator.Start_Record (Name_Space (E), Name);

                  E := Definitions.Record_Definition (E);

                  declare
                     R : constant Asis.Record_Component_List :=
                           Definitions.Record_Components (E);
                  begin
                     for K in R'Range loop
                        Analyse_Field (R (K));
                     end loop;
                  end;

                  --  Create now all deferred arrays

                  for K in 1 .. U_Array_Index loop
                     Generator.Start_Array
                       (To_String (Deferred_U_Arrays (K).NS),
                        To_String (Deferred_U_Arrays (K).Name),
                        To_String (Deferred_U_Arrays (K).Comp_Type),
                        Deferred_U_Arrays (K).Length);
                  end loop;
               end if;

            when A_Constrained_Array_Definition =>

               declare
                  S : constant Asis.Definition_List :=
                        Definitions.Discrete_Subtype_Definitions (E);
                  Array_Len   : Natural;
                  Type_Suffix : Unbounded_String;
                  C           : Asis.Element;
               begin
                  if S'Length /= 1 then
                     Raise_Spec_Error
                       (E,
                        Message => "Arrays with multiple dimensions not "
                          & "supported.");
                  end if;

                  if Flat_Element_Kind (S (1))
                    = A_Discrete_Simple_Expression_Range_As_Subtype_Definition
                  then
                     --  This is the constraint (a .. b)
                     C := S (1);
                  else
                     --  This is a subtype definition as constraint
                     --  (Positive range a .. b)
                     C := Definitions.Subtype_Constraint (S (1));
                  end if;

                  Array_Type_Suffix (C, Type_Suffix, Array_Len);

                  E := Definitions.Array_Component_Definition (E);

                  if not Generator.Type_Exists (Name_Space (E), Name) then
                     Generator.Start_Array
                       (Name_Space (E), Name,
                        Image (Text.Element_Image (E)),
                        Array_Len);

                     Analyse_Array_Component (E);
                  end if;
               end;

            when An_Unconstrained_Array_Definition =>

               E := Definitions.Array_Component_Definition (E);

               if not Generator.Type_Exists (Name_Space (E), Name) then
                  Generator.Start_Array
                    (Name_Space (E), Name,
                     Image (Text.Element_Image (E)));

                  Analyse_Array_Component (E);
               end if;

            when A_Derived_Type_Definition =>

               declare
                  TDV  : constant Asis.Element :=
                           Declarations.Type_Declaration_View
                             (Definitions.Corresponding_Root_Type (E));
                  PST  : Asis.Element;
                  C    : Asis.Constraint;
                  Comp : Asis.Element;
               begin
                  if Flat_Element_Kind (TDV)
                    = An_Unconstrained_Array_Definition
                    or else Flat_Element_Kind (TDV)
                    = A_Constrained_Array_Definition
                  then
                     --  This is derived type from an array, we have to create
                     --  a new array definition if this derived type as a set
                     --  of constraint.

                     PST := Definitions.Parent_Subtype_Indication (E);

                     C    := Definitions.Subtype_Constraint (PST);
                     Comp := Definitions.Array_Component_Definition (TDV);

                     --  Check whether the base name is a standard Ada string

                     declare
                        B_Name : constant String :=
                                   Characters.Handling.To_Lower
                                     (Image (Text.Element_Image
                                       (Definitions.Subtype_Mark (PST))));
                        Len    : Unbounded_String;
                     begin
                        if B_Name = "string" then

                           if Flat_Element_Kind (C) = An_Index_Constraint then
                              declare
                                 R       : constant Asis.Discrete_Range_List :=
                                               Definitions.Discrete_Ranges (C);
                                 Dummy   : Unbounded_String;
                                 Str_Len : Natural;
                              begin
                                 Array_Type_Suffix (R (1), Dummy, Str_Len);
                                 Len := To_Unbounded_String
                                   (AWS.Utils.Image (Str_Len));
                              end;
                           end if;

                           if not Generator.Type_Exists
                             (Name_Space (E), Name)
                           then
                              Generator.Register_Derived
                                (Name_Space (E), Name,
                                 (To_Unbounded_String
                                      (SOAP.Name_Space.Value
                                         (SOAP.Name_Space.XSD)),
                                  To_Unbounded_String ("string"),
                                  Null_Unbounded_String,
                                  Null_Unbounded_String,
                                  Len));
                           end if;

                           return;
                        end if;
                     end;

                     if Flat_Element_Kind (C) = An_Index_Constraint then
                        --  This derived type has constraints
                        declare
                           R           : constant Asis.Discrete_Range_List :=
                                           Definitions.Discrete_Ranges (C);
                           Type_Suffix : Unbounded_String;
                           Array_Len   : Natural;
                        begin
                           if R'Length /= 1 then
                              Raise_Spec_Error
                                (C,
                                 Message => "Arrays with multiple dimensions"
                                   & " not supported.");
                           end if;

                           Array_Type_Suffix (R (1), Type_Suffix, Array_Len);

                           if not Generator.Type_Exists
                             (Name_Space (Comp), Name)
                           then
                              Generator.Start_Array
                                (Name_Space (Comp), Name,
                                 Image (Text.Element_Image (Comp)),
                                 Array_Len);

                              Analyse_Array_Component (Comp);
                           end if;
                           return;
                        end;
                     end if;
                  end if;
               end;

               if not Generator.Type_Exists (Name_Space (E), Name) then
                  Generator.Register_Derived
                    (Name_Space (E), Name,
                     Type_Def (Elem, Base => False));
               end if;

            when A_Subtype_Indication =>

               declare
                  CFS  : constant Asis.Declaration :=
                           Declarations.Corresponding_First_Subtype (Elem);
                  TDV  : constant Asis.Definition :=
                           Declarations.Type_Declaration_View (CFS);
                  C    : Asis.Constraint;
                  Comp : Asis.Element;
               begin
                  if Flat_Element_Kind (TDV)
                    = An_Unconstrained_Array_Definition
                  then
                     --  This is a subtype of an unconstraint array, create
                     --  the corresponding array definition.

                     C    := Definitions.Subtype_Constraint (E);
                     Comp := Definitions.Array_Component_Definition (TDV);

                     declare
                        R           : constant Asis.Discrete_Range_List :=
                                        Definitions.Discrete_Ranges (C);
                        Type_Suffix : Unbounded_String;
                        Array_Len   : Natural;
                     begin
                        if R'Length /= 1 then
                           Raise_Spec_Error
                             (C,
                              Message => "Arrays with multiple dimensions"
                                & " not supported.");
                        end if;

                        Array_Type_Suffix (R (1), Type_Suffix, Array_Len);

                        if not Generator.Type_Exists
                          (Name_Space (Comp), Name)
                        then
                           Generator.Start_Array
                             (Name_Space (Comp), Name,
                              Image (Text.Element_Image (Comp)),
                              Array_Len);
                           Analyse_Array_Component (Comp);
                        end if;
                     end;

                  else
                     if not Generator.Type_Exists (Name_Space (E), Name) then
                        Generator.Register_Type
                          (Name_Space (E), Name,
                           Type_Def (Elem, Base => True));
                     end if;
                  end if;
               end;

            when An_Enumeration_Type_Definition =>

               if not Options.Enum_To_String then
                  if not Generator.Type_Exists (Name_Space (E), Name) then
                     Generator.Start_Enumeration (Name_Space (E), Name);

                     declare
                        D : constant Asis.Declaration_List :=
                              Definitions.Enumeration_Literal_Declarations (E);
                     begin
                        for K in D'Range loop
                           Generator.New_Literal
                             (Image
                                (Text.Element_Image
                                   (Declarations.Names (D (K)) (1))));
                        end loop;
                     end;
                  end if;
               end if;

            when A_Signed_Integer_Type_Definition
               | A_Modular_Type_Definition
               | A_Floating_Point_Definition
                 =>

               if not Is_Standard (Elem)
                 and then not Generator.Type_Exists (Name_Space (E), Name)
               then
                  --  We do not want to generate types declared in standard
                  Def := Type_Def (Elem, Base => True);
                  Generator.Register_Type (Name_Space (E), Name, Def);
               end if;

            when others =>
               --  A type definition not handled by this version
               null;
         end case;
      end Analyse_Type;

      -----------------------
      -- Array_Type_Suffix --
      -----------------------

      procedure Array_Type_Suffix
        (E           : Asis.Element;
         Type_Suffix : out Unbounded_String;
         Length      : out Positive)
      is
         use Extensions.Flat_Kinds;

         Low : constant Asis.Expression := Definitions.Lower_Bound (E);
         Up  : constant Asis.Expression := Definitions.Upper_Bound (E);
      begin
         declare
            Low_Str : constant String := Image (Text.Element_Image (Low));
            Up_Str  : constant String := Image (Text.Element_Image (Up));
         begin
            Length := Integer'Value (Up_Str) - Integer'Value (Low_Str) + 1;
            Type_Suffix := To_Unbounded_String ("_" & Low_Str & "_" & Up_Str);
         exception
            when others =>
               Raise_Spec_Error
                 (E,
                  Message => "Only arrays with numeric indexes supported.");
         end;
      end Array_Type_Suffix;

      ----------------
      -- Name_Space --
      ----------------

      function Name_Space (E : Asis.Element) return String is
         NS : String :=
                Image (Compilation_Units.Unit_Full_Name
                       (Elements.Enclosing_Compilation_Unit (E)));
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

      --------------
      -- Type_Def --
      --------------

      function Type_Def
        (Elem : Asis.Element;
         Base : Boolean) return Generator.Type_Data
      is
         use Extensions.Flat_Kinds;

         function Register_Deferred
           (E : Asis.Declaration) return Generator.Type_Data;
         --  Register a deferred type to be generated after first
         --  pass. Returns the name of the type.

         function "+" (Str : String)
           return Unbounded_String renames To_Unbounded_String;

         function Build_Type
           (Name  : String;
            NS    : String := SOAP.Name_Space.Value (SOAP.Name_Space.XSD);
            First : Long_Long_Integer := Long_Long_Integer'Last;
            Last  : Long_Long_Integer := Long_Long_Integer'First)
            return Generator.Type_Data
         is (+NS, +Name,
             (if First = Long_Long_Integer'Last
              then Null_Unbounded_String
              else +Long_Long_Integer'Image (First)),
             (if Last = Long_Long_Integer'First
              then Null_Unbounded_String
              else +Long_Long_Integer'Image (Last)),
             Null_Unbounded_String);

         function Build_Type_F
           (Name  : String;
            First : Long_Float := Long_Float'Last;
            Last  : Long_Float := Long_Float'First)
            return Generator.Type_Data
         is (+SOAP.Name_Space.Value (SOAP.Name_Space.XSD),
             +Name,
             (if First = Long_Float'Last
              then Null_Unbounded_String
              else +Long_Float'Image (First)),
             (if Last = Long_Float'First
              then Null_Unbounded_String
              else +Long_Float'Image (Last)),
             Null_Unbounded_String);

         procedure Get_Range
           (E : Asis.Element; Lower, Upper : out Long_Long_Integer);
         procedure Get_Range
           (E : Asis.Element; Lower, Upper : out Long_Float);
         --  Returns the range constraint for the given type pointed to by E

         function Compute_Value
           (V : Asis.Expression) return Long_Long_Integer;
         function Compute_Value
           (V : Asis.Expression) return Long_Float;
         function Compute_Value
           (V : Asis.Expression) return SOAP.Types.Unsigned_Long;
         --  Retruns the computed value for the given expression. This is
         --  supposed to be a simple expression for a range declaration:
         --  range -2**5 .. 2**7 or mod 2**15;

         function Is_Calendar (T : Asis.Declaration) return Boolean;
         --  Returns true if the declaration is inside the Ada.Calendar package

         -------------------
         -- Compute_Value --
         -------------------

         function Compute_Value
           (V : Asis.Expression) return Long_Float
         is
            VI  : constant String := Image (Text.Element_Image (V));
            M   : constant Natural := Strings.Fixed.Index (VI, "-");
            E   : constant Natural := Strings.Fixed.Index (VI, "**");
            N   : Long_Float;
            Exp : Natural := 1;

         begin
            if E = 0 and then M /= 0 then
               --  We have a simple minus before a number
               return -Long_Float'Value (VI (M + 1 .. VI'Last));

            elsif M < E then
               Exp := Natural'Value (VI (E + 2 .. VI'Last));

               if M = 0 then
                  N := Long_Float'Value (VI (VI'First .. E - 1));
               else
                  N := -Long_Float'Value (VI (M + 1 .. E - 1));
               end if;

               return N ** Exp;

            else
               --  expressions not supported
               --  ???
               return 0.0;
            end if;
         end Compute_Value;

         function Compute_Value
           (V : Asis.Expression) return Long_Long_Integer
         is
            VI  : constant String := Image (Text.Element_Image (V));
            M   : constant Natural := Strings.Fixed.Index (VI, "-");
            E   : constant Natural := Strings.Fixed.Index (VI, "**");
            N   : Long_Long_Integer;
            Exp : Natural := 1;

         begin
            if E = 0 and then M /= 0 then
               --  We have a simple minus before a number
               return -Long_Long_Integer'Value (VI (M + 1 .. VI'Last));

            elsif M < E then
               Exp := Natural'Value (VI (E + 2 .. VI'Last));

               if M = 0 then
                  N := Long_Long_Integer'Value (VI (VI'First .. E - 1));
               else
                  N := -Long_Long_Integer'Value (VI (M + 1 .. E - 1));
               end if;

               return N ** Exp;

            else
               --  expressions not supported
               --  ???
               return 0;
            end if;
         end Compute_Value;

         function Compute_Value
           (V : Asis.Expression) return SOAP.Types.Unsigned_Long
         is
            use type SOAP.Types.Unsigned_Long;
            VI     : constant String := Image (Text.Element_Image (V));
            E      : constant Natural := Strings.Fixed.Index (VI, "**");
            N      : SOAP.Types.Unsigned_Long;
            N1, N2 : SOAP.Types.Unsigned_Long;
         begin
            N1 := SOAP.Types.Unsigned_Long'Value (VI (VI'First .. E - 1));
            N2 := SOAP.Types.Unsigned_Long'Value (VI (E + 2 .. VI'Last));
            N := N1;
            for K in 1 .. N2 - 1 loop
               N := N * N1;
            end loop;

            return N;
         end Compute_Value;

         ---------------
         -- Get_Range --
         ---------------

         procedure Get_Range
           (E : Asis.Element; Lower, Upper : out Long_Long_Integer)
         is
            C      : Asis.Element;
            LB, UB : Asis.Expression;

         begin
            Lower := Long_Long_Integer'Last;
            Upper := Long_Long_Integer'First;

            case Flat_Element_Kind (E) is
               when A_Signed_Integer_Type_Definition =>
                  C := Definitions.Integer_Constraint (E);

               when An_Ordinary_Type_Declaration =>
                  C := Definitions.Subtype_Constraint
                    (Definitions.Parent_Subtype_Indication
                       (Declarations.Type_Declaration_View (E)));

               when A_Derived_Type_Definition =>
                  C := Definitions.Subtype_Constraint (E);

               when others =>
                  C := E;
            end case;

            if Flat_Element_Kind (C) = A_Simple_Expression_Range then
               LB := Definitions.Lower_Bound (C);
               UB := Definitions.Upper_Bound (C);

               if Flat_Element_Kind (LB) = A_Function_Call then
                  Lower := Compute_Value (LB);

               elsif Flat_Element_Kind (LB) = A_First_Attribute then
                  Lower := Long_Long_Integer'Last;
               else
                  Lower := Long_Long_Integer'Value
                    (Image (Expressions.Value_Image (LB)));
               end if;

               if Flat_Element_Kind (UB) = A_Function_Call then
                  Upper := Compute_Value (UB);
               elsif Flat_Element_Kind (UB) = A_Last_Attribute then
                  Upper := Long_Long_Integer'First;
               else
                  Upper := Long_Long_Integer'Value
                    (Image (Expressions.Value_Image (UB)));
               end if;
            end if;
         end Get_Range;

         procedure Get_Range
           (E : Asis.Element; Lower, Upper : out Long_Float)
         is
            C      : Asis.Element;
            LB, UB : Asis.Expression;

         begin
            Lower := Long_Float'Last;
            Upper := Long_Float'First;

            case Flat_Element_Kind (E) is
               when A_Floating_Point_Definition =>
                  C := Definitions.Real_Range_Constraint (E);

               when An_Ordinary_Type_Declaration =>
                  C := Definitions.Subtype_Constraint
                    (Definitions.Parent_Subtype_Indication
                       (Declarations.Type_Declaration_View (E)));

               when A_Derived_Type_Definition =>
                  C := Definitions.Subtype_Constraint (E);

               when others =>
                  C := E;
            end case;

            if Flat_Element_Kind (C) = A_Simple_Expression_Range then
               LB := Definitions.Lower_Bound (C);
               UB := Definitions.Upper_Bound (C);

               if Flat_Element_Kind (LB) = A_Function_Call then
                  Lower := Compute_Value (LB);
               elsif Flat_Element_Kind (LB) = A_First_Attribute then
                  Lower := Long_Float'Last;
               else
                  Lower := Long_Float'Value
                    (Image (Expressions.Value_Image (LB)));
               end if;

               if Flat_Element_Kind (UB) = A_Function_Call then
                  Upper := Compute_Value (UB);
               elsif Flat_Element_Kind (UB) = A_Last_Attribute then
                  Upper := Long_Float'First;
               else
                  Upper := Long_Float'Value
                    (Image (Expressions.Value_Image (UB)));
               end if;
            end if;
         end Get_Range;

         -----------------
         -- Is_Calendar --
         -----------------

         function Is_Calendar (T : Asis.Declaration) return Boolean is
         begin
            return Characters.Handling.To_Lower
              (Image
                 (Compilation_Units.Unit_Full_Name
                      (Elements.Enclosing_Compilation_Unit (T))))
                  = "ada.calendar";
         end Is_Calendar;

         -----------------------
         -- Register_Deferred --
         -----------------------

         function Register_Deferred
           (E : Asis.Declaration) return Generator.Type_Data
         is
            Name : constant String :=
                     Image
                       (Declarations.Defining_Name_Image
                          (Declarations.Names (E) (1)));
         begin
            Index := Index + 1;
            Deferred_Types (Index) := E;
            return Build_Type (Name, Name_Space (Declarations.Names (E) (1)));
         end Register_Deferred;

         E   : Asis.Element := Elem;
         CFS : Asis.Declaration;
         CND : Asis.Declaration;
         Tn  : Asis.Element := Elem;

      begin
         if Elements.Expression_Kind (E) = A_Selected_Component then
            E := Expressions.Selector (E);
         end if;

         if Flat_Element_Kind (E) = An_Ordinary_Type_Declaration
           and then Flat_Element_Kind (Declarations.Type_Declaration_View (E))
            = A_Derived_Type_Definition
         then
            E := Declarations.Type_Declaration_View (E);
         end if;

         if Flat_Element_Kind (E) = A_Derived_Type_Definition then
            E := Definitions.Subtype_Mark
                   (Definitions.Parent_Subtype_Indication (E));
            Tn := E;
         end if;

         if Elements.Expression_Kind (E) = A_Selected_Component then
            E := Expressions.Selector (E);
         end if;

         if Flat_Element_Kind (E) /= An_Ordinary_Type_Declaration
           and then Flat_Element_Kind (E) /= A_Subtype_Declaration
           and then Flat_Element_Kind (E) /= A_Derived_Type_Definition
           and then Flat_Element_Kind (E) /= A_Selected_Component
         then
            CND := Expressions.Corresponding_Name_Declaration (E);

         else
            CND := Elem;
         end if;

         CFS := Declarations.Corresponding_First_Subtype (CND);

         --  Get type view

         E := Declarations.Type_Declaration_View (CFS);

         case Flat_Element_Kind (E) is

            when A_Record_Type_Definition =>
               --  This is a record, checks if the record definition has
               --  been parsed.

               return Register_Deferred (CFS);

            when An_Enumeration_Type_Definition =>
               --  Handle special enumerations like Boolean and Character

               if Characters.Handling.To_Lower
                    (Image (Text.Element_Image (Elem))) = "character"
               then
                  return Build_Type ("character");

               elsif Characters.Handling.To_Lower
                       (Image (Text.Element_Image (Elem))) = "boolean"
               then
                  return Build_Type ("boolean");

               else
                  if Options.Enum_To_String then
                     return Build_Type ("string");

                  else
                     return Register_Deferred (CFS);
                  end if;
               end if;

            when A_Floating_Point_Definition =>
               --  This is a floating point type, check the constraint to
               --  return the proper mapping.

               if Base then
                  declare
                     Dig      : constant Integer :=
                                  Integer'Value
                                    (Image (Expressions.Value_Image
                                     (Definitions.Digits_Expression (E))));
                     Ilb, Iub : Long_Float;
                  begin
                     Get_Range (E, Ilb, Iub);

                     if Dig <= Float'Digits then
                        return Build_Type_F ("float", Ilb, Iub);
                     else
                        return Build_Type_F ("long_float", Ilb, Iub);
                     end if;
                  end;

               else
                  declare
                     Ilb, Iub : Long_Float;
                  begin
                     Get_Range (Elem, Ilb, Iub);

                     return Build_Type_F
                       (Image (Text.Element_Image (Tn)), Ilb, Iub);
                  end;
               end if;

            when A_Modular_Type_Definition =>
               --  This is an integer type, check the constraint to return the
               --  proper mapping.

               if Base then
                  declare
                     use type SOAP.Types.Unsigned_Long;
                     Mod_Node : constant Asis.Expression :=
                                  Definitions.Mod_Static_Expression (E);
                     Modulus  : SOAP.Types.Unsigned_Long;
                  begin
                     if Flat_Element_Kind (Mod_Node) = A_Function_Call then
                        Modulus := Compute_Value (Mod_Node);
                     else
                        Modulus := SOAP.Types.Unsigned_Long'Value
                          (Image (Expressions.Value_Image (Mod_Node)));
                     end if;

                     if Modulus = SOAP.Types.Unsigned_Long
                       (SOAP.Types.Unsigned_Byte'Modulus)
                     then
                        return Build_Type ("unsigned_byte");

                     elsif Modulus < SOAP.Types.Unsigned_Long
                       (SOAP.Types.Unsigned_Byte'Modulus)
                     then
                        return Build_Type
                          ("unsigned_byte",
                           Last => Long_Long_Integer (Modulus - 1));

                     elsif Modulus = SOAP.Types.Unsigned_Long
                       (SOAP.Types.Unsigned_Short'Modulus)
                     then
                        return Build_Type ("unsigned_short");

                     elsif Modulus < SOAP.Types.Unsigned_Long
                       (SOAP.Types.Unsigned_Short'Modulus)
                     then
                        return Build_Type
                          ("unsigned_short",
                           Last => Long_Long_Integer (Modulus - 1));

                     elsif Modulus = SOAP.Types.Unsigned_Long
                       (SOAP.Types.Unsigned_Int'Modulus)
                     then
                        return Build_Type ("unsigned_int");

                     elsif Modulus < SOAP.Types.Unsigned_Long
                       (SOAP.Types.Unsigned_Int'Modulus)
                     then
                        return Build_Type
                          ("unsigned_int",
                           Last => Long_Long_Integer (Modulus - 1));

                     else
                        return Build_Type
                          ("unsigned_long",
                           Last => Long_Long_Integer (Modulus - 1));
                     end if;
                  end;

               else
                  return Build_Type (Image (Text.Element_Image (Tn)));
               end if;

            when A_Signed_Integer_Type_Definition =>
               --  This is an integer type, check the constraint to return the
               --  proper mapping.

               if Base then
                  declare
                     Ilb, Iub : Long_Long_Integer;
                  begin
                     Get_Range (E, Ilb, Iub);

                     if Ilb = Long_Long_Integer (SOAP.Types.Byte'First)
                       and then Iub = Long_Long_Integer (SOAP.Types.Byte'Last)
                     then
                        return Build_Type ("byte");

                     elsif Ilb >= Long_Long_Integer (SOAP.Types.Byte'First)
                       and then Iub <= Long_Long_Integer (SOAP.Types.Byte'Last)
                     then
                        return Build_Type ("byte", First => Ilb, Last => Iub);

                     elsif Ilb = Long_Long_Integer (SOAP.Types.Short'First)
                       and then
                         Iub = Long_Long_Integer (SOAP.Types.Short'Last)
                     then
                        return Build_Type ("short");

                     elsif Ilb >= Long_Long_Integer (SOAP.Types.Short'First)
                       and then
                         Iub <= Long_Long_Integer (SOAP.Types.Short'Last)
                     then
                        return Build_Type ("short", First => Ilb, Last => Iub);

                     elsif Ilb = Long_Long_Integer (Integer'First)
                       and then Iub = Long_Long_Integer (Integer'Last)
                     then
                        return Build_Type ("integer");

                     elsif Ilb >= Long_Long_Integer (Integer'First)
                       and then Iub <= Long_Long_Integer (Integer'Last)
                     then
                        return Build_Type
                          ("integer", First => Ilb, Last => Iub);

                     elsif Ilb = Long_Long_Integer (Long_Integer'First)
                       and then Iub = Long_Long_Integer (Long_Integer'Last)
                     then
                        return Build_Type ("long");

                     else
                        return Build_Type ("long", First => Ilb, Last => Iub);
                     end if;
                  end;

               else
                  declare
                     Ilb, Iub : Long_Long_Integer;
                  begin
                     Get_Range (Elem, Ilb, Iub);

                     return Build_Type
                       (Image (Text.Element_Image (Tn)),
                        First => Ilb, Last => Iub);
                  end;
               end if;

            when A_Derived_Type_Definition =>
               --  This is a derived type definition, analyse the name after
               --  the "is new".
               --  Record the type to generate the corresponding schema.

               return Register_Deferred (CFS);

            when A_Constrained_Array_Definition
              | An_Unconstrained_Array_Definition
              =>
               --  If this array definition is the standard Ada String,
               --  returns it, no need to analyse this further.

               declare
                  T_Name : constant String :=
                             Characters.Handling.To_Lower
                               (Image (Text.Element_Image (Elem)));
               begin
                  --  Check for specific array name like String and Base64

                  if  T_Name = "string" then
                     return Build_Type ("string");

                  elsif T_Name = "soap_base64"
                    or else T_Name = "utils.soap_base64"
                    or else T_Name = "soap.utils.soap_base64"
                  then
                     --  This is the SOAP Base64 runtime type support
                     return Build_Type ("SOAP_Base64");
                  else
                     return Register_Deferred (CFS);
                  end if;
               end;

            when A_Private_Type_Definition =>
               E := Declarations.Names (CFS) (1);

               declare
                  Name : constant String :=
                           Characters.Handling.To_Lower
                             (Image (Declarations.Defining_Name_Image (E)));
               begin
                  if Name = "unbounded_string" then
                     if Base then
                        return Build_Type ("string");
                     else
                        return Build_Type ("unbounded_string");
                     end if;

                  elsif Name = "time" and then Is_Calendar (E) then
                     return Build_Type ("time");

                  else
                     Raise_Spec_Error
                       (E, Message => "unsupported private type " & Name);
                  end if;
               end;

            when A_Derived_Record_Extension_Definition =>
               --  This can be a safe pointer object

               declare
                  Name : constant String :=
                           Characters.Handling.To_Lower
                             (Image
                                (Declarations.Defining_Name_Image
                                   (Declarations.Names (CFS) (1))));
               begin
                  if Name = "safe_pointer" then
                     --  We have a Safe_Pointer definition here, let's get the
                     --  corresponding type.

                     --  Get the record definition

                     E := Definitions.Record_Definition (E);

                     --  Get first record element type

                     E := Definitions.Subtype_Mark
                       (Definitions.Component_Subtype_Indication
                          (Declarations.Object_Declaration_View
                             (Definitions.Record_Components (E) (1))));

                     --  Get the corresponding type

                     E := Declarations.Type_Declaration_View
                       (Declarations.Corresponding_First_Subtype
                          (Expressions.Corresponding_Name_Declaration (E)));

                     E := Definitions.Access_To_Object_Definition (E);

                     E := Definitions.Subtype_Mark (E);

                     E := Declarations.Corresponding_First_Subtype
                       (Expressions.Corresponding_Name_Declaration (E));

                     return Register_Deferred (E);

                  else
                     Raise_Spec_Error
                       (E, Message => "unsupported record extension " & Name);
                  end if;
               end;

            when others =>
               E := Declarations.Names (CFS) (1);
               Raise_Spec_Error
                 (E,
                  Message => "unsupported element kind "
                             & Image (Declarations.Defining_Name_Image (E)));
         end case;
      end Type_Def;

   begin
      Analyse_Node (Body_Structure'Access);

      --  Analyse deferred types

      declare
         Prev_Index : Natural;
         First      : Positive := 1;
      begin
         --  When analysing the deferred types we could have some more types
         --  discoverred. Do the analyse of the defintions until there is no
         --  more added into the deferred list.

         loop
            Prev_Index := Index;

            for K in First .. Index loop
               Analyse_Type (Deferred_Types (K));
            end loop;

            exit when Prev_Index = Index;

            First := Prev_Index;
         end loop;
      end;
   end Analyse_Structure;

   --------------
   -- Clean_Up --
   --------------

   procedure Clean_Up is
   begin
      --  Deleting the tree file itself

      Directories.Delete_File (Tree_Name.all);

      --  And the corresponding ALI file

      Directories.Delete_File
        (Tree_Name (Tree_Name'First .. Tree_Name'Last - 3) & "ali");
   exception
      when others =>
         null;
   end Clean_Up;

   -------------------------
   -- Create_Element_Node --
   -------------------------

   procedure Create_Element_Node
     (Element : Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Body_State)
   is
      use Extensions.Flat_Kinds;

      Arg_Kind     : constant Flat_Element_Kinds :=
                       Flat_Element_Kind (Element);
      Current_Node : Link;

      procedure Insert_In_List
        (State    : in out Body_State;
         El       : Asis.Element;
         New_Node :     out Link);
      --  Inserts an argument Element in the current list, keeping the
      --  alphabetic ordering. Creates a new sublist if needed.
      --  New_Node returns the reference to the newly inserted node

      --------------------
      -- Insert_In_List --
      --------------------

      procedure Insert_In_List
        (State    : in out Body_State;
         El       : Asis.Element;
         New_Node : out Link) is
      begin
         New_Node      := new Element_Node;
         New_Node.Spec := El;

         New_Node.Spec_Name := new String'(Name (El));

         if State.New_List_Needed then
            --  Here we have to set up a new sub-list:
            State.Current_List    := New_Node;
            New_Node.Up           := State.Last_Top;
            State.Last_Top.Down   := New_Node;
            State.New_List_Needed := False;

            New_Node.Last := New_Node;
            --  We've just created a new list. It contains a single element
            --  which is its last Element, so we are setting the link to the
            --  last element to the Prev field of the list head element

         else
            --  Here we have to insert New_Node in an existing list,
            --  keeping the alphabetical order of program unit names

            New_Node.Up := State.Current_List.Up;

            if Arg_Kind = An_Incomplete_Type_Declaration then
               --  No need for alphabetical ordering, inserting in the
               --  very beginning:

               New_Node.Last := State.Current_List.Last;
               --  New_Node will be the head element of the list, so we have
               --  to copy into this new head element the reference to the
               --  last element of the list.

               New_Node.Next           := State.Current_List;
               State.Current_List.Prev := New_Node;
               State.Current_List      := New_Node;
            else

               New_Node.Prev                := State.Current_List.Last;
               State.Current_List.Last.Next := New_Node;
               State.Current_List.Last      := New_Node;
            end if;

         end if;
      end Insert_In_List;

      --  Start of the processing of Create_Element_Node

   begin
      if State.Argument_Spec then
         Body_Structure.Spec      := Element;
         State.Argument_Spec      := False;
         Body_Structure.Spec_Name := new String'(Name (Element));
         Current_Node             := Body_Structure'Access;

      elsif Arg_Kind = A_Defining_Identifier then
         --  Skipping a defining name of a spec which may contain local
         --  specs requiring bodies
         null;

      elsif Arg_Kind = A_Protected_Definition then
         --  We just have to go one level down to process protected items
         null;

      elsif not (Arg_Kind = A_Procedure_Declaration
                   or else Arg_Kind = A_Function_Declaration
                   or else Arg_Kind = A_Subtype_Declaration
                   or else Arg_Kind = An_Ordinary_Type_Declaration
                   or else Arg_Kind = A_Package_Declaration)
      then
         --  Do nothing if this is not a procedure or function
         null;

      else
         Insert_In_List (State, Element, Current_Node);
      end if;

      if Arg_Kind = A_Package_Declaration
        or else Arg_Kind = A_Generic_Package_Declaration
        or else Arg_Kind = A_Single_Protected_Declaration
        or else Arg_Kind = A_Protected_Type_Declaration
      then
         --  Here we may have specs requiring bodies inside a construct
         State.New_List_Needed := True;
         State.Last_Top        := Current_Node;

      elsif Arg_Kind = A_Protected_Definition then
         --  We have to skip this syntax level
         null;

      elsif Arg_Kind = A_Package_Instantiation then
         --  We want to keep track of all package instantiations to analyse
         --  those that are used to create a safe pointer for array in
         --  records.
         Insert_In_List (State, Element, Current_Node);

      else
         --  No need to go deeper
         Control := Abandon_Children;
      end if;
   end Create_Element_Node;

   -----------------
   -- Create_Tree --
   -----------------

   procedure Create_Tree is

      File_Name : String_Access;
      Success   : Boolean := False;

      function Get_Tree_Name return String;
      --  Returns the name of the tree file

      ----------
      -- Free --
      ----------

      procedure Unchecked_Free is
        new Ada.Unchecked_Deallocation (String, String_Access);

      -------------------
      -- Get_Tree_Name --
      -------------------

      function Get_Tree_Name return String is
         F_Name    : constant String :=
                       Directories.Base_Name (To_String (Options.File_Name));
         Dot_Index : Natural;
         Last      : Natural;

      begin
         Dot_Index := Strings.Fixed.Index (F_Name, ".", Strings.Backward);

         if Dot_Index = 0 then
            Last := F_Name'Last;
         else
            Last := Dot_Index - 1;
         end if;

         return To_String (Options.Tree_File_Path)
           & F_Name (F_Name'First .. Last) & ".adt";
      end Get_Tree_Name;

   begin
      File_Name := new String'(To_String (Options.File_Name));

      Compile
        (File_Name, Arg_List (Arg_List'First .. Arg_Index), Success,
         GCC          => GPRBUILD,
         Use_GPRBUILD => True);

      if not Success then
         Text_IO.Put_Line
           (Text_IO.Standard_Error,
            "ada2wsdl: cannot create the tree file for " & File_Name.all);
         raise Parameter_Error;
      end if;

      Tree_Name := new String'(Get_Tree_Name);

      if not Directories.Exists (Tree_Name.all) then
         Text_IO.Put_Line
           (Text_IO.Standard_Error,
            "ada2wsdl: after compilation, cannot find tree file "
            & Tree_Name.all);
         Text_IO.Put_Line
           (Text_IO.Standard_Error, "ada2wsdl: consider using -t option");
         raise Parameter_Error;
      end if;

      Unchecked_Free (File_Name);
   end Create_Tree;

   ------------------------
   -- Emergency_Clean_Up --
   ------------------------

   procedure Emergency_Clean_Up is
   begin
      if Ada_Environments.Is_Open (My_Context) then
         Ada_Environments.Close (My_Context);
      end if;

      Ada_Environments.Dissociate (My_Context);

      Implementation.Finalize;

      if Text_IO.Is_Open (Spec_File) then
         --  No need to keep a broken body in case of an emergency clean up
         Text_IO.Close (Spec_File);
      end if;
   end Emergency_Clean_Up;

   -----------
   -- Go_Up --
   -----------

   procedure Go_Up
     (Element : Asis.Element;
      Control : in out Traverse_Control;
      State   : in out Body_State)
   is
      pragma Unreferenced (Control);

      use Extensions.Flat_Kinds;

      Arg_Kind : constant Flat_Element_Kinds := Flat_Element_Kind (Element);
   begin
      if not (Arg_Kind = A_Package_Declaration
              or else Arg_Kind = A_Generic_Package_Declaration
              or else Arg_Kind = A_Single_Protected_Declaration
              or else Arg_Kind = A_Protected_Type_Declaration)
      then
         return;
      end if;

      if State.New_List_Needed then
         --  No local body is needed for a given construct
         State.New_List_Needed := False;

      else
         --  We have to reset the current list:

         if State.Current_List /= null then
            State.Current_List := State.Current_List.Up;

            while State.Current_List.Prev /= null loop
               State.Current_List := State.Current_List.Prev;
            end loop;
         end if;
      end if;
   end Go_Up;

   -----------
   -- Image --
   -----------

   function Image (Str : Wide_String) return String is
      use AWS;
   begin
      return Strings.Fixed.Trim
        (Characters.Conversions.To_String (Str),
         Left  => Utils.Spaces,
         Right => Utils.Spaces);
   end Image;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Create_Tree;
      Options.Initialized := True;
   exception
      when others =>
         Options.Initialized := False;
         raise;
   end Initialize;

   ----------
   -- Name --
   ----------

   function Name (Elem : Asis.Element) return String is
      Def_Name : constant Asis.Element := Declarations.Names (Elem) (1);
   begin
      return Characters.Conversions.To_String
        (Declarations.Defining_Name_Image (Def_Name));
   end Name;

   -----------
   -- Start --
   -----------

   procedure Start is

      use Text_IO;

      CU         : Asis.Compilation_Unit;
      My_Control : Traverse_Control := Continue;
      My_State   : Body_State;

   begin
      Asis.Implementation.Initialize;

      Ada_Environments.Associate
        (My_Context,
        "My_Context",
        "-C1 " & Characters.Conversions.To_Wide_String (Tree_Name.all));

      Ada_Environments.Open (My_Context);

      CU := Extensions.Main_Unit_In_Current_Tree (My_Context);

      if Compilation_Units.Is_Nil (CU) then
         Put_Line
           (Standard_Error,
            "Nothing to be done for " & To_String (Options.File_Name));
         return;

      else
         --  And here we have to do the job

         Create_Structure
           (Element => Elements.Unit_Declaration (CU),
            Control => My_Control,
            State   => My_State);

         if not Options.Quiet then
            New_Line;
         end if;

         Analyse_Structure;

         if not Options.Quiet then
            New_Line;
            Put_Line
              ("WSDL document " & To_String (Options.WSDL_File_Name)
                 & " is created for " & To_String (Options.File_Name) & '.');
         end if;
      end if;

      Ada_Environments.Close (My_Context);

      Ada_Environments.Dissociate (My_Context);

      Implementation.Finalize;

   exception

      when Ex : Asis.Exceptions.ASIS_Inappropriate_Context
             |  Asis.Exceptions.ASIS_Inappropriate_Container
             |  Asis.Exceptions.ASIS_Inappropriate_Compilation_Unit
             |  Asis.Exceptions.ASIS_Inappropriate_Element
             |  Asis.Exceptions.ASIS_Inappropriate_Line
             |  Asis.Exceptions.ASIS_Inappropriate_Line_Number
             |  Asis.Exceptions.ASIS_Failed
        =>
         New_Line (Standard_Error);

         Put_Line (Standard_Error, "Unexpected bug in Ada2WSDL v" & Version);
         Put      (Standard_Error, Exception_Name (Ex));
         Put_Line (Standard_Error, " raised");
         Put
           (Standard_Error, "ada2wsdl: ASIS Diagnosis is "
              & Characters.Conversions.To_String
                  (Asis.Implementation.Diagnosis));
         New_Line (Standard_Error);
         Put      (Standard_Error, "ada2wsdl: Status Value   is ");
         Put_Line (Standard_Error, Asis.Errors.Error_Kinds'Image
                   (Asis.Implementation.Status));
         Emergency_Clean_Up;
         raise Fatal_Error;

      when others =>
         Emergency_Clean_Up;
         raise;
   end Start;

end Ada2WSDL.Parser;
