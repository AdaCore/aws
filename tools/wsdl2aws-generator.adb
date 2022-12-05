------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2003-2022, AdaCore                     --
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

with Ada.Characters.Handling;
with Ada.Containers.Indefinite_Ordered_Sets;
with Ada.Directories;
with Ada.Environment_Variables;
with Ada.Strings.Fixed;
with Ada.Strings.Maps.Constants;
with Ada.Text_IO;

with AWS.Containers.Key_Value;
with AWS.Utils;

with SOAP.Types;
with SOAP.Utils;
with SOAP.WSDL.Name_Spaces;
with SOAP.WSDL.Schema;

with WSDL2AWS.WSDL.Types;

with wsdl2aws_templates;
pragma Unreferenced (wsdl2aws_templates);

package body WSDL2AWS.Generator is

   use Ada;

   use type Templates.Tag;

   --  All the templates files used to generate the type code

   Template_Enum_Ads      : constant String := "s-type-enum.tads";
   Template_Enum_Adb      : constant String := "s-type-enum.tadb";
   Template_Enum_Types    : constant String := "s-type-enum-types.tads";

   Template_Derived_Ads   : constant String := "s-type-derived.tads";
   Template_Derived_Types : constant String := "s-type-derived-types.tads";

   Template_Array_Ads     : constant String := "s-type-array.tads";
   Template_Array_Types   : constant String := "s-type-array-types.tads";

   Template_Record_Ads    : constant String := "s-type-record.tads";
   Template_Record_Adb    : constant String := "s-type-record.tadb";
   Template_Record_Types  : constant String := "s-type-record-types.tads";

   Template_Types_Ads     : constant String := "s-types.tads";
   Template_Types_Adb     : constant String := "s-types.tadb";

   Template_Stub_Types_Ads : constant String := "s-stub-types.tads";
   Template_Main_Adb       : constant String := "s-main.tadb";
   Template_Root_Ads       : constant String := "s-root.tads";
   Template_NS_Pkg_Ads     : constant String := "s-name-space-pkg.tads";

   procedure Generate
     (O            : Object;
      Filename     : String;
      Template     : String;
      Translations : Templates.Translate_Set);
   --  Render a template with associated translations into Filename

   procedure Generate
     (O            : Object;
      File         : Text_IO.File_Type;
      Template     : String;
      Translations : Templates.Translate_Set);
   --  Render a template with associated translations into File

   procedure Insert_Types_Def
     (O            : in out Object;
      Template     : String;
      Translations : Templates.Translate_Set);
   --  Insert a type chunk into the global types definitions

   procedure Generate_Params
     (O                : Object;
      N                : WSDL.Parameters.P_Set;
      P_Decl           : in out Templates.Tag;
      P_Name           : in out Templates.Tag;
      P_Kind           : in out Templates.Tag;
      P_Min            : in out Templates.Tag;
      P_Max            : in out Templates.Tag;
      P_Compound_Size  : in out Templates.Tag;
      P_Type           : in out Templates.Tag;
      P_Base_Type      : in out Templates.Tag;
      P_Root_Type      : in out Templates.Tag;
      P_Root_Type_Kind : in out Templates.Tag;
      P_Type_Name      : in out Templates.Tag;
      P_Type_Kind      : in out Templates.Tag;
      P_Ada_Type       : in out Templates.Tag;
      P_Q_Name         : in out Templates.Tag;
      P_NS_Name        : in out Templates.Tag;
      P_NS_Value       : in out Templates.Tag;
      P_Elt_NS_Name    : in out Templates.Tag;
      P_Elt_NS_Value   : in out Templates.Tag);
   --  Generate all tag information for the parameters pointed to by N

   function Type_Name
     (O : Object;
      N : WSDL.Parameters.P_Set) return String;
   --  Returns the name of the type for parameter on node N

   package String_Store is
     new Ada.Containers.Indefinite_Ordered_Sets (String);

   function Format_Name (O : Object; Name : String) return String;
   --  Returns Name formated with the Ada style if O.Ada_Style is true and
   --  Name unchanged otherwise.

   procedure Put_Types
     (O          : in out Object;
      Proc       : String;
      SOAPAction : String;
      Input      : WSDL.Parameters.P_Set;
      Output     : WSDL.Parameters.P_Set);
   --  This must be called to create the data types for composite objects

   function Result_Type
     (O      : Object;
      Proc   : String;
      Output : WSDL.Parameters.P_Set) return String;
   --  Returns the result type given the output parameters

   function Is_Simple_Wrapped_Parameter
     (O  : Object;
      P  : WSDL.Parameters.P_Set) return Boolean;
   --  Returns True if P is a record with a least one field and we are in
   --  Document style binding.

   function To_Unit_Name (Filename : String) return String;
   --  Returns the unit name given a filename following the GNAT
   --  naming scheme.

   procedure Output_Schema_Definition
     (O          : in out Object;
      Key, Value : String);
   --  This is just a key/value pair to record schema definitions for the
   --  runtime. The information format is:
   --
   --     proc.param         ->  type_name
   --     record.field       ->  type_name
   --     type_name          ->  type_name | @enum
   --
   --  The special tag @enum above is to be able to differentiate between
   --  xsd:string and enumeration literal.
   --
   --  And some special keys:
   --
   --     @binding.style     ->  [rcp|document]
   --     @<proc>.encoding   ->  [literal/encoded] (encoding for proc name)
   --     @param1[:param_n]  ->  operation         (operation for signature)

   function Is_String (N : WSDL.Parameters.P_Set) return Boolean;
   --  Returns True is N is a string

   S_Gen    : SOAP.WSDL.Schema.Definition;
   --  Keep record of generated schema definitions to avoid dupliace

   NS_Generated : String_Store.Set;
   --  Keep record generated name-space renaming in types package to avoid
   --  duplicate.

   Types_Gen : String_Store.Set;

   --  Stub generator routines

   package Stub is

      procedure Start_Service
        (O                  : in out Object;
         Name               : String;
         Root_Documentation : String;
         Documentation      : String;
         Location           : String);

      procedure End_Service
        (O    : in out Object;
         Name : String);

      procedure New_Procedure
        (O             : in out Object;
         Proc          : String;
         Documentation : String;
         SOAPAction    : String;
         Wrapper_Name  : String;
         Namespace     : SOAP.Name_Space.Object;
         Input         : WSDL.Parameters.P_Set;
         Output        : WSDL.Parameters.P_Set;
         Fault         : WSDL.Parameters.P_Set);

   end Stub;

   --  Skeleton generator routines

   package Skel is

      procedure Start_Service
        (O                  : in out Object;
         Name               : String;
         Root_Documentation : String;
         Documentation      : String;
         Location           : String);

      procedure End_Service
        (O    : in out Object;
         Name : String);

      procedure New_Procedure
        (O             : in out Object;
         Proc          : String;
         Documentation : String;
         SOAPAction    : String;
         Wrapper_Name  : String;
         Namespace     : SOAP.Name_Space.Object;
         Input         : WSDL.Parameters.P_Set;
         Output        : WSDL.Parameters.P_Set;
         Fault         : WSDL.Parameters.P_Set);

   end Skel;

   --  Callback generator routines

   package CB is

      procedure Start_Service
        (O                  : in out Object;
         Name               : String;
         Root_Documentation : String;
         Documentation      : String;
         Location           : String);

      procedure End_Service
        (O    : in out Object;
         Name : String);

      procedure New_Procedure
        (O             : in out Object;
         Proc          : String;
         Documentation : String;
         SOAPAction    : String;
         Wrapper_Name  : String;
         Namespace     : SOAP.Name_Space.Object;
         Input         : WSDL.Parameters.P_Set;
         Output        : WSDL.Parameters.P_Set;
         Fault         : WSDL.Parameters.P_Set);

   end CB;

   --  Simple name set used to keep record of all generated types

   package Name_Set is

      procedure Add (Name : String);
      --  Add new name into the set

      function Exists (Name : String) return Boolean;
      --  Returns true if Name is in the set

   end Name_Set;

   ---------------
   -- Ada_Style --
   ---------------

   procedure Ada_Style (O : in out Object) is
   begin
      O.Ada_Style := True;
   end Ada_Style;

   --------------
   -- Add_TagV --
   --------------

   procedure Add_TagV
     (Set                  : in out Templates.Translate_Set;
      Assoc_Name, Tag_Name : String)
   is
      T : Templates.Tag;
   begin
      if Templates.Exists (Set, Assoc_Name) then
         T := Templates.Get (Templates.Get (Set, Assoc_Name));
      end if;

      T := T & Tag_Name;

      Templates.Insert (Set, Templates.Assoc (Assoc_Name, T));
   end Add_TagV;

   procedure Add_TagV
     (Set        : in out Templates.Translate_Set;
      Assoc_Name : String;
      Value      : Boolean)
   is
      T : Templates.Tag;
   begin
      if Templates.Exists (Set, Assoc_Name) then
         T := Templates.Get (Templates.Get (Set, Assoc_Name));
      end if;

      T := T & Value;

      Templates.Insert (Set, Templates.Assoc (Assoc_Name, T));
   end Add_TagV;

   procedure Add_TagV
     (Set        : in out Templates.Translate_Set;
      Assoc_Name : String;
      Tag        : Templates.Tag)
   is
      R : Templates.Tag;
   begin
      if Templates.Exists (Set, Assoc_Name) then
         declare
            T : constant Templates.Tag :=
                  Templates.Get (Templates.Get (Set, Assoc_Name));
         begin
            R := T & Tag;
         end;
      else
         R := +Tag;
      end if;

      Templates.Insert (Set, Templates.Assoc (Assoc_Name, R));
   end Add_TagV;

   --------
   -- CB --
   --------

   package body CB is separate;

   -------------
   -- CVS_Tag --
   -------------

   procedure CVS_Tag (O : in out Object) is
   begin
      O.CVS_Tag := True;
   end CVS_Tag;

   -----------
   -- Debug --
   -----------

   procedure Debug (O : in out Object) is
   begin
      O.Debug := True;
   end Debug;

   ------------------------
   -- Disable_Time_Stamp --
   ------------------------

   procedure Disable_Time_Stamp (O : in out Object) is
   begin
      O.Stamp := False;
   end Disable_Time_Stamp;

   -----------------
   -- End_Service --
   -----------------

   overriding procedure End_Service
     (O    : in out Object;
      Name : String) is
   begin
      --  Generate binding style information

      Output_Schema_Definition
        (O,
         Key   => "@binding.style",
         Value => SOAP.WSDL.Schema.Binding_Style'Image (O.Style));

      --  Generate the Schema information

      for C in WSDL.Types.Get_Schema_Definition.Iterate loop
         Output_Schema_Definition
           (O,
            Key   => AWS.Containers.Key_Value.Key (C),
            Value => AWS.Containers.Key_Value.Element (C));
      end loop;

      Generate
        (O,
         Characters.Handling.To_Lower (Format_Name (O, Name)) & "-types.ads",
         Template_Types_Ads, O.Type_S_Trans);
      Generate
        (O,
         Characters.Handling.To_Lower (Format_Name (O, Name)) & "-types.adb",
         Template_Types_Adb, O.Type_B_Trans);

      --  Stub

      if O.Gen_Stub then
         Stub.End_Service (O, Name);
      end if;

      --  Skeleton

      if O.Gen_Skel then
         Skel.End_Service (O, Name);
      end if;

      --  Callbacks

      if O.Gen_CB then
         CB.End_Service (O, Name);
      end if;
   end End_Service;

   --------------
   -- Endpoint --
   --------------

   procedure Endpoint (O : in out Object; URL : String) is
   begin
      O.Endpoint := To_Unbounded_String (URL);
   end Endpoint;

   -----------------
   -- Format_Name --
   -----------------

   function Format_Name (O : Object; Name : String) return String is

      function Ada_Format (Name : String) return String;
      --  Returns Name with the Ada style

      ----------------
      -- Ada_Format --
      ----------------

      function Ada_Format (Name : String) return String is
         Result : Unbounded_String;
      begin
         if not O.Ada_Style then
            --  No need to reformat this name
            return Name;
         end if;

         for K in Name'Range loop
            if K = Name'First then
               Append (Result, Characters.Handling.To_Upper (Name (K)));

            elsif Characters.Handling.Is_Upper (Name (K))
              and then not Characters.Handling.Is_Upper (Name (K - 1))
              and then K > Name'First
              and then Name (K - 1) not in '_' | '.' | '-'
              and then K < Name'Last
              and then Name (K + 1) not in '_' | '.' | '-'
            then
               Append (Result, "_" & Name (K));

            else
               Append (Result, Name (K));
            end if;
         end loop;

         return To_String (Result);
      end Ada_Format;

      Ada_Name : constant String := Ada_Format (Name);

   begin
      if SOAP.Utils.Is_Ada_Reserved_Word (Name) then
         return "v_" & Ada_Name;
      else
         return Ada_Name;
      end if;
   end Format_Name;

   ------------
   -- Gen_CB --
   ------------

   procedure Gen_CB (O : in out Object) is
   begin
      O.Gen_CB := True;
   end Gen_CB;

   ----------------------
   -- Gen_Safe_Pointer --
   ----------------------

   procedure Gen_Safe_Pointer (O : in out Object) is
   begin
      O.Sp := True;
   end Gen_Safe_Pointer;

   --------------
   -- Generate --
   --------------

   procedure Generate
     (O            : Object;
      File         : Text_IO.File_Type;
      Template     : String;
      Translations : Templates.Translate_Set)
   is
      use type Templates.Translate_Set;
      use type SOAP.WSDL.Schema.Binding_Style;

      Template_Dir  : constant String :=
                        Environment_Variables.Value
                          ("AWS_TEMPLATE_FILES", Default => "./");
      Template_File : constant String :=
                        Directories.Compose (Template_Dir, Template);
      Final_T       : Templates.Translate_Set := Translations;
   begin
      Final_T := Final_T
        & Templates.Assoc ("AWS_VERSION",  AWS.Version)
        & Templates.Assoc ("SOAP_VERSION", SOAP.Version)
        & Templates.Assoc ("OPTIONS", To_String (O.Options))
        & Templates.Assoc ("WSDL2AWS_VERSION", WSDL2AWS.Version)
        & Templates.Assoc
            ("HTTP_VERSION", HTTP_Protocol'Image (O.HTTP_Version))
        & Templates.Assoc ("HTTP_PROXY", O.Proxy)
        & Templates.Assoc ("HTTP_PROXY_USER", O.P_User)
        & Templates.Assoc ("HTTP_PROXY_PASSWORD", O.P_Pwd)
        & Templates.Assoc ("IS_RPC", O.Style = SOAP.WSDL.Schema.RPC)
        & Templates.Assoc ("DEBUG", O.Debug)
        & Templates.Assoc ("TRACE", O.Traces)
        & Templates.Assoc ("SERVICE_NAME", O.Unit)
        & Templates.Assoc ("SAFE_POINTER", O.Sp);

      if Types_Spec (O) /= "" then
         Add_TagV (Final_T, "USER_UNITS", Types_Spec (O, With_Clause => True));
      end if;

      if Procs_Spec (O) /= "" and then Procs_Spec (O) /= Types_Spec (O) then
         Add_TagV (Final_T, "USER_UNITS", Types_Spec (O, With_Clause => True));
      end if;

      if Directories.Exists (Template_File) then
         Text_IO.Put (File, Templates.Parse (Template_File, Final_T));
      else
         Text_IO.Put (File, Templates.Parse (Template, Final_T));
      end if;
   end Generate;

   procedure Generate
     (O            : Object;
      Filename     : String;
      Template     : String;
      Translations : Templates.Translate_Set)
   is
      File : Text_IO.File_Type;
   begin
      Text_IO.Create (File, Text_IO.Out_File, Filename);
      Generate (O, File, Template, Translations);
      Text_IO.Close (File);
   end Generate;

   ---------------------
   -- Generate_Params --
   ---------------------

   procedure Generate_Params
     (O                : Object;
      N                : WSDL.Parameters.P_Set;
      P_Decl           : in out Templates.Tag;
      P_Name           : in out Templates.Tag;
      P_Kind           : in out Templates.Tag;
      P_Min            : in out Templates.Tag;
      P_Max            : in out Templates.Tag;
      P_Compound_Size  : in out Templates.Tag;
      P_Type           : in out Templates.Tag;
      P_Base_Type      : in out Templates.Tag;
      P_Root_Type      : in out Templates.Tag;
      P_Root_Type_Kind : in out Templates.Tag;
      P_Type_Name      : in out Templates.Tag;
      P_Type_Kind      : in out Templates.Tag;
      P_Ada_Type       : in out Templates.Tag;
      P_Q_Name         : in out Templates.Tag;
      P_NS_Name        : in out Templates.Tag;
      P_NS_Value       : in out Templates.Tag;
      P_Elt_NS_Name    : in out Templates.Tag;
      P_Elt_NS_Value   : in out Templates.Tag)
   is
      use type WSDL.Types.Kind;
   begin
      P_Decl      := P_Decl & Format_Name (O, To_String (N.Name));
      P_Name      := P_Name & To_String (N.Name);
      P_Kind      := P_Kind & WSDL.Types.Kind'Image (N.Mode);
      P_Min       := P_Min & N.Min;
      P_Max       := P_Max & N.Max;
      P_Type      := P_Type & WSDL.Types.Name (N.Typ, True);
      P_Type_Name := P_Type_Name
                       & Format_Name
                           (O, WSDL.Types.Name (N.Typ, False));
      P_Q_Name    := P_Q_Name
                       & SOAP.Utils.To_Name (WSDL.Types.Name (N.Typ, True));
      P_Ada_Type  := P_Ada_Type & Type_Name (O, N);

      if N.Mode = WSDL.Types.K_Simple then
         P_Type_Kind := P_Type_Kind
           & SOAP.WSDL.To_Type (WSDL.Types.Name (N.Typ))'Image;
      else
         P_Type_Kind := P_Type_Kind & "";
      end if;

      if N.Mode /= WSDL.Types.K_Derived then
         P_Base_Type      := P_Base_Type & "";
         P_Root_Type_Kind := P_Root_Type_Kind & "";
      end if;

      if N.Mode in WSDL.Types.Compound_Type then
         P_Compound_Size := P_Compound_Size & WSDL.Parameters.Length (N);
      else
         P_Compound_Size := P_Compound_Size & 0;
      end if;

      declare
         Def    : constant WSDL.Types.Definition :=
                    WSDL.Types.Find (N.Typ);
         T_Name : constant String := WSDL.Types.Name (Def.Ref);
      begin
         case N.Mode is
            when WSDL.Types.K_Simple =>
               P_Root_Type := P_Root_Type
                 & SOAP.WSDL.Set_Type (SOAP.WSDL.To_Type (T_Name));

            when WSDL.Types.K_Derived =>
               P_Root_Type := P_Root_Type
                 & SOAP.WSDL.Set_Type
                     (SOAP.WSDL.To_Type
                        (WSDL.Types.Root_Type_For (Def)));

               declare
                  P_Name : constant String :=
                             WSDL.Types.Name (Def.Parent, True);
                  B_Name : constant String :=
                             (if SOAP.WSDL.Is_Standard (P_Name)
                              then WSDL.Types.Name (N.Typ, True)
                              else SOAP.Utils.To_Name (P_Name));
               begin
                  P_Base_Type      := P_Base_Type & B_Name;
                  P_Root_Type_Kind := P_Root_Type_Kind
                    & SOAP.WSDL.To_Type
                        (WSDL.Types.Root_Type_For (Def))'Image;
               end;

            when WSDL.Types.K_Enumeration =>
               P_Root_Type := P_Root_Type & "SOAP.Types.SOAP_Enumeration";

            when WSDL.Types.K_Array =>
               P_Root_Type := P_Root_Type & "SOAP.Types.SOAP_Array";

            when WSDL.Types.K_Record =>
               P_Root_Type := P_Root_Type & "SOAP.Types.SOAP_Record";
         end case;
      end;

      declare
         NS : constant SOAP.Name_Space.Object :=
                SOAP.WSDL.Name_Spaces.Get
                  (SOAP.Utils.NS (To_String (N.Elmt_Name)));
      begin
         P_Elt_NS_Name  := P_Elt_NS_Name & SOAP.Name_Space.Name (NS);
         P_Elt_NS_Value := P_Elt_NS_Value & SOAP.Name_Space.Value (NS);
      end;

      declare
         NS : constant SOAP.Name_Space.Object :=
                WSDL.Types.NS (N.Typ);
      begin
         P_NS_Name  := P_NS_Name & SOAP.Name_Space.Name (NS);
         P_NS_Value := P_NS_Value & SOAP.Name_Space.Value (NS);
      end;
   end Generate_Params;

   ------------------
   -- HTTP_Version --
   ------------------

   procedure HTTP_Version
     (O                : in out Object;
      Protocol_Version : HTTP_Protocol) is
   begin
      O.HTTP_Version := Protocol_Version;
   end HTTP_Version;

   ----------------------
   -- Insert_Types_Def --
   ----------------------

   procedure Insert_Types_Def
     (O            : in out Object;
      Template     : String;
      Translations : Templates.Translate_Set)
   is
      Template_Dir  : constant String :=
                        Environment_Variables.Value
                          ("AWS_TEMPLATE_FILES", Default => "./");
      Template_File : constant String :=
                        Directories.Compose (Template_Dir, Template);

      T_Name        : constant String :=
                        Templates.Get
                          (if Templates.Exists (Translations, "TYPE_NAME")
                           then Templates.Get (Translations, "TYPE_NAME")
                           else Templates.Get (Translations, "PROC"));
      Key           : constant String := T_Name & '@' & Template;
   begin
      if not Types_Gen.Contains (Key) then
         if Directories.Exists (Template_File) then
            Add_TagV
              (O.Type_S_Trans,
               "TYPE_DECLS",
               Templates.Parse (Template_File, Translations));
         else
            Add_TagV
              (O.Type_S_Trans,
               "TYPE_DECLS",
               Templates.Parse (Template, Translations));
         end if;

         Types_Gen.Insert (Key);
      end if;
   end Insert_Types_Def;

   ---------------------------------
   -- Is_Simple_Wrapped_Parameter --
   ---------------------------------

   function Is_Simple_Wrapped_Parameter
     (O  : Object;
      P  : WSDL.Parameters.P_Set) return Boolean
   is
      use type SOAP.WSDL.Schema.Binding_Style;
      use type WSDL.Parameters.P_Set;
      use type WSDL.Types.Kind;
   begin
      return P /= null
        and then P.Mode = WSDL.Types.K_Record
        and then O.Style = SOAP.WSDL.Schema.Document
        and then WSDL.Parameters.Length (P.P) >= 1;
   end Is_Simple_Wrapped_Parameter;

   ---------------
   -- Is_String --
   ---------------

   function Is_String (N : WSDL.Parameters.P_Set) return Boolean is
      use all type SOAP.WSDL.Parameter_Type;
      use type WSDL.Types.Kind;
   begin
      return N.Mode = WSDL.Types.K_Simple
        and then SOAP.WSDL.To_Type (WSDL.Types.Name (N.Typ)) = P_String;
   end Is_String;

   ----------
   -- Main --
   ----------

   procedure Main (O : in out Object; Name : String) is
   begin
      O.Main := To_Unbounded_String (Name);
   end Main;

   --------------
   -- Name_Set --
   --------------

   package body Name_Set is separate;

   -------------------
   -- New_Procedure --
   -------------------

   overriding procedure New_Procedure
     (O             : in out Object;
      Proc          : String;
      Documentation : String;
      SOAPAction    : String;
      Wrapper_Name  : String;
      Namespace     : SOAP.Name_Space.Object;
      Input         : WSDL.Parameters.P_Set;
      Output        : WSDL.Parameters.P_Set;
      Fault         : WSDL.Parameters.P_Set)
   is
      use type SOAP.WSDL.Schema.Binding_Style;
      use type WSDL.Parameters.P_Set;

      procedure Generate_Call_Signature (P : WSDL.Parameters.P_Set);
      --  Generate a call signature for Proc. This is needed to be able to map
      --  this signature to the corresponding SOAP operation when using the
      --  Document style binding. The signature is the key with the following
      --  format: '@' & <param1> & [:<param2>]

      procedure Generate_Schema (Prefix : String; P : WSDL.Parameters.P_Set);
      --  Generate the fully qualified name for the parameters. This is needed
      --  for the document/literal binding to match the payload with the
      --  corresponding data type.

      procedure Add_Proc_Tag
        (Tag_Name : String;
         Value    : String);
      procedure Add_Proc_Tag
        (Tag_Name : String;
         Value    : Boolean);
      procedure Add_Proc_Tag
        (Tag_Name : String;
         Value    : Templates.Tag);
      --  Add a tag for all procedure templates

      procedure Generate_Input_Params
        (O     : in out Object;
         Input : WSDL.Parameters.P_Set);
      --  Generate the input parameters NAME / TYPE

      procedure Generate_Output_Params
        (O      : in out Object;
         Proc   : String;
         Output : WSDL.Parameters.P_Set);
      --  Output procedure header into File. The terminating ';' or
      --  'is' is outputed depending on Spec value. If Mode is in
      --  Con_Stub_Header the connection based spec is generated,
      --  otherwise it is the endpoint based.

      ------------------
      -- Add_Proc_Tag --
      ------------------

      procedure Add_Proc_Tag
        (Tag_Name : String;
         Value    : String) is
      begin
         Add_TagV (O.Stub_S_Trans, Tag_Name, Value);
         Add_TagV (O.Stub_B_Trans, Tag_Name, Value);
         Add_TagV (O.Skel_S_Trans, Tag_Name, Value);
         Add_TagV (O.Skel_B_Trans, Tag_Name, Value);
      end Add_Proc_Tag;

      procedure Add_Proc_Tag
        (Tag_Name : String;
         Value    : Boolean) is
      begin
         Add_TagV (O.Stub_S_Trans, Tag_Name, Value);
         Add_TagV (O.Stub_B_Trans, Tag_Name, Value);
         Add_TagV (O.Skel_S_Trans, Tag_Name, Value);
         Add_TagV (O.Skel_B_Trans, Tag_Name, Value);
      end Add_Proc_Tag;

      procedure Add_Proc_Tag
        (Tag_Name : String;
         Value    : Templates.Tag) is
      begin
         Add_TagV (O.Stub_S_Trans, Tag_Name, Value);
         Add_TagV (O.Stub_B_Trans, Tag_Name, Value);
         Add_TagV (O.Skel_S_Trans, Tag_Name, Value);
         Add_TagV (O.Skel_B_Trans, Tag_Name, Value);
      end Add_Proc_Tag;

      -----------------------------
      -- Generate_Call_Signature --
      -----------------------------

      procedure Generate_Call_Signature (P : WSDL.Parameters.P_Set) is
         Sig : Unbounded_String;
         N   : WSDL.Parameters.P_Set := P;
      begin
         while N /= null loop
            if Sig = Null_Unbounded_String then
               Append (Sig, "@");
            else
               Append (Sig, ":");
            end if;

            Append (Sig, SOAP.Utils.No_NS (To_String (N.Elmt_Name)));

            N := N.Next;
         end loop;

         Output_Schema_Definition
           (O, To_String (Sig), To_String (O.Prefix) & Proc);
      end Generate_Call_Signature;

      ---------------------------
      -- Generate_Input_Params --
      ---------------------------

      procedure Generate_Input_Params
        (O     : in out Object;
         Input : WSDL.Parameters.P_Set)
      is
         use type WSDL.Types.Kind;

         Parameter_Name   : Templates.Tag;
         Parameter_Type   : Templates.Tag;
         P_Decl           : Templates.Tag;
         P_Name           : Templates.Tag;
         P_Kind           : Templates.Tag;
         P_Min            : Templates.Tag;
         P_Max            : Templates.Tag;
         P_Compound_Size  : Templates.Tag;
         P_Type           : Templates.Tag;
         P_Base_Type      : Templates.Tag;
         P_Root_Type      : Templates.Tag;
         P_Root_Type_Kind : Templates.Tag;
         P_Type_Name      : Templates.Tag;
         P_Type_Kind      : Templates.Tag;
         P_Ada_Type       : Templates.Tag;
         P_Q_Name         : Templates.Tag;
         P_NS_Name        : Templates.Tag;
         P_NS_Value       : Templates.Tag;
         P_Elt_NS_Name    : Templates.Tag;
         P_Elt_NS_Value   : Templates.Tag;
         N                : WSDL.Parameters.P_Set;
      begin
         if Is_Simple_Wrapped_Parameter (O, Input) then
            N := Input.P;
         else
            N := Input;
         end if;

         while N /= null loop
            declare
               Q_Name : constant String :=
                          SOAP.Utils.To_Name
                            (WSDL.Types.Name (N.Typ, NS => True));
               T_Name : constant String := WSDL.Types.Name (N.Typ);
            begin
               Parameter_Name := Parameter_Name
                 & Format_Name (O, To_String (N.Name));

               case N.Mode is
                  when WSDL.Types.K_Simple =>
                     Parameter_Type := Parameter_Type
                       & SOAP.WSDL.To_Ada (SOAP.WSDL.To_Type (T_Name));

                  when WSDL.Types.K_Enumeration =>
                     Parameter_Type := Parameter_Type
                       & (T_Name & "_Type");

                  when WSDL.Types.K_Derived =>
                     Parameter_Type := Parameter_Type
                       & (Q_Name & "_Type");

                  when WSDL.Types.K_Array =>
                     Parameter_Type := Parameter_Type
                       & (Format_Name (O, T_Name) & "_Type");

                  when WSDL.Types.K_Record =>
                     Parameter_Type := Parameter_Type
                       & (Format_Name (O, T_Name) & "_Type");
               end case;

               N := N.Next;
            end;
         end loop;

         Add_Proc_Tag ("PARAMETER_NAME", Parameter_Name);
         Add_Proc_Tag ("PARAMETER_TYPE", Parameter_Type);

         --  Parameters

         N := Input;

         while N /= null loop
            Generate_Params
              (O, N, P_Decl, P_Name, P_Kind, P_Min, P_Max, P_Compound_Size,
               P_Type, P_Base_Type, P_Root_Type, P_Root_Type_Kind,
               P_Type_Name, P_Type_Kind, P_Ada_Type, P_Q_Name,
               P_NS_Name, P_NS_Value, P_Elt_NS_Name, P_Elt_NS_Value);
            N := N.Next;
         end loop;

         Add_TagV (O.Stub_B_Trans, "IP_DECL_NAME", P_Decl);
         Add_TagV (O.Stub_B_Trans, "IP_NAME", P_Name);
         Add_TagV (O.Stub_B_Trans, "IP_KIND", P_Kind);
         Add_TagV (O.Stub_B_Trans, "IP_MIN", P_Min);
         Add_TagV (O.Stub_B_Trans, "IP_MAX", P_Max);
         Add_TagV (O.Stub_B_Trans, "IP_COMPOUND_SIZE", P_Compound_Size);
         Add_TagV (O.Stub_B_Trans, "IP_TYPE", P_Type);
         Add_TagV (O.Stub_B_Trans, "IP_BASE_TYPE", P_Base_Type);
         Add_TagV (O.Stub_B_Trans, "IP_ROOT_TYPE", P_Root_Type);
         Add_TagV (O.Stub_B_Trans, "IP_ROOT_TYPE_KIND", P_Root_Type_Kind);
         Add_TagV (O.Stub_B_Trans, "IP_TYPE_NAME", P_Type_Name);
         Add_TagV (O.Stub_B_Trans, "IP_TYPE_KIND", P_Type_Kind);
         Add_TagV (O.Stub_B_Trans, "IP_ADA_TYPE", P_Ada_Type);
         Add_TagV (O.Stub_B_Trans, "IP_Q_NAME", P_Q_Name);
         Add_TagV (O.Stub_B_Trans, "IP_NS_NAME", P_NS_Name);
         Add_TagV (O.Stub_B_Trans, "IP_NS_VALUE", P_NS_Value);
         Add_TagV (O.Stub_B_Trans, "IP_ELT_NS_NAME", P_Elt_NS_Name);
         Add_TagV (O.Stub_B_Trans, "IP_ELT_NS_VALUE", P_Elt_NS_Value);

         Add_TagV (O.Skel_B_Trans, "IP_DECL_NAME", P_Decl);
         Add_TagV (O.Skel_B_Trans, "IP_NAME", P_Name);
         Add_TagV (O.Skel_B_Trans, "IP_KIND", P_Kind);
         Add_TagV (O.Skel_B_Trans, "IP_MIN", P_Min);
         Add_TagV (O.Skel_B_Trans, "IP_MAX", P_Max);
         Add_TagV (O.Skel_B_Trans, "IP_COMPOUND_SIZE", P_Compound_Size);
         Add_TagV (O.Skel_B_Trans, "IP_TYPE", P_Type);
         Add_TagV (O.Skel_B_Trans, "IP_BASE_TYPE", P_Base_Type);
         Add_TagV (O.Skel_B_Trans, "IP_ROOT_TYPE", P_Root_Type);
         Add_TagV (O.Skel_B_Trans, "IP_ROOT_TYPE_KIND", P_Root_Type_Kind);
         Add_TagV (O.Skel_B_Trans, "IP_TYPE_NAME", P_Type_Name);
         Add_TagV (O.Skel_B_Trans, "IP_TYPE_KIND", P_Type_Kind);
         Add_TagV (O.Skel_B_Trans, "IP_ADA_TYPE", P_Ada_Type);
         Add_TagV (O.Skel_B_Trans, "IP_Q_NAME", P_Q_Name);
         Add_TagV (O.Skel_B_Trans, "IP_NS_NAME", P_NS_Name);
         Add_TagV (O.Skel_B_Trans, "IP_NS_VALUE", P_NS_Value);
         Add_TagV (O.Skel_B_Trans, "IP_ELT_NS_NAME", P_Elt_NS_Name);
         Add_TagV (O.Skel_B_Trans, "IP_ELT_NS_VALUE", P_Elt_NS_Value);
      end Generate_Input_Params;

      ----------------------------
      -- Generate_Output_Params --
      ----------------------------

      procedure Generate_Output_Params
        (O      : in out Object;
         Proc   : String;
         Output : WSDL.Parameters.P_Set)
      is
         use type WSDL2AWS.WSDL.Types.Kind;

         N                  : WSDL.Parameters.P_Set;
         Proc_S_Return_Type : Templates.Tag;
         Proc_B_Return_Type : Templates.Tag;

         P_Decl           : Templates.Tag;
         P_Name           : Templates.Tag;
         P_Kind           : Templates.Tag;
         P_Min            : Templates.Tag;
         P_Max            : Templates.Tag;
         P_Compound_Size  : Templates.Tag;
         P_Type           : Templates.Tag;
         P_Base_Type      : Templates.Tag;
         P_Root_Type      : Templates.Tag;
         P_Root_Type_Kind : Templates.Tag;
         P_Type_Name      : Templates.Tag;
         P_Type_Kind      : Templates.Tag;
         P_Ada_Type       : Templates.Tag;
         P_Q_Name         : Templates.Tag;
         P_NS_Name        : Templates.Tag;
         P_NS_Value       : Templates.Tag;
         P_Elt_NS_Name    : Templates.Tag;
         P_Elt_NS_Value   : Templates.Tag;

      begin
         if Is_Simple_Wrapped_Parameter (O, Output) then
            N := Output.P;
         else
            N := Output;
         end if;

         if N /= null then
            if Is_Simple_Wrapped_Parameter (O, Output)
              and then N.Mode = WSDL.Types.K_Record
            then
               --  A record inside a record in Document style binding

               Proc_S_Return_Type := Proc_S_Return_Type
                 & (Format_Name (O, WSDL.Types.Name (N.Typ) & "_Type"));
            else
               Proc_S_Return_Type := Proc_S_Return_Type
                 & (Result_Type (O, Proc, N));
            end if;
         else
            Proc_S_Return_Type := Proc_S_Return_Type
              & "Not_A_Function";
         end if;

         --  Only done once, ???? can probably be removed after clean-up
         Add_TagV (O.Stub_S_Trans, "PROC_RETURN_TYPE", Proc_S_Return_Type);
         Add_TagV (O.Stub_B_Trans, "PROC_RETURN_TYPE", Proc_S_Return_Type);
         Add_TagV (O.Skel_S_Trans, "PROC_RETURN_TYPE", Proc_S_Return_Type);
         Add_TagV (O.Skel_B_Trans, "PROC_RETURN_TYPE", Proc_S_Return_Type);

         N := Output;

         if N /= null then
            if Is_Simple_Wrapped_Parameter (O, Output)
              and then N.Mode = WSDL.Types.K_Record
            then
               --  A record inside a record in Document style binding
               Proc_B_Return_Type := Proc_B_Return_Type
                 & (Format_Name (O, WSDL.Types.Name (N.Typ) & "_Type"));
            else
               Proc_B_Return_Type := Proc_B_Return_Type
                 & (Result_Type (O, Proc, N));
            end if;
         else
            Proc_B_Return_Type := Proc_B_Return_Type
              & "Not_A_Function";
         end if;

         Add_TagV
           (O.Stub_B_Trans, "PROC_CB_RETURN_TYPE", Proc_B_Return_Type);
         Add_TagV
           (O.Skel_B_Trans, "PROC_CB_RETURN_TYPE", Proc_S_Return_Type);

         if Output = null then
            Add_TagV (O.Skel_B_Trans, "SINGLE_OUT_PARAMETER", False);
            Add_TagV (O.Stub_B_Trans, "SINGLE_OUT_PARAMETER", False);
            Add_TagV (O.Skel_B_Trans, "RETURN_TYPE_KIND", "NONE");

         else
            if Is_Simple_Wrapped_Parameter (O, Output)
              and then Is_String (Output.P)
            then
               Add_TagV (O.Skel_B_Trans, "RETURN_TYPE_KIND", "P_STRING");
            else
               Add_TagV (O.Skel_B_Trans, "RETURN_TYPE_KIND", "NONE");
            end if;

            if Output.Next = null then
               Add_TagV (O.Skel_B_Trans, "SINGLE_OUT_PARAMETER", True);
               Add_TagV (O.Stub_B_Trans, "SINGLE_OUT_PARAMETER", True);
            else
               Add_TagV (O.Skel_B_Trans, "SINGLE_OUT_PARAMETER", False);
               Add_TagV (O.Stub_B_Trans, "SINGLE_OUT_PARAMETER", False);
            end if;
         end if;

         --  Output parameters

         if Output /= null
           and then Output.Next = null
         then
            Generate_Params
              (O, Output, P_Decl, P_Name, P_Kind,
               P_Min, P_Max, P_Compound_Size,
               P_Type, P_Base_Type, P_Root_Type, P_Root_Type_Kind,
               P_Type_Name, P_Type_Kind, P_Ada_Type, P_Q_Name,
               P_NS_Name, P_NS_Value, P_Elt_NS_Name, P_Elt_NS_Value);
         end if;

         Add_TagV (O.Skel_B_Trans, "OP_DECL_NAME", P_Decl);
         Add_TagV (O.Skel_B_Trans, "OP_NAME", P_Name);
         Add_TagV (O.Skel_B_Trans, "OP_KIND", P_Kind);
         Add_TagV (O.Skel_B_Trans, "OP_MIN", P_Min);
         Add_TagV (O.Skel_B_Trans, "OP_MAX", P_Max);
         Add_TagV (O.Skel_B_Trans, "OP_COMPOUND_SIZE", P_Compound_Size);
         Add_TagV (O.Skel_B_Trans, "OP_TYPE", P_Type);
         Add_TagV (O.Skel_B_Trans, "OP_BASE_TYPE", P_Base_Type);
         Add_TagV (O.Skel_B_Trans, "OP_ROOT_TYPE", P_Root_Type);
         Add_TagV (O.Skel_B_Trans, "OP_ROOT_TYPE_KIND", P_Root_Type_Kind);
         Add_TagV (O.Skel_B_Trans, "OP_TYPE_NAME", P_Type_Name);
         Add_TagV (O.Skel_B_Trans, "OP_TYPE_KIND", P_Type_Kind);
         Add_TagV (O.Skel_B_Trans, "OP_ADA_TYPE", P_Ada_Type);
         Add_TagV (O.Skel_B_Trans, "OP_Q_NAME", P_Q_Name);
         Add_TagV (O.Skel_B_Trans, "OP_NS_NAME", P_NS_Name);
         Add_TagV (O.Skel_B_Trans, "OP_NS_VALUE", P_NS_Value);
         Add_TagV (O.Skel_B_Trans, "OP_ELT_NS_NAME", P_Elt_NS_Name);
         Add_TagV (O.Skel_B_Trans, "OP_ELT_NS_VALUE", P_Elt_NS_Value);

         Add_TagV (O.Stub_B_Trans, "OP_DECL_NAME", P_Decl);
         Add_TagV (O.Stub_B_Trans, "OP_NAME", P_Name);
         Add_TagV (O.Stub_B_Trans, "OP_KIND", P_Kind);
         Add_TagV (O.Stub_B_Trans, "OP_MIN", P_Min);
         Add_TagV (O.Stub_B_Trans, "OP_MAX", P_Max);
         Add_TagV (O.Stub_B_Trans, "OP_COMPOUND_SIZE", P_Compound_Size);
         Add_TagV (O.Stub_B_Trans, "OP_TYPE", P_Type);
         Add_TagV (O.Stub_B_Trans, "OP_BASE_TYPE", P_Base_Type);
         Add_TagV (O.Stub_B_Trans, "OP_ROOT_TYPE", P_Root_Type);
         Add_TagV (O.Stub_B_Trans, "OP_ROOT_TYPE_KIND", P_Root_Type_Kind);
         Add_TagV (O.Stub_B_Trans, "OP_TYPE_NAME", P_Type_Name);
         Add_TagV (O.Stub_B_Trans, "OP_TYPE_KIND", P_Type_Kind);
         Add_TagV (O.Stub_B_Trans, "OP_ADA_TYPE", P_Ada_Type);
         Add_TagV (O.Stub_B_Trans, "OP_Q_NAME", P_Q_Name);
         Add_TagV (O.Stub_B_Trans, "OP_NS_NAME", P_NS_Name);
         Add_TagV (O.Stub_B_Trans, "OP_NS_VALUE", P_NS_Value);
         Add_TagV (O.Stub_B_Trans, "OP_ELT_NS_NAME", P_Elt_NS_Name);
         Add_TagV (O.Stub_B_Trans, "OP_ELT_NS_VALUE", P_Elt_NS_Value);
      end Generate_Output_Params;

      ---------------------
      -- Generate_Schema --
      ---------------------

      procedure Generate_Schema (Prefix : String; P : WSDL.Parameters.P_Set) is

         procedure Generate_Wrapper (Name : String; P : WSDL.Parameters.P_Set);
         --  Handles top-level wrapper

         procedure Generate_Array (Name : String; P : WSDL.Parameters.P_Set);
         --  Handles arrays

         procedure Generate_Record (Name : String; P : WSDL.Parameters.P_Set);
         --  Handlers records

         procedure Generate_Type (Name : String; P : WSDL.Parameters.P_Set);
         --  Handles types

         --------------------
         -- Generate_Array --
         --------------------

         procedure Generate_Array (Name : String; P : WSDL.Parameters.P_Set) is
            Def    : constant WSDL.Types.Definition := WSDL.Types.Find (P.Typ);
            E_Name : constant String := To_String (Def.E_Name);
            Q_Name : constant String := Name & (if E_Name = ""
                                                then ""
                                                else '.' & E_Name);
         begin
            if E_Name = "" then
               --  This is a set and not an array, inside we have a record
               Output_Schema_Definition (O, Name & "@is_a", "@record");
            else
               Output_Schema_Definition (O, Name & "@is_a", "@array");
            end if;

            if P.P /= null then
               Output_Schema_Definition (O, Q_Name, WSDL.Types.Name (P.P.Typ));
               Generate_Wrapper (Q_Name, P.P);
            end if;
         end Generate_Array;

         ---------------------
         -- Generate_Record --
         ---------------------

         procedure Generate_Record
           (Name : String; P : WSDL.Parameters.P_Set)
         is
            E : WSDL.Parameters.P_Set := P.P;
         begin
            Output_Schema_Definition (O, Name & "@is_a", "@record");

            while E /= null loop
               Generate_Wrapper (Name & '.' & To_String (E.Name), E);
               E := E.Next;
            end loop;
         end Generate_Record;

         ---------------------
         -- Generate_Type --
         ---------------------

         procedure Generate_Type
           (Name : String; P : WSDL.Parameters.P_Set) is
         begin
            Output_Schema_Definition
              (O, Name & "@is_a", WSDL.Types.Name (P.Typ, True));
            Output_Schema_Definition
              (O, Name & "@is_a", WSDL.Types.Name (P.Typ, False));
         end Generate_Type;

         ----------------------
         -- Generate_Wrapper --
         ----------------------

         procedure Generate_Wrapper
           (Name : String; P : WSDL.Parameters.P_Set)
         is
            use all type WSDL.Types.Kind;
         begin
            case P.Mode is
               when K_Array =>
                  Generate_Array (Name, P);
               when K_Record =>
                  Generate_Record (Name, P);
               when K_Enumeration | K_Derived | K_Simple =>
                  Generate_Type (Name, P);
            end case;
         end Generate_Wrapper;

         N : WSDL.Parameters.P_Set := P;

      begin
         while N /= null loop
            Generate_Wrapper
              (Prefix & SOAP.Utils.No_NS (To_String (N.Name)), N);
            N := N.Next;
         end loop;
      end Generate_Schema;

      L_Proc : constant String := Format_Name (O, Proc);
      W_Name : constant String := (if O.Style = SOAP.WSDL.Schema.Document
                                   then Wrapper_Name else Proc);

   begin
      if not O.Quiet then
         Text_IO.Put_Line ("   > " & Proc);
      end if;

      Put_Types (O, Proc, Wrapper_Name, Input, Output);

      Generate_Input_Params (O, Input);

      Generate_Output_Params (O, Proc, Output);

      Add_Proc_Tag ("HAS_INPUT", Input /= null);
      Add_Proc_Tag ("HAS_OUTPUT", Output /= null);
      Add_Proc_Tag ("PROC", L_Proc);
      Add_Proc_Tag ("DOCUMENTATION", Documentation);
      Add_Proc_Tag ("SOAP_ACTION", To_String (O.Prefix) & SOAPAction);

      Add_TagV (O.Stub_B_Trans, "SOAP_PROC", To_String (O.Prefix) & W_Name);
      Add_TagV (O.Skel_B_Trans, "SOAP_PROC", To_String (O.Prefix) & Proc);

      Add_Proc_Tag
        ("SIMPLE_WRAPPED_IN_PARAMETER",
         Is_Simple_Wrapped_Parameter (O, Input));
      Add_Proc_Tag
        ("SIMPLE_WRAPPED_OUT_PARAMETER",
         Is_Simple_Wrapped_Parameter (O, Output));

      if O.Gen_Stub then
         Stub.New_Procedure
           (O, Proc, Documentation, SOAPAction, Wrapper_Name, Namespace,
            Input, Output, Fault);
      end if;

      if O.Gen_Skel then
         Skel.New_Procedure
           (O, Proc, Documentation, SOAPAction, Wrapper_Name, Namespace,
            Input, Output, Fault);
      end if;

      Generate_Call_Signature (Input);
      Generate_Call_Signature (Output);

      --  Then generate schema (fully prefixed for document/literal)

      Generate_Schema
        ((if O.Style = SOAP.WSDL.Schema.Document then "" else Proc & '.'),
         Input);

      Generate_Schema
        ((if O.Style = SOAP.WSDL.Schema.Document
         then "" else Proc & "Response."),
         Output);

      --  Skip line after procedure signatures

      if O.Gen_CB then
         CB.New_Procedure
           (O, Proc, Documentation, SOAPAction, Wrapper_Name, Namespace,
            Input, Output, Fault);
      end if;
   end New_Procedure;

   -------------
   -- No_Skel --
   -------------

   procedure No_Skel (O : in out Object) is
   begin
      O.Gen_Skel := False;
   end No_Skel;

   -------------
   -- No_Stub --
   -------------

   procedure No_Stub (O : in out Object) is
   begin
      O.Gen_Stub := False;
   end No_Stub;

   -------------
   -- Options --
   -------------

   procedure Options (O : in out Object; Options : String) is
   begin
      O.Options := To_Unbounded_String (Options);
   end Options;

   ------------------------------
   -- Output_Schema_Definition --
   ------------------------------

   procedure Output_Schema_Definition
     (O          : in out Object;
      Key, Value : String) is
   begin
      if not S_Gen.Contains (Key) then
         S_Gen.Insert (Key, Value);

         Add_TagV (O.Type_B_Trans, "SCHEMA_DECLS_KEY", Key);
         Add_TagV (O.Type_B_Trans, "SCHEMA_DECLS_VALUE", Value);
      end if;
   end Output_Schema_Definition;

   ---------------
   -- Overwrite --
   ---------------

   procedure Overwrite (O : in out Object) is
   begin
      O.Force := True;
   end Overwrite;

   ----------------
   -- Procs_Spec --
   ----------------

   function Procs_Spec
     (O           : Object;
      With_Clause : Boolean := False) return String
   is
      Prefix : constant String :=
                 (if With_Clause then "" else "Standard.");
   begin
      if O.Spec /= Null_Unbounded_String then
         return Prefix & To_String (O.Spec);
      elsif O.Types_Spec /= Null_Unbounded_String then
         return Prefix & To_String (O.Types_Spec);
      else
         return "";
      end if;
   end Procs_Spec;

   ---------------
   -- Put_Types --
   ---------------

   procedure Put_Types
     (O          : in out Object;
      Proc       : String;
      SOAPAction : String;
      Input      : WSDL.Parameters.P_Set;
      Output     : WSDL.Parameters.P_Set)
   is
      use Characters.Handling;

      use type AWS.Templates.Translate_Set;

      use type WSDL.Parameters.P_Set;
      use type WSDL.Types.Kind;

      use type SOAP.WSDL.Schema.Binding_Style;

      W_Name : constant String :=
                 (if O.Style = SOAP.WSDL.Schema.Document
                  then SOAPAction
                  else Proc);

      D_Gen : String_Store.Set;

      procedure Generate_Record
        (Name      : String;
         Suffix    : String;
         P         : WSDL.Parameters.P_Set;
         Is_Output : Boolean               := False);
      --  Output record definitions (type and routine conversion). Note that
      --  this routine also handles choice records. The current implementation
      --  only handles single occurence of a choice.

      procedure Generate_Array
        (Name  : String;
         P     : WSDL.Parameters.P_Set;
         Regen : Boolean);
      --  Generate array definitions (type and routine conversion)

      procedure Generate_Derived
        (Name : String;
         Def  : WSDL.Types.Definition;
         P    : WSDL.Parameters.P_Set);
      --  Generate derived type definition

      procedure Generate_Enumeration
        (Name : String;
         P    : WSDL.Parameters.P_Set);
      --  Generate enumeration type definition

      function Generate_Namespace
        (NS     : SOAP.Name_Space.Object;
         Create : Boolean) return String;
      --  Generate the namespace package from NS

      procedure Get_References
        (Unit_List   : in out AWS.Templates.Tag;
         P           : WSDL.Parameters.P_Set;
         For_Derived : Boolean := False);
      --  Add unit to be with/use into List

      procedure Initialize_Types_Package
        (Translations : in out Templates.Translate_Set;
         P            : WSDL.Parameters.P_Set;
         Name         : String;
         Output       : Boolean;
         Prefix       : out Unbounded_String;
         Def          : WSDL.Types.Definition := WSDL.Types.No_Definition;
         Regen        : Boolean := False);
      --  Creates the full namespaces if needed and return it in Prefix.
      --  Creates also the package hierarchy. Returns a spec and body file
      --  descriptor.

      procedure Output_Types (P : WSDL.Parameters.P_Set);
      --  Output types conversion routines

      function Get_Routine (P : WSDL.Parameters.P_Set) return String;
      --  Returns the Get routine for the given type

      function Set_Routine (P : WSDL.Parameters.P_Set) return String;
      --  Returns the constructor routine for the given type

      function Is_Inside_Record (Name : String) return Boolean;
      --  Returns True if Name is defined inside a record in the Input
      --  or Output parameter list.

      --------------------
      -- Generate_Array --
      --------------------

      procedure Generate_Array
        (Name  : String;
         P     : WSDL.Parameters.P_Set;
         Regen : Boolean)
      is
         use type WSDL.Types.Definition;

         function To_Ada_Type (Name : String) return String;
         --  Returns the Ada corresponding type (for array element)

         function Set_Type (Def  : WSDL.Types.Definition) return String;
         --  Returns the SOAP type for Name

         --------------
         -- Set_Type --
         --------------

         function Set_Type (Def  : WSDL.Types.Definition) return String is
            Name : constant String := WSDL.Types.Name (Def.Ref);
         begin
            if SOAP.WSDL.Is_Standard (Name) then
               return SOAP.WSDL.Set_Type (SOAP.WSDL.To_Type (Name));

            else
               if Def.Mode = WSDL.Types.K_Derived then
                  return Set_Type (WSDL.Types.Find (Def.Parent));

               elsif Def.Mode = WSDL.Types.K_Enumeration then
                  return "SOAP.Types.SOAP_Enumeration";

               else
                  return "SOAP.Types.SOAP_Record";
               end if;
            end if;
         end Set_Type;

         -----------------
         -- To_Ada_Type --
         -----------------

         function To_Ada_Type (Name : String) return String is
         begin
            if SOAP.WSDL.Is_Standard (Name) then
               return SOAP.WSDL.To_Ada
                 (SOAP.WSDL.To_Type (Name), Constrained => True);

            else
               return Format_Name (O, Name) & "_Type";
            end if;
         end To_Ada_Type;

         S_Name  : constant String := Name (Name'First .. Name'Last - 5);
         --  Simple name without the ending _Type

         Def     : constant WSDL.Types.Definition := WSDL.Types.Find (P.Typ);
         NS      : constant SOAP.Name_Space.Object := WSDL.Types.NS (P.Typ);
         Pck_NS  : constant String :=
                     To_Unit_Name (Generate_Namespace (NS, False));

         F_Name  : constant String := Format_Name (O, Name);
         E_Type  : constant WSDL.Types.Definition :=
                     WSDL.Types.Find
                       (if Def = WSDL.Types.No_Definition
                        then P.Typ
                        else Def.E_Type);
         Q_Name  : constant String :=
                     (WSDL.Types.Name
                        (E_Type.Ref,
                         NS => E_Type.Mode = WSDL.Types.K_Derived));
         T_Name  : constant String :=
                     (if WSDL.Types.Is_Character (E_Type)
                      then SOAP.Utils.No_NS (Q_Name)
                      else SOAP.Utils.To_Name (Q_Name));

         --  Array's element type name

         ET_Name : constant String :=
                     (if Def = WSDL.Types.No_Definition
                      then WSDL.Types.Name (P.P.Typ, True)
                      else WSDL.Types.Name (Def.E_Type, True));

         Prefix       : Unbounded_String;
         Translations : Templates.Translate_Set;
      begin
         Initialize_Types_Package
           (Translations, P, F_Name, False, Prefix, Regen => Regen);

         Translations := Translations
           & Templates.Assoc ("TYPE_NAME", F_Name)
           & Templates.Assoc ("NAME_SPACE", SOAP.Name_Space.Name (NS))
           & Templates.Assoc ("LENGTH", P.Length)
           & Templates.Assoc ("INSIDE_RECORD", Is_Inside_Record (S_Name))
           & Templates.Assoc ("QUALIFIED_NAME", Q_Name)
           & Templates.Assoc ("ELEMENT_TYPE", To_Ada_Type (T_Name))
           & Templates.Assoc ("ELEMENT_NAME", To_String (Def.E_Name))
           & Templates.Assoc ("QUALIFIED_ELEMENT_TYPE", ET_Name)
           & Templates.Assoc
               ("SET_TYPE", Set_Type (WSDL.Types.Find (Def.E_Type)))
           & Templates.Assoc
               ("SET_ROUTINE", Set_Routine (P))
           & Templates.Assoc
               ("GET_ROUTINE", Get_Routine (P))
           & Templates.Assoc
               ("TYPE_REF", WSDL.Types.Name (P.Typ))
           & Templates.Assoc
               ("DOCUMENTATION", P.Doc);

         if not NS_Generated.Contains (SOAP.Name_Space.Name (NS)) then
            Translations := Translations
              & Templates.Assoc ("NAME_SPACE_PACKAGE", Pck_NS);
            NS_Generated.Insert (SOAP.Name_Space.Name (NS));
         end if;

         Generate
           (O,
            To_Lower (To_String (Prefix)) & ".ads",
            Template_Array_Ads, Translations);

         Insert_Types_Def (O, Template_Array_Types, Translations);
      end Generate_Array;

      ----------------------
      -- Generate_Derived --
      ----------------------

      procedure Generate_Derived
        (Name : String;
         Def  : WSDL.Types.Definition;
         P    : WSDL.Parameters.P_Set)
      is
         U_Name  : constant String := SOAP.Utils.No_NS (Name) & "_Type";
         --  Unit name must not have a namespace

         Q_Name  : constant String := SOAP.Utils.To_Name (Name);
         F_Name  : constant String :=
                     Format_Name (O, SOAP.Utils.To_Name (Name) & "_Type");
         P_Name  : constant String := WSDL.Types.Name (Def.Parent, True);
         B_Name  : constant String :=
                     SOAP.Utils.To_Name
                       ((if SOAP.WSDL.Is_Standard (P_Name)
                        then SOAP.WSDL.To_Ada
                          (SOAP.WSDL.To_Type (P_Name),
                           not WSDL.Types.Is_Constrained (Def)
                           and then Types_Spec (O) = "")
                        else P_Name & "_Type"));

         Prefix           : Unbounded_String;
         L_Range, U_Range : Unbounded_String;
         Is_Range         : Boolean := True;
         Predicate_Kind   : Templates.Tag;
         Predicate        : Templates.Tag;
         Translations     : Templates.Translate_Set;
      begin
         Initialize_Types_Package
           (Translations, P, U_Name, False, Prefix, Def);

         --  Is types are to be reused from an Ada  spec ?

         Translations := Translations
           & Templates.Assoc ("TYPE_NAME", F_Name)
           & Templates.Assoc ("BASE_NAME", B_Name)
           & Templates.Assoc ("QUALIFIED_NAME", Q_Name)
           & Templates.Assoc ("PARENT_NAME", SOAP.Utils.No_NS (P_Name))
           & Templates.Assoc ("DOCUMENTATION", P.Doc);

         if not D_Gen.Contains (B_Name) then
            D_Gen.Insert (B_Name);
            Translations := Translations
              & Templates.Assoc ("UNIQ_DERIVED", True);
         end if;

         --  For array support

         Translations := Translations
           & Templates.Assoc
               ("FROM_SOAP",
                WSDL.Types.From_SOAP (Def, Object => "O"))
           & Templates.Assoc
               ("TO_SOAP",
                WSDL.Types.To_SOAP
                  (Def,
                   Object    => "D",
                   Name      => "Name",
                   Type_Name => "Type_Name",
                   Name_Kind => WSDL.Types.Both_Var,
                   NS        => "NS"))
           & Templates.Assoc
               ("SET_TYPE",
                SOAP.WSDL.Set_Type
                 (SOAP.WSDL.To_Type (WSDL.Types.Root_Type_For (Def))));

         if Types_Spec (O) = "" then
            declare
               use type SOAP.WSDL.Parameter_Type;

               Root_Type   : constant SOAP.WSDL.Parameter_Type :=
                               SOAP.WSDL.To_Type
                                 (WSDL.Types.Root_Type_For (Def));
               Constraints : WSDL.Types.Constraints_Def;
            begin
               --  Get constraints from parent types. Note that we do not want
               --  the constraint of the first parent only, but the constraints
               --  from the whole derived hierarchy.

               WSDL.Types.Get_Constraints (Def, Constraints);

               --  Check if we have to build a regexp

               if Root_Type = SOAP.WSDL.P_String
                 and then Constraints.Pattern /= Null_Unbounded_String
               then
                  Translations := Translations
                    & Templates.Assoc ("CONSTRAINT_PATTERN",
                                       To_String (Constraints.Pattern));
               end if;

               --  Generate constraints if any. We first get the root type to
               --  know if the constraints are on integers, floats or strings.
               --
               --  * on integers and floats we generate a range:
               --
               --     range <lower> .. <upper>
               --
               --  where <lower> or <upper> could the base type 'First or
               --  'Last.
               --
               --  * on static strings we generate:
               --
               --     (1 .. <length>)
               --       Dynamic_Preficate => Match (<type>, <pattern>);
               --
               --  * on variable length strings we generate aspects:
               --
               --     Dynamic_Predicate =>
               --       Length (Unbounded_String (<type>)) >= <min>
               --       and then Length (Unbounded_String (<type>)) <= <max>
               --       and then Match (To_String (<type>), <pattern>)

               case Root_Type is
                  when SOAP.WSDL.P_Float =>
                     declare
                        Lower, Upper : Float;
                        L_Set, U_Set : Boolean;
                     begin
                        Lower := Float'First;
                        Upper := Float'Last;

                        --  Get constraints from the WSDL definition

                        WSDL.Types.Get_Constraint_Float
                          (Constraints, Lower, L_Set, Upper, U_Set);

                        --  If constraints are found, write them

                        if L_Set or U_Set then
                           L_Range := To_Unbounded_String
                             ((if not L_Set or else Lower < 0.0
                                then " " else "")
                              & (if L_Set
                                then Float'Image (Lower)
                                else " " & B_Name & "'First"));
                           U_Range := To_Unbounded_String
                              ((if not U_Set or else Upper < 0.0
                                then " " else "")
                              & (if L_Set
                                then Float'Image (Upper)
                                else " " & B_Name & "'Last"));
                        end if;
                     end;

                  when SOAP.WSDL.P_Double =>
                     declare
                        Lower, Upper : Long_Float;
                        L_Set, U_Set : Boolean;
                     begin
                        Lower := Long_Float'First;
                        Upper := Long_Float'Last;

                        --  Get constraints from the WSDL definition

                        WSDL.Types.Get_Constraint_Double
                          (Constraints, Lower, L_Set, Upper, U_Set);

                        --  If constraints are found, write them

                        if L_Set or U_Set then
                           L_Range := To_Unbounded_String
                             ((if not L_Set or else Lower < 0.0
                                then " " else "")
                              & (if L_Set
                                then Long_Float'Image (Lower)
                                else B_Name & "'First"));
                           U_Range := To_Unbounded_String
                              ((if not U_Set or else Upper < 0.0
                                then " " else "")
                              & (if U_Set
                                then Long_Float'Image (Upper)
                                else B_Name & "'Last"));
                        end if;
                     end;

                  when SOAP.WSDL.P_String =>
                     if WSDL.Types.Is_Constrained (Def) then
                        L_Range := To_Unbounded_String ("1");
                        U_Range := To_Unbounded_String
                          (Natural'Image (Constraints.Length));
                        Is_Range := False;

                     else
                        declare
                           Unset : Integer renames WSDL.Types.Unset;
                           Empty : Unbounded_String
                                     renames Null_Unbounded_String;
                        begin
                           if Constraints.Min_Length /= WSDL.Types.Unset
                             or else Constraints.Max_Length /= WSDL.Types.Unset
                             or else Constraints.Pattern /= Empty
                           then
                              if Constraints.Min_Length /= Unset then
                                 Predicate_Kind := Predicate_Kind
                                   & "MIN";
                                 Predicate := Predicate
                                   & Constraints.Min_Length;
                              end if;

                              if Constraints.Max_Length /= Unset then
                                 Predicate_Kind := Predicate_Kind
                                   & "MAX";
                                 Predicate := Predicate
                                   & Constraints.Max_Length;
                              end if;

                              if Constraints.Pattern /= Empty then
                                 Predicate_Kind := Predicate_Kind
                                   & "PATTERN";
                                 Predicate := Predicate
                                   & "PATTERN";
                              end if;
                           end if;
                        end;
                     end if;

                  when  SOAP.WSDL.P_Character =>
                     null;

                  when SOAP.WSDL.P_Any_Type | SOAP.WSDL.P_B64 =>
                     null;

                  when others =>
                     declare
                        use SOAP;
                        Lower, Upper : Long_Long_Integer;
                        L_Set, U_Set : Boolean;
                     begin
                        --  Set min/max depending on the type

                        case Root_Type is
                           when SOAP.WSDL.P_Byte =>
                              Lower := Long_Long_Integer (Types.Byte'First);
                              Upper := Long_Long_Integer (Types.Byte'Last);

                           when SOAP.WSDL.P_Short =>
                              Lower := Long_Long_Integer (Types.Short'First);
                              Upper := Long_Long_Integer (Types.Short'Last);

                           when SOAP.WSDL.P_Integer =>
                              Lower := Long_Long_Integer (Integer'First);
                              Upper := Long_Long_Integer (Integer'Last);

                           when SOAP.WSDL.P_Long =>
                              Lower := Long_Long_Integer (Types.Long'First);
                              Upper := Long_Long_Integer (Types.Long'Last);

                           when SOAP.WSDL.P_Unsigned_Byte =>
                              Lower := 0;
                              Upper := Long_Long_Integer (Types.Byte'Last);

                           when SOAP.WSDL.P_Unsigned_Short =>
                              Lower := 0;
                              Upper := Long_Long_Integer (Types.Short'Last);

                           when SOAP.WSDL.P_Unsigned_Int =>
                              Lower := 0;
                              Upper := Long_Long_Integer (Integer'Last);

                           when SOAP.WSDL.P_Unsigned_Long =>
                              Lower := 0;
                              Upper := Long_Long_Integer (Types.Long'Last);

                           when others =>
                              null;
                        end case;

                        --  Get constraints from the WSDL definition

                        WSDL.Types.Get_Constraint_Integer
                          (Constraints, Lower, L_Set, Upper, U_Set);

                        --  If constraints are found, write them

                        if L_Set or U_Set then
                           L_Range := To_Unbounded_String
                             ((if L_Set
                               then Long_Long_Integer'Image (Lower)
                               else " " & B_Name & "'First"));
                           U_Range := To_Unbounded_String
                             ((if U_Set
                               then Long_Long_Integer'Image (Upper)
                               else " " & B_Name & "'Last"));
                        end if;
                     end;
               end case;
            end;

            Translations := Translations
              & Templates.Assoc ("LOWER_RANGE", L_Range)
              & Templates.Assoc ("UPPER_RANGE", U_Range)
              & Templates.Assoc ("PREDICATE_KIND", Predicate_Kind)
              & Templates.Assoc ("PREDICATE", Predicate)
              & Templates.Assoc ("IS_RANGE", Is_Range);

            --  Routine to convert to base type

            if SOAP.WSDL.Is_Standard (P_Name) then
               Translations := Translations
                 & Templates.Assoc ("ROUTINE_NAME",
                                    SOAP.Utils.No_NS (P_Name) & "_Type");
            else
               Translations := Translations
                 & Templates.Assoc ("ROUTINE_NAME", B_Name);
            end if;

            --  For Types child package

            if SOAP.WSDL.Is_Standard (P_Name) then
               Translations := Translations
                 & Templates.Assoc
                     ("ALIAS_ROUTINE_NAME", SOAP.Utils.No_NS (Name));
            end if;

         else
            Translations := Translations
              & Templates.Assoc
                  ("TYPE_REF", SOAP.Utils.No_NS (Name));

            if SOAP.WSDL.Is_Standard (P_Name) then
               Translations := Translations
                 & Templates.Assoc
                     ("ALIAS_ROUTINE_NAME", SOAP.Utils.No_NS (P_Name));
            end if;
         end if;

         Generate
           (O, To_Lower (To_String (Prefix)) & ".ads",
            Template_Derived_Ads, Translations);

         Insert_Types_Def (O, Template_Derived_Types, Translations);
      end Generate_Derived;

      --------------------------
      -- Generate_Enumeration --
      --------------------------

      procedure Generate_Enumeration
        (Name : String;
         P    : WSDL.Parameters.P_Set)
      is
         use type WSDL.Types.E_Node_Access;

         F_Name : constant String := Format_Name (O, Name);
         Def     : constant WSDL.Types.Definition := WSDL.Types.Find (P.Typ);
         N       : WSDL.Types.E_Node_Access := Def.E_Def;
         Prefix  : Unbounded_String;

         Translations : Templates.Translate_Set :=
                          Templates.Null_Set
                          & Templates.Assoc
                              ("TYPE_REF", WSDL.Types.Name (Def.Ref))
                          & Templates.Assoc ("TYPE_NAME", F_Name);
         E_Name       : Templates.Tag;
         E_Value      : Templates.Tag;
      begin
         Initialize_Types_Package
           (Translations, P, F_Name, False, Prefix);

         while N /= null loop
            E_Name := E_Name & Format_Name (O, To_String (N.Value));
            E_Value := E_Value & To_String (N.Value);
            N := N.Next;
         end loop;

         Translations := Translations
           & Templates.Assoc ("E_NAME", E_Name)
           & Templates.Assoc ("E_VALUE", E_Value);

         Generate
           (O,
            To_Lower (To_String (Prefix)) & ".ads",
            Template_Enum_Ads, Translations);
         Generate
           (O,
            To_Lower (To_String (Prefix)) & ".adb",
            Template_Enum_Adb, Translations);

         Insert_Types_Def (O, Template_Enum_Types, Translations);
      end Generate_Enumeration;

      ------------------------
      -- Generate_Namespace --
      ------------------------

      function Generate_Namespace
        (NS     : SOAP.Name_Space.Object;
         Create : Boolean) return String
      is
         use type SOAP.Name_Space.Object;

         function Gen_Dir (Prefix, Name : String) return String;
         --  Generate a set of directory for each value in Prefix using :, /
         --  and . as directory separator.

         function Gen_Package
           (Prefix, Name : String; Leaf : Boolean) return String;
         --  Generate a packge for Name. If Leaf is true, this is a leaf
         --  package and we do generate the Name_Space variable for this
         --  hierarchy.

         -------------
         -- Gen_Dir --
         -------------

         function Gen_Dir (Prefix, Name : String) return String is
            F : constant Natural := Name'First;
            L : Natural;
         begin
            L := Strings.Fixed.Index
              (Name (F .. Name'Last), Strings.Maps.To_Set (":/."));

            if L = 0 then
               return Gen_Package
                 (Prefix, Name (F .. Name'Last), Leaf => True);
            else
               return Gen_Dir
                 (Gen_Package (Prefix, Name (F .. L - 1), Leaf => False),
                  Name (L + 1 .. Name'Last));
            end if;
         end Gen_Dir;

         -----------------
         -- Gen_Package --
         -----------------

         function Gen_Package
           (Prefix, Name : String; Leaf : Boolean) return String
         is

            function Get_Prefix return String;
            --  Retruns Prefix & '-' if prefix is not empty

            function Get_Name (Name : String) return String;
            --  Returns Name if a valid identifier, prefix with 'n' if number,
            --  and Ada reserved word or some AWS package names. This is to
            --  avoid name clashes.

            --------------
            -- Get_Name --
            --------------

            function Get_Name (Name : String) return String is
               N : constant String := Format_Name (O, Name);
            begin
               if Strings.Fixed.Count
                 (Name, Strings.Maps.Constants.Decimal_Digit_Set) = Name'Length
                 or else SOAP.Utils.Is_Ada_Reserved_Word (Name)
                 or else Name = "soap"
                 or else Name = "aws"
               then
                  return 'n' & N;
               else
                  return N;
               end if;
            end Get_Name;

            ----------------
            -- Get_Prefix --
            ----------------

            function Get_Prefix return String is
            begin
               if Prefix = "" then
                  return "";
               else
                  return Prefix & '-';
               end if;
            end Get_Prefix;

            N    : constant String :=
                     Get_Prefix & Get_Name
                       (Strings.Fixed.Translate
                          (Name,
                           Strings.Maps.To_Mapping ("./:-", "____")));

            T    : Templates.Translate_Set;
         begin
            if Create then
               T := T
                 & Templates.Assoc ("UNIT_NAME", To_Unit_Name (N));

               if Leaf then
                  T := T
                    & Templates.Assoc ("NS_NAME", SOAP.Name_Space.Name (NS))
                    & Templates.Assoc ("NS_VALUE", SOAP.Name_Space.Value (NS));
               end if;

               Generate (O, To_Lower (N) & ".ads", Template_NS_Pkg_Ads, T);
            end if;
            return N;
         end Gen_Package;

      begin
         if NS = SOAP.Name_Space.No_Name_Space
           or else SOAP.Name_Space.Value (NS) = ""
         then
            return Generate_Namespace (SOAP.Name_Space.AWS, True);

         else
            declare
               --  If we have forced the AWS name-space (-n option), use it
               V     : constant String :=
                         (if SOAP.Name_Space.Is_Default_AWS_NS
                          then SOAP.Name_Space.Value (NS)
                          else SOAP.Name_Space.Value (SOAP.Name_Space.AWS));
               First : Positive := V'First;
               Last  : Positive := V'Last;
               K     : Natural;
            begin
               --  Remove http:// prefix if present
               if V (V'First .. V'First + 6) = "http://" then
                  First := First + 7;
               end if;

               --  Remove trailing / if present

               while V (Last) = '/' loop
                  Last := Last - 1;
               end loop;

               K := Strings.Fixed.Index
                 (V (First .. Last), "/", Strings.Backward);

               if K = 0 then
                  return Gen_Dir ("", V (First .. Last));
               else
                  return Gen_Package
                    (Prefix => Gen_Dir ("", V (First .. K - 1)),
                     Name   => V (K + 1 .. Last),
                     Leaf   => True);
               end if;
            end;
         end if;
      end Generate_Namespace;

      ---------------------
      -- Generate_Record --
      ---------------------

      procedure Generate_Record
        (Name      : String;
         Suffix    : String;
         P         : WSDL.Parameters.P_Set;
         Is_Output : Boolean               := False)
      is
         F_Name    : constant String :=
                       Format_Name (O, SOAP.Utils.No_NS (Name) & Suffix);
         Def       : constant WSDL.Types.Definition := WSDL.Types.Find (P.Typ);
         Is_Choice : constant Boolean :=
                       Def.Mode = WSDL.Types.K_Record and then Def.Is_Choice;
         NS        : constant SOAP.Name_Space.Object := WSDL.Types.NS (P.Typ);
         Pck_NS    : constant String :=
                       To_Unit_Name (Generate_Namespace (NS, False));

         R       : WSDL.Parameters.P_Set;
         N       : WSDL.Parameters.P_Set;

         Count   : Natural := 0;

         Prefix             : Unbounded_String;
         Translations       : Templates.Translate_Set;
         Field_Number       : Templates.Tag;
         Field_Comment      : Templates.Tag;
         Field_Array_First  : Templates.Tag;
         Field_Array_Last   : Templates.Tag;
         Field_Array_Length : Templates.Tag;

         R_Decl           : Templates.Tag;
         R_Name           : Templates.Tag;
         R_Kind           : Templates.Tag;
         R_Min            : Templates.Tag;
         R_Max            : Templates.Tag;
         R_Compound_Size  : Templates.Tag;
         R_Type           : Templates.Tag;
         R_Base_Type      : Templates.Tag;
         R_Root_Type      : Templates.Tag;
         R_Root_Type_Kind : Templates.Tag;
         R_Type_Name      : Templates.Tag;
         R_Type_Kind      : Templates.Tag;
         R_Ada_Type       : Templates.Tag;
         R_Q_Name         : Templates.Tag;
         R_NS_Name        : Templates.Tag;
         R_NS_Value       : Templates.Tag;
         R_Elt_NS_Name    : Templates.Tag;
         R_Elt_NS_Value   : Templates.Tag;

      begin
         Initialize_Types_Package (Translations, P, F_Name, Is_Output, Prefix);

         if Is_Output then
            R := P;
         else
            R := P.P;
         end if;

         N := R;

         while N /= null loop
            Generate_Params
              (O, N, R_Decl, R_Name, R_Kind, R_Min, R_Max, R_Compound_Size,
               R_Type, R_Base_Type, R_Root_Type, R_Root_Type_Kind,
               R_Type_Name, R_Type_Kind, R_Ada_Type, R_Q_Name,
               R_NS_Name, R_NS_Value, R_Elt_NS_Name, R_Elt_NS_Value);

            Count := Count + 1;
            Field_Number   := Field_Number & Count;
            Field_Comment  := Field_Comment & N.Doc;

            if N.Mode = WSDL.Types.K_Array then
               Field_Array_First  := Field_Array_First & N.Min;
               Field_Array_Last   := Field_Array_Last & N.Max;
               Field_Array_Length := Field_Array_Length
                 & (if N.Max = Positive'Last
                    then N.Max
                    else 1 + N.Max - N.Min);
            else
               Field_Array_First  := Field_Array_First & "";
               Field_Array_Last   := Field_Array_Last & "";
               Field_Array_Length := Field_Array_Length & "";
            end if;

            N := N.Next;
         end loop;

         Translations := Translations
           & Templates.Assoc ("TYPE_NAME", F_Name)
           & Templates.Assoc ("IS_CHOICE", Is_Choice)
           & Templates.Assoc ("FIELD_COUNT", Count)
           & Templates.Assoc ("FIELD_COMMENT", Field_Comment)
           & Templates.Assoc ("FIELD_NUMBER", Field_Number)
           & Templates.Assoc ("FIELD_ARRAY_FIRST", Field_Array_First)
           & Templates.Assoc ("FIELD_ARRAY_LAST", Field_Array_Last)
           & Templates.Assoc ("FIELD_ARRAY_LENGTH", Field_Array_Length)
           & Templates.Assoc ("NAME_SPACE", SOAP.Name_Space.Name (NS))
           & Templates.Assoc ("TYPE_REF", WSDL.Types.Name (P.Typ))
           & Templates.Assoc ("DOCUMENTATION", P.Doc)

           & Templates.Assoc ("RF_DECL_NAME", R_Decl)
           & Templates.Assoc ("RF_NAME", R_Name)
           & Templates.Assoc ("RF_KIND", R_Kind)
           & Templates.Assoc ("RF_MIN", R_Min)
           & Templates.Assoc ("RF_MAX", R_Max)
           & Templates.Assoc ("RF_COMPOUND_SIZE", R_Compound_Size)
           & Templates.Assoc ("RF_TYPE", R_Type)
           & Templates.Assoc ("RF_BASE_TYPE", R_Base_Type)
           & Templates.Assoc ("RF_ROOT_TYPE", R_Root_Type)
           & Templates.Assoc ("RF_ROOT_TYPE_KIND", R_Root_Type_Kind)
           & Templates.Assoc ("RF_TYPE_NAME", R_Type_Name)
           & Templates.Assoc ("RF_TYPE_KIND", R_Type_Kind)
           & Templates.Assoc ("RF_ADA_TYPE", R_Ada_Type)
           & Templates.Assoc ("RF_Q_NAME", R_Q_Name)
           & Templates.Assoc ("RF_NS_NAME", R_NS_Name)
           & Templates.Assoc ("RF_NS_VALUE", R_NS_Value)
           & Templates.Assoc ("RF_ELT_NS_NAME", R_Elt_NS_Name)
           & Templates.Assoc ("RF_ELT_NS_VALUE", R_Elt_NS_Value);

         --  Is types are to be reused from an Ada spec ?

         if Types_Spec (O) = ""
              or else
            (Is_Simple_Wrapped_Parameter (O, Input) and then P = Input)
              or else
            (Is_Simple_Wrapped_Parameter (O, Output) and then P = Output)
         then
            Translations := Translations
              & Templates.Assoc ("SIMPLE_WRAPPED_PARAMETER", True);

         else
            Translations := Translations
              & Templates.Assoc ("SIMPLE_WRAPPED_PARAMETER", False);
         end if;

         if not NS_Generated.Contains (SOAP.Name_Space.Name (NS)) then
            Translations := Translations
              & Templates.Assoc ("NAME_SPACE_PACKAGE", Pck_NS);
            NS_Generated.Insert (SOAP.Name_Space.Name (NS));
         end if;

         Generate
           (O,
            To_Lower (To_String (Prefix)) & ".ads",
            Template_Record_Ads, Translations);
         Generate
           (O,
            To_Lower (To_String (Prefix)) & ".adb",
            Template_Record_Adb, Translations);

         Insert_Types_Def (O, Template_Record_Types, Translations);
      end Generate_Record;

      ---------------------
      --  Get_References --
      ---------------------

      procedure Get_References
        (Unit_List   : in out AWS.Templates.Tag;
         P           : WSDL.Parameters.P_Set;
         For_Derived : Boolean := False)
      is
         procedure Output_Refs (Def : WSDL.Types.Definition; Gen : Boolean);
         --  Recursivelly record with/use clauses for derived types

         Generated : String_Store.Set;
         --  We must ensure that we do not generate the same with clause twice.
         --  This can happen with derived types on a record.

         -----------------
         -- Output_Refs --
         -----------------

         procedure Output_Refs (Def : WSDL.Types.Definition; Gen : Boolean) is
            Q_Name : constant String := WSDL.Types.Name (Def.Ref, NS => True);
            F_Name : constant String :=
                       Format_Name (O, WSDL.Types.Name (Def.Ref));
            Prefix : constant String :=
                       Generate_Namespace (WSDL.Types.NS (Def.Ref), False);
         begin
            --  For array we want to output references even for standard types
            --  as we have the generated safe-access circuitry.

            if Gen and then not Generated.Contains (Q_Name) then
               if SOAP.WSDL.Is_Standard (WSDL.Types.Name (Def.Ref)) then
                  --  We want here to add a reference to the standard type but
                  --  also generate the corresponding root-package with the
                  --  needed name-space.

                  Unit_List := Unit_List
                    & To_Unit_Name
                       (Generate_Namespace (WSDL.Types.NS (Def.Ref), True));
               else
                  Unit_List := Unit_List
                     & (To_Unit_Name (Prefix) & '.' & F_Name & "_Type_Pkg");
               end if;

               Generated.Insert (Q_Name);
            end if;

            if Def.Mode = WSDL.Types.K_Derived
              and then not SOAP.WSDL.Is_Standard (WSDL.Types.Name (Def.Parent))
            then
               Output_Refs (WSDL.Types.Find (Def.Parent), True);
            end if;
         end Output_Refs;

         N : WSDL.Parameters.P_Set := P;

      begin
         while N /= null loop
            Output_Refs (WSDL.Types.Find (N.Typ), not For_Derived);

            --  If we are not handling a compound type, only reference the root
            --  type.

            exit when For_Derived;

            N := N.Next;
         end loop;
      end Get_References;

      -----------------
      -- Get_Routine --
      -----------------

      function Get_Routine (P : WSDL.Parameters.P_Set) return String is
         Def    : constant WSDL.Types.Definition := WSDL.Types.Find (P.Typ);
         T_Name : constant String := WSDL.Types.Name (P.Typ);
      begin
         case P.Mode is
            when WSDL.Types.K_Simple =>
               return SOAP.WSDL.Get_Routine (SOAP.WSDL.To_Type (T_Name));

            when WSDL.Types.K_Derived =>
               return SOAP.WSDL.Get_Routine (SOAP.WSDL.To_Type (T_Name));

            when WSDL.Types.K_Enumeration =>
               return SOAP.WSDL.Get_Routine (SOAP.WSDL.P_String);

            when WSDL.Types.K_Array =>
               declare
                  E_Ref  : constant WSDL.Types.Definition :=
                             WSDL.Types.Find (Def.E_Type);
                  E_Type : constant String := WSDL.Types.Name (Def.E_Type);
                  Q_Type : constant String :=
                             SOAP.Utils.To_Name
                               (WSDL.Types.Name (Def.E_Type, True));
               begin
                  if SOAP.WSDL.Is_Standard (E_Type) then
                     return SOAP.WSDL.Get_Routine
                       (SOAP.WSDL.To_Type (E_Type), Constrained => True);

                  elsif E_Ref.Mode = WSDL.Types.K_Derived then
                     return "To_" & Format_Name (O, Q_Type) & "_Type";

                  else
                     return "To_" & Format_Name (O, E_Type) & "_Type";
                  end if;
               end;

            when WSDL.Types.K_Record =>
               return "To_" & Type_Name (O, P);
         end case;
      end Get_Routine;

      ------------------------------
      -- Initialize_Types_Package --
      ------------------------------

      procedure Initialize_Types_Package
        (Translations : in out Templates.Translate_Set;
         P            : WSDL.Parameters.P_Set;
         Name         : String;
         Output       : Boolean;
         Prefix       : out Unbounded_String;
         Def          : WSDL.Types.Definition := WSDL.Types.No_Definition;
         Regen        : Boolean := False)
      is
         use type WSDL.Types.Definition;
         use WSDL.Parameters;

         F_Name      : constant String := Name & "_Pkg";
         Q_Type_Name : Unbounded_String;
         Unit_List   : Templates.Tag;
      begin
         Prefix := To_Unbounded_String
           (Generate_Namespace
              (WSDL.Types.NS
                (if Def = WSDL.Types.No_Definition
                 then P.Typ
                 else Def.Ref), True) & '-' & F_Name);

         --  Add references into the main types package

         if not Regen then
            Add_TagV
              (O.Type_S_Trans,
               "WITHED_UNITS", To_Unit_Name (To_String (Prefix)));
         end if;

         if Def.Mode /= WSDL.Types.K_Simple then
            Q_Type_Name :=
              To_Unbounded_String
                (WSDL.Types.Name
                   ((if Def = WSDL.Types.No_Definition
                    then P.Typ
                    else Def.Ref), NS => True));
         end if;

         --  Either a compound type or an anonymous returned compound type

         if Output then
            Get_References (Unit_List, P);
         elsif P.Mode in WSDL.Types.Compound_Type then
            Get_References (Unit_List, P.P);
         end if;

         if Def.Mode = WSDL.Types.K_Derived then
            if SOAP.WSDL.Is_Standard (WSDL.Types.Name (Def.Parent)) then
               Unit_List := Unit_List
                 & To_Unit_Name
                     (Generate_Namespace (WSDL.Types.NS (Def.Parent), True));
            else
               Unit_List := Unit_List
                 & (To_Unit_Name
                     (Generate_Namespace (WSDL.Types.NS (Def.Parent), False))
                    & '.' & WSDL.Types.Name (Def.Parent) & "_Type_Pkg");
            end if;
         end if;

         Translations := Translations
           & Templates.Assoc ("TYPE_SPEC", Types_Spec (O))
           & Templates.Assoc ("UNIT_NAME", To_Unit_Name (To_String (Prefix)))
           & Templates.Assoc ("Q_TYPE_NAME", Q_Type_Name)
           & Templates.Assoc ("WITHED_UNITS", Unit_List);
      end Initialize_Types_Package;

      ----------------------
      -- Is_Inside_Record --
      ----------------------

      function Is_Inside_Record (Name : String) return Boolean is
         In_Record : Boolean := False;

         procedure Check_Record
           (P_Set : WSDL.Parameters.P_Set;
            Mode  : in out Boolean);
         --  Checks all record fields for Name

         procedure Check_Parameters
           (P_Set : WSDL.Parameters.P_Set);
         --  Checks P_Set for Name declared inside a record

         ----------------------
         -- Check_Parameters --
         ----------------------

         procedure Check_Parameters
           (P_Set : WSDL.Parameters.P_Set)
         is
            P : WSDL.Parameters.P_Set := P_Set;
         begin
            while P /= null loop
               if P.Mode = WSDL.Types.K_Record then
                  Check_Record (P.P, In_Record);

               elsif P.Mode = WSDL.Types.K_Array then
                  --  Recursively check for every array parameters. This is
                  --  to handle the case where an array has a parameter which
                  --  is a record containing the type Name.
                  Check_Parameters (P.P);
               end if;

               P := P.Next;
            end loop;
         end Check_Parameters;

         ------------------
         -- Check_Record --
         ------------------

         procedure Check_Record
           (P_Set : WSDL.Parameters.P_Set;
            Mode  : in out Boolean)
         is
            P : WSDL.Parameters.P_Set := P_Set;
         begin
            while P /= null loop
               if P.Mode = WSDL.Types.K_Array
                 and then WSDL.Types.Name (P.Typ) = Name
               then
                  Mode := True;
                  return;
               end if;

               if P.Mode in WSDL.Types.Compound_Type then
                  Check_Record (P.P, Mode);
               end if;

               P := P.Next;
            end loop;
         end Check_Record;

      begin
         Check_Parameters (Input);
         Check_Parameters (Output);

         return In_Record;
      end Is_Inside_Record;

      ------------------
      -- Output_Types --
      ------------------

      procedure Output_Types (P : WSDL.Parameters.P_Set) is
         N : WSDL.Parameters.P_Set := P;
      begin
         while N /= null loop
            declare
               T_Name : constant String := WSDL.Types.Name (N.Typ);
            begin
               case N.Mode is
                  when WSDL.Types.K_Simple =>
                     null;

                  when WSDL.Types.K_Derived =>
                     declare

                        procedure Generate (Def : WSDL.Types.Definition);
                        --  Generate all definitions for the derived types in
                        --  the right order of reference.

                        --------------
                        -- Generate --
                        --------------

                        procedure Generate (Def : WSDL.Types.Definition) is
                           use type SOAP.Name_Space.Object;

                           T_Name : constant String :=
                                      WSDL.Types.Name (Def.Ref, True);
                        begin
                           if WSDL.Types.NS (Def.Ref) /=
                             SOAP.Name_Space.XSD
                           then
                              Generate (WSDL.Types.Find (Def.Parent));

                              if not Name_Set.Exists (T_Name) then
                                 Name_Set.Add (T_Name);

                                 Generate_Derived (T_Name, Def, N);
                              end if;
                           end if;
                        end Generate;

                     begin
                        Generate (WSDL.Types.Find (N.Typ));
                     end;

                  when WSDL.Types.K_Enumeration =>
                     if not Name_Set.Exists (T_Name) then
                        Name_Set.Add (T_Name);

                        Generate_Enumeration (T_Name & "_Type", N);
                     end if;

                  when WSDL.Types.K_Array =>
                     Output_Types (N.P);

                     declare
                        Regen : Boolean;
                     begin
                        if not Name_Set.Exists (T_Name)
                          or else Is_Inside_Record (T_Name)
                        then
                           if Name_Set.Exists (T_Name)
                             and then Is_Inside_Record (T_Name)
                           then
                              --  We force the regeneration of the array
                              --  definition when it is inside a record to
                              --  be sure that we have a safe access generated.
                              Regen := True;
                           else
                              Regen := False;
                              Name_Set.Add (T_Name);
                           end if;

                           Generate_Array (T_Name & "_Type", N, Regen);
                        end if;
                     end;

                  when WSDL.Types.K_Record =>
                     Output_Types (N.P);

                     if not Name_Set.Exists (T_Name) then
                        Name_Set.Add (T_Name);

                        Generate_Record
                          (WSDL.Types.Name (N.Typ, True), "_Type", N);
                     end if;
               end case;
            end;

            N := N.Next;
         end loop;
      end Output_Types;

      -----------------
      -- Set_Routine --
      -----------------

      function Set_Routine (P : WSDL.Parameters.P_Set) return String is
         Def    : constant WSDL.Types.Definition :=  WSDL.Types.Find (P.Typ);
         T_Name : constant String := WSDL.Types.Name (P.Typ);
      begin
         case P.Mode is
            when WSDL.Types.K_Simple =>
               return SOAP.WSDL.Set_Routine
                 (SOAP.WSDL.To_Type (T_Name), Constrained => True);

            when WSDL.Types.K_Derived =>
               return SOAP.WSDL.Set_Routine
                 (WSDL.Types.Name (Def.Parent), Constrained => True);

            when WSDL.Types.K_Enumeration =>
               return SOAP.WSDL.Set_Routine
                 (SOAP.WSDL.P_String, Constrained => True);

            when WSDL.Types.K_Array =>
               declare
                  E_Type : constant String := WSDL.Types.Name (Def.E_Type);
               begin
                  if SOAP.WSDL.Is_Standard (E_Type) then
                     return SOAP.WSDL.Set_Routine
                       (SOAP.WSDL.To_Type (E_Type), Constrained => True);
                  else
                     return "To_SOAP_Object";
                  end if;
               end;

            when WSDL.Types.K_Record =>
               return "To_SOAP_Object";
         end case;
      end Set_Routine;

      L_Proc : constant String := Format_Name (O, Proc);

   begin
      Output_Types (Input);

      Output_Types (Output);

      Output_Schema_Definition
        (O,
         Key   => '@' & To_String (O.Prefix) & W_Name & ".encoding",
         Value => SOAP.Types.Encoding_Style'Image
                    (O.Encoding (WSDL.Parser.Input)));

      Output_Schema_Definition
        (O,
         Key   => '@' & To_String (O.Prefix) & W_Name & "Response.encoding",
         Value =>
           SOAP.Types.Encoding_Style'Image (O.Encoding (WSDL.Parser.Output)));

      Output_Schema_Definition
        (O,
         Key   => '@' & To_String (O.Prefix)
                  & SOAP.Utils.No_NS (W_Name) & ".encoding",
         Value => SOAP.Types.Encoding_Style'Image
                    (O.Encoding (WSDL.Parser.Input)));

      Output_Schema_Definition
        (O,
         Key   => '@' & To_String (O.Prefix)
                  & SOAP.Utils.No_NS (W_Name) & "Response.encoding",
         Value =>
           SOAP.Types.Encoding_Style'Image (O.Encoding (WSDL.Parser.Output)));

      if Output /= null then
         --  Something in the SOAP procedure output

         Output_Schema_Definition
           (O,
            Key   => '@' & To_String (Output.Name) & ".encoding",
            Value =>
              SOAP.Types.Encoding_Style'Image
                (O.Encoding (WSDL.Parser.Output)));

         if Output.Next = null then
            --  A single parameter

            if Output.Mode /= WSDL.Types.K_Simple then
               declare
                  Def : constant WSDL.Types.Definition :=
                          WSDL.Types.Find (Output.Typ, False);
                  T   : Templates.Translate_Set;
               begin
                  T := T
                    & Templates.Assoc
                        ("PROC", L_Proc)
                    & Templates.Assoc
                        ("QUALIFIED_NAME",
                         Format_Name
                           (O,
                            SOAP.Utils.To_Name
                              (WSDL.Types.Name
                                   (Output.Typ,
                                    Def.Mode = WSDL.Types.K_Derived))))
                    & Templates.Assoc
                        ("RESULT_IS_ARRAY", Output.Mode = WSDL.Types.K_Array);

                  Insert_Types_Def (O, Template_Stub_Types_Ads, T);
               end;
            end if;

         else
            --  Multiple parameters in the output, generate a record in this
            --  case.

            Generate_Record (L_Proc, "_Result", Output, Is_Output => True);
         end if;
      end if;
   end Put_Types;

   -----------
   -- Quiet --
   -----------

   procedure Quiet (O : in out Object) is
   begin
      O.Quiet := True;
   end Quiet;

   -----------------
   -- Result_Type --
   -----------------

   function Result_Type
     (O      : Object;
      Proc   : String;
      Output : WSDL.Parameters.P_Set) return String
   is
      use type WSDL.Types.Kind;

      L_Proc : constant String := Format_Name (O, Proc);
   begin
      if WSDL.Parameters.Length (Output) = 1
        and then Output.Mode = WSDL.Types.K_Simple
      then
         return SOAP.WSDL.To_Ada
           (SOAP.WSDL.To_Type (WSDL.Types.Name (Output.Typ)));
      else
         return L_Proc & "_Result";
      end if;
   end Result_Type;

   ----------------
   -- Set_Prefix --
   ----------------

   procedure Set_Prefix (O : in out Object; Prefix : String) is
   begin
      O.Prefix := To_Unbounded_String (Prefix);
   end Set_Prefix;

   ---------------
   -- Set_Proxy --
   ---------------

   procedure Set_Proxy
     (O : in out Object; Proxy, User, Password : String) is
   begin
      O.Proxy  := To_Unbounded_String (Proxy);
      O.P_User := To_Unbounded_String (User);
      O.P_Pwd  := To_Unbounded_String (Password);
   end Set_Proxy;

   ------------------
   -- Set_Timeouts --
   ------------------

   procedure Set_Timeouts
     (O        : in out Object;
      Timeouts : Client.Timeouts_Values) is
   begin
      O.Timeouts := Timeouts;
   end Set_Timeouts;

   ----------
   -- Skel --
   ----------

   package body Skel is separate;

   ----------------
   -- Specs_From --
   ----------------

   procedure Specs_From (O : in out Object; Spec : String) is
   begin
      O.Spec := To_Unbounded_String (Spec);
   end Specs_From;

   -------------------
   -- Start_Service --
   -------------------

   overriding procedure Start_Service
     (O                  : in out Object;
      Name               : String;
      Root_Documentation : String;
      Documentation      : String;
      Location           : String)
   is
      use type Client.Timeouts_Values;
      use type Templates.Translate_Set;

      U_Name : constant String := To_Unit_Name (Format_Name (O, Name));

      procedure Generate_Main (Filename : String);
      --  Generate the main server's procedure. Either the file exists and is
      --  a template use it to generate the main otherwise just generate a
      --  standard main procedure.

      function Timeout_Image (Timeout : Duration) return String;

      -------------------
      -- Generate_Main --
      -------------------

      procedure Generate_Main (Filename : String) is
         L_Filename : constant String :=
                        Characters.Handling.To_Lower (Filename);
      begin
         declare
            T : Templates.Translate_Set;
         begin
            T := T
              & Templates.Assoc ("SOAP_SERVICE", U_Name)
              & Templates.Assoc ("UNIT_NAME", To_Unit_Name (Filename));

            Generate (O, L_Filename & ".adb", Template_Main_Adb, T);
         end;
      end Generate_Main;

      -------------------
      -- Timeout_Image --
      -------------------

      function Timeout_Image (Timeout : Duration) return String is
      begin
         if Timeout = Duration'Last then
            return "Duration'Last";
         else
            return AWS.Utils.Significant_Image (Timeout, 3);
         end if;
      end Timeout_Image;

      LL_Name : constant String :=
                  Characters.Handling.To_Lower (Format_Name (O, Name));

   begin
      O.Type_S_Trans := O.Type_S_Trans
        & Templates.Assoc ("UNIT_NAME", U_Name);

      O.Type_B_Trans := O.Type_B_Trans
        & Templates.Assoc ("UNIT_NAME", U_Name);

      O.Root_S_Trans := O.Root_S_Trans
        & Templates.Assoc ("UNIT_NAME", U_Name)
        & Templates.Assoc ("ROOT_DOCUMENTATION", Root_Documentation);

      O.Stub_S_Trans := O.Stub_S_Trans
        & Templates.Assoc ("ROOT_DOCUMENTATION", Root_Documentation);

      O.Skel_S_Trans := O.Skel_S_Trans
        & Templates.Assoc ("ROOT_DOCUMENTATION", Root_Documentation);

      O.Location := To_Unbounded_String (Get_Endpoint (O, Location));

      Validate_Location : declare
         Loc : constant String := To_String (O.Location);
      begin
         --  Validate location

         if Loc'Length < 8
           or else
             (Loc (Loc'First .. Loc'First + 6) /= "http://"
              and then Loc (Loc'First .. Loc'First + 7) /= "https://")
         then
            raise WSDL.Parser.WSDL_Error
              with  "location is not a valid end-point, "
              & "consider using option -e";
         end if;
      end Validate_Location;

      if not O.Quiet then
         Text_IO.New_Line;
         Text_IO.Put_Line ("Service " & Name);
         Text_IO.Put_Line ("   " & Root_Documentation);
      end if;

      if O.Timeouts /= Client.No_Timeout then
         O.Root_S_Trans := O.Root_S_Trans
           & Templates.Assoc
             ("CONNECT_TIMEOUT",
              Timeout_Image (Client.Connect_Timeout (O.Timeouts)))
           & Templates.Assoc
             ("SEND_TIMEOUT",
              Timeout_Image (Client.Send_Timeout (O.Timeouts)))
           & Templates.Assoc
             ("RECEIVE_TIMEOUT",
              Timeout_Image (Client.Receive_Timeout (O.Timeouts)))
           & Templates.Assoc
             ("RESPONSE_TIMEOUT",
              Timeout_Image (Client.Response_Timeout (O.Timeouts)));
      end if;

      O.Root_S_Trans := O.Root_S_Trans
        & Templates.Assoc ("END_POINT", Get_Endpoint (O, Location));

      --  Add namespaces in schema

      Output_Schema_Definition
        (O,
         Key   => SOAP.Name_Space.Value (O.xsd),
         Value => SOAP.Utils.No_NS (SOAP.Name_Space.Name (O.xsd)));
      Output_Schema_Definition
        (O,
         Key   => SOAP.Name_Space.Value (O.xsi),
         Value => SOAP.Utils.No_NS (SOAP.Name_Space.Name (O.xsi)));
      Output_Schema_Definition
        (O,
         Key   => SOAP.Name_Space.Value (O.env),
         Value => SOAP.Utils.No_NS (SOAP.Name_Space.Name (O.env)));
      Output_Schema_Definition
        (O,
         Key   => SOAP.Name_Space.Value (O.enc),
         Value => SOAP.Utils.No_NS (SOAP.Name_Space.Name (O.enc)));

      --  Then the user's name-spaces

      declare
         procedure Write_NS (Key, Value : String);

         --------------
         -- Write_NS --
         --------------

         procedure Write_NS (Key, Value : String) is
         begin
            Output_Schema_Definition (O, Key, Value);
         end Write_NS;

      begin
         SOAP.WSDL.Name_Spaces.Iterate (Write_NS'Access);
      end;

      --  The WSDL document

      if O.WSDL_File /= Null_Unbounded_String then
         declare
            File   : Text_IO.File_Type;
            Buffer : String (1 .. 1_024);
            Last   : Natural;
         begin
            Text_IO.Open (File, Text_IO.In_File, To_String (O.WSDL_File));

            while not Text_IO.End_Of_File (File) loop
               Text_IO.Get_Line (File, Buffer, Last);
               Add_TagV (O.Root_S_Trans, "WSDL", Buffer (1 .. Last));
            end loop;
         end;
      end if;

      Generate (O, LL_Name & ".ads", Template_Root_Ads, O.Root_S_Trans);

      O.Unit := To_Unbounded_String (U_Name);

      --  Stubs

      if O.Gen_Stub then
         Stub.Start_Service
           (O, Name, Root_Documentation, Documentation, Location);
      end if;

      --  Skeletons

      if O.Gen_Skel then
         Skel.Start_Service
           (O, Name, Root_Documentation, Documentation, Location);
      end if;

      --  Callbacks

      if O.Gen_CB then
         CB.Start_Service
           (O, Name, Root_Documentation, Documentation, Location);
      end if;

      --  Main

      if O.Main /= Null_Unbounded_String then
         Generate_Main (To_String (O.Main));
      end if;
   end Start_Service;

   ----------
   -- Stub --
   ----------

   package body Stub is separate;

   ------------------
   -- To_Unit_Name --
   ------------------

   function To_Unit_Name (Filename : String) return String is
   begin
      return Strings.Fixed.Translate
        (Filename, Strings.Maps.To_Mapping ("-", "."));
   end To_Unit_Name;

   ------------
   -- Traces --
   ------------

   procedure Traces (O : in out Object) is
   begin
      O.Traces := True;
   end Traces;

   ---------------
   -- Type_Name --
   ---------------

   function Type_Name
     (O : Object;
      N : WSDL.Parameters.P_Set) return String
   is
      T_Name : constant String := WSDL.Types.Name (N.Typ);
      Q_Name : constant String :=
                 SOAP.Utils.To_Name (WSDL.Types.Name (N.Typ, True));
   begin
      case N.Mode is
         when WSDL.Types.K_Simple =>
            --  This routine is called only for SOAP object in records
            --  or arrays.
            return SOAP.WSDL.To_Ada
              (SOAP.WSDL.To_Type (T_Name), Constrained => True);

         when WSDL.Types.K_Derived =>
            return Format_Name (O, Q_Name) & "_Type";

         when WSDL.Types.K_Enumeration =>
            return Format_Name (O, T_Name) & "_Type";

         when WSDL.Types.K_Array =>
            if O.Sp then
               return Format_Name (O, T_Name) & "_Type_Safe_Access";
            else
               return Format_Name (O, T_Name) & "_Type";
            end if;

         when WSDL.Types.K_Record =>
            return Format_Name (O, T_Name) & "_Type";
      end case;
   end Type_Name;

   ----------------
   -- Types_From --
   ----------------

   procedure Types_From (O : in out Object; Spec : String) is
   begin
      O.Types_Spec := To_Unbounded_String (To_Unit_Name (Spec));
   end Types_From;

   ----------------
   -- Types_Spec --
   ----------------

   function Types_Spec
     (O           : Object;
      With_Clause : Boolean := False) return String
   is
      Prefix : constant String :=
                 (if With_Clause then "" else "Standard.");
   begin
      if O.Types_Spec /= Null_Unbounded_String then
         return Prefix & To_String (O.Types_Spec);
      elsif O.Spec /= Null_Unbounded_String then
         return Prefix & To_String (O.Spec);
      else
         return "";
      end if;
   end Types_Spec;

   ---------------
   -- WSDL_File --
   ---------------

   procedure WSDL_File (O : in out Object; Filename : String) is
   begin
      O.WSDL_File := To_Unbounded_String (Filename);
   end WSDL_File;

end WSDL2AWS.Generator;
