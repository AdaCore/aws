------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2003-2020, AdaCore                     --
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

with Ada.Calendar;
with Ada.Characters.Handling;
with Ada.Containers.Indefinite_Ordered_Sets;
with Ada.Strings.Fixed;
with Ada.Strings.Maps.Constants;
with Ada.Text_IO;

with GNAT.Calendar.Time_IO;

with AWS.Containers.Key_Value;
with AWS.Containers.String_Vectors;
with AWS.Templates;
with AWS.Utils;

with SOAP.Types;
with SOAP.Utils;
with SOAP.WSDL.Name_Spaces;
with SOAP.WSDL.Schema;

with WSDL2AWS.WSDL.Types;

package body WSDL2AWS.Generator is

   use Ada;

   package String_Store is
     new Ada.Containers.Indefinite_Ordered_Sets (String);

   function Format_Name (O : Object; Name : String) return String;
   --  Returns Name formated with the Ada style if O.Ada_Style is true and
   --  Name unchanged otherwise.

   procedure Put_File_Header (O : Object; File : Text_IO.File_Type);
   --  Add a standard file header into file

   procedure Put_Types_Header_Spec
     (O         : Object;
      File      : Text_IO.File_Type;
      Unit_Name : String;
      Elab_Body : Boolean := False;
      Is_NS     : Boolean := False);
   --  Put standard header for types body packages

   procedure Put_Types_Header_Body
     (O : Object; File : Text_IO.File_Type; Unit_Name : String);
   --  Put standard header for types spec packages

   procedure Put_Types
     (O          : Object;
      Proc       : String;
      SOAPAction : String;
      Input      : WSDL.Parameters.P_Set;
      Output     : WSDL.Parameters.P_Set);
   --  This must be called to create the data types for composite objects

   type Header_Mode is
     (Stub_Spec, Stub_Body,     -- URL based stub spec/body
      C_Stub_Spec, C_Stub_Body, -- Connection based stub spec/body
      Skel_Spec, Skel_Body);    -- skeleton spec/body

   subtype Stub_Header is Header_Mode range Stub_Spec .. C_Stub_Body;
   subtype Con_Stub_Header is Header_Mode range C_Stub_Spec .. C_Stub_Body;

   procedure Put_Header
     (File   : Text_IO.File_Type;
      O      : Object;
      Proc   : String;
      Input  : WSDL.Parameters.P_Set;
      Output : WSDL.Parameters.P_Set;
      Mode   : Header_Mode);
   --  Output procedure header into File. The terminating ';' or 'is' is
   --  outputed depending on Spec value. If Mode is in Con_Stub_Header the
   --  connection based spec is generated, otherwise it is the endpoint based.

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

   procedure Header_Box
     (O    : Object;
      File : Text_IO.File_Type;
      Name : String);
   --  Generate header box

   function To_Unit_Name (Filename : String) return String;
   --  Returns the unit name given a filename following the GNAT
   --  naming scheme.

   type Elab_Pragma is (Off, Single, Children);
   --  Off      - no pragma Elaborate
   --  Single   - a single pragma for the unit
   --  Children - a pragma for each child unit

   procedure With_Unit
     (File       : Text_IO.File_Type;
      Name       : String;
      Elab       : Elab_Pragma := Single;
      Use_Clause : Boolean := False);
   --  Output a with clause for unit Name, also output a use clause if
   --  Use_Clause is set. A pragma Elaborate_All is issued for this unit if
   --  Elab is set.

   procedure Close_File (File : in out Text_IO.File_Type);
   --  Close given files and reset the Withed_Unit set for this file

   procedure Output_Comment
     (File    : Text_IO.File_Type;
      Comment : String;
      Indent  : Natural);
   --  Ouput Comment into File wrapped with 80 characters

   procedure Output_Schema_Definition (Key, Value : String);
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

   Root     : Text_IO.File_Type; -- Parent packages
   Type_Ads : Text_IO.File_Type; -- Child with all type definitions
   Type_Adb : Text_IO.File_Type; -- Corresponding body with schema definition
   Tmp_Ads  : Text_IO.File_Type; -- Temp file for spec types
   Stub_Ads : Text_IO.File_Type; -- Child with client interface
   Stub_Adb : Text_IO.File_Type;
   Skel_Ads : Text_IO.File_Type; -- Child with server interface
   Skel_Adb : Text_IO.File_Type;
   CB_Ads   : Text_IO.File_Type; -- Child with all callback routines
   CB_Adb   : Text_IO.File_Type;

   Withed_Unit : AWS.Containers.String_Vectors.Vector;
   --  List of withed unit, used to avoid duplicate

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

   --------
   -- CB --
   --------

   package body CB is separate;

   ----------------
   -- Close_File --
   ----------------

   procedure Close_File (File : in out Text_IO.File_Type) is
      Name : constant String := Text_IO.Name (File);
      K    : Positive := 1;
   begin
      Text_IO.Close (File);

      while K <= Withed_Unit.Last_Index loop
         if Strings.Fixed.Index (Withed_Unit (K), Name) /= 0 then
            Withed_Unit.Delete (K);
         else
            K := K + 1;
         end if;
      end loop;
   end Close_File;

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
      Name : String)
   is
      U_Name : constant String := To_Unit_Name (Format_Name (O, Name));
      Buffer : String (1 .. 512);
      Last   : Natural;
   begin
      --  Root

      Text_IO.New_Line (Root);
      Text_IO.Put_Line (Root, "end " & U_Name & ";");

      Close_File (Root);

      --  Types

      --  Copy Tmp_Ads into Type_Ads

      Text_IO.Reset (Tmp_Ads, Text_IO.In_File);

      while not Text_IO.End_Of_File (Tmp_Ads) loop
         Text_IO.Get_Line (Tmp_Ads, Buffer, Last);
         Text_IO.Put_Line (Type_Ads, Buffer (1 .. Last));
      end loop;

      Close_File (Tmp_Ads);

      Text_IO.New_Line (Type_Ads);
      Text_IO.Put_Line (Type_Ads, "end " & U_Name & ".Types;");

      Close_File (Type_Ads);

      --  Generate binding style information

      Output_Schema_Definition
        (Key   => "@binding.style",
         Value => SOAP.WSDL.Schema.Binding_Style'Image (O.Style));
      Text_IO.New_Line (Type_Adb);

      --  Generate the Schema information

      Text_IO.Put_Line (Type_Adb, "   --  Definitions for derived types");

      for C in WSDL.Types.Get_Schema_Definition.Iterate loop
         Output_Schema_Definition
           (Key   => AWS.Containers.Key_Value.Key (C),
            Value => AWS.Containers.Key_Value.Element (C));
      end loop;

      Text_IO.Put_Line (Type_Adb, "end " & U_Name & ".Types;");

      Close_File (Type_Adb);

      --  Stub

      if O.Gen_Stub then
         Stub.End_Service (O, Name);
         Close_File (Stub_Ads);
         Close_File (Stub_Adb);
      end if;

      --  Skeleton

      if O.Gen_Skel then
         Skel.End_Service (O, Name);
         Close_File (Skel_Ads);
         Close_File (Skel_Adb);
      end if;

      --  Callbacks

      if O.Gen_CB then
         CB.End_Service (O, Name);
         Close_File (CB_Ads);
         Close_File (CB_Adb);
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

   ----------------
   -- Header_Box --
   ----------------

   procedure Header_Box
     (O    : Object;
      File : Text_IO.File_Type;
      Name : String)
   is
      pragma Unreferenced (O);
   begin
      Text_IO.Put_Line
        (File, "   " & String'(1 .. 6 + Name'Length => '-'));
      Text_IO.Put_Line
        (File, "   -- " & Name & " --");
      Text_IO.Put_Line
        (File, "   " & String'(1 .. 6 + Name'Length => '-'));
   end Header_Box;

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
      use type WSDL.Types.Kind;
      use all type SOAP.WSDL.Parameter_Type;
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

      procedure Generate_Call_Signature (P : WSDL.Parameters.P_Set);
      --  Generate a call signature for Proc. This is needed to be able to map
      --  this signature to the corresponding SOAP operation when using the
      --  Document style binding. The signature is the key with the following
      --  format: '@' & <param1> & [:<param2>]

      procedure Generate_Schema (Prefix : String; P : WSDL.Parameters.P_Set);
      --  Generate the fully qualified name for the parameters. This is needed
      --  for the document/literal binding to match the payload with the
      --  corresponding data type.

      -----------------------------
      -- Generate_Call_Signature --
      -----------------------------

      procedure Generate_Call_Signature (P : WSDL.Parameters.P_Set) is
         use type WSDL.Parameters.P_Set;

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
           (To_String (Sig), To_String (O.Prefix) & Proc);
      end Generate_Call_Signature;

      ---------------------
      -- Generate_Schema --
      ---------------------

      procedure Generate_Schema (Prefix : String; P : WSDL.Parameters.P_Set) is

         use type WSDL.Parameters.P_Set;

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
               Output_Schema_Definition (Name & "@is_a", "@record");
            else
               Output_Schema_Definition (Name & "@is_a", "@array");
            end if;

            if P.P /= null then
               Output_Schema_Definition (Q_Name, WSDL.Types.Name (P.P.Typ));
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
            Output_Schema_Definition (Name & "@is_a", "@record");

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
              (Name & "@is_a", WSDL.Types.Name (P.Typ, True));
            Output_Schema_Definition
              (Name & "@is_a", WSDL.Types.Name (P.Typ, False));
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

   begin
      if not O.Quiet then
         Text_IO.Put_Line ("   > " & Proc);
      end if;

      Put_Types (O, Proc, Wrapper_Name, Input, Output);

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

      Text_IO.New_Line (Type_Adb);

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

   --------------------
   -- Output_Comment --
   --------------------

   procedure Output_Comment
     (File    : Text_IO.File_Type;
      Comment : String;
      Indent  : Natural)
   is
      use Ada.Strings.Fixed;
      Max_Len : constant Natural := 75 - 4 - Indent;
      F       : Positive := Comment'First;
      L       : Positive := F;
   begin
      while F < Comment'Last loop
         L := Positive'Min (Comment'Last, F + Max_Len);

         if L < Comment'Last then
            while L > F and then Comment (L) /= ' ' loop
               L := L - 1;
            end loop;

            if L = F then
               L := Positive'Min (Comment'Last, F + Max_Len);
            end if;
         end if;

         while L > F and then Comment (L) = ' ' loop
            L := L - 1;
         end loop;

         Text_IO.Put_Line
           (File, String'(Indent * ' ') & "--  " & Comment (F .. L));
         F := L + 1;

         while F < Comment'Last and then Comment (F) = ' ' loop
            F := F + 1;
         end loop;
      end loop;
   end Output_Comment;

   ------------------------------
   -- Output_Schema_Definition --
   ------------------------------

   procedure Output_Schema_Definition (Key, Value : String) is
   begin
      if not S_Gen.Contains (Key) then
         Text_IO.Put_Line
           (Type_Adb,
            "   Schema.Insert (""" & Key & """, """ & Value & """);");
         S_Gen.Insert (Key, Value);
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

   ---------------------
   -- Put_File_Header --
   ---------------------

   procedure Put_File_Header (O : Object; File : Text_IO.File_Type) is

      function Time_Stamp return String;
      --  Returns a time stamp Ada comment line

      function Version_String return String;
      --  Returns a version string Ada comment line

      ----------------
      -- Time_Stamp --
      ----------------

      function Time_Stamp return String is
      begin
         return "--  This file was generated on "
           & GNAT.Calendar.Time_IO.Image
           (Ada.Calendar.Clock, "%A %d %B %Y at %T");
      end Time_Stamp;
      --------------------
      -- Version_String --
      --------------------

      function Version_String return String is
      begin
         return "--  AWS " & AWS.Version & " - SOAP " & SOAP.Version;
      end Version_String;

   begin
      Text_IO.New_Line (File);
      Text_IO.Put_Line (File, "--  wsdl2aws SOAP Generator v" & Version);
      Text_IO.Put_Line (File, "--");
      Text_IO.Put_Line (File, Version_String);

      if O.Stamp then
         Text_IO.Put_Line (File, Time_Stamp);
      end if;

      Text_IO.Put_Line (File, "--");
      Text_IO.Put_Line (File, "--  $ wsdl2aws " & To_String (O.Options));
      Text_IO.New_Line (File);

      if O.CVS_Tag then
         Text_IO.Put_Line (File, "--  $" & "Id$");
         Text_IO.New_Line (File);
      end if;
   end Put_File_Header;

   ----------------
   -- Put_Header --
   ----------------

   procedure Put_Header
     (File   : Text_IO.File_Type;
      O      : Object;
      Proc   : String;
      Input  : WSDL.Parameters.P_Set;
      Output : WSDL.Parameters.P_Set;
      Mode   : Header_Mode)
   is
      use Ada.Strings.Fixed;
      use type WSDL.Parameters.P_Set;

      procedure Put_Indent (Last : Character := ' ');
      --  Ouput proper indentation spaces

      procedure Input_Parameters;
      --  Output input parameters

      procedure Output_Parameters_And_End;
      --  Output output parameters for function

      Max_Len : Positive := 8;
      N       : WSDL.Parameters.P_Set;

      ----------------------
      -- Input_Parameters --
      ----------------------

      procedure Input_Parameters is
         use type WSDL2AWS.WSDL.Types.Kind;
      begin
         if Input /= null then
            --  Input parameters

            if Is_Simple_Wrapped_Parameter (O, Input) then
               N := Input.P;
            else
               N := Input;
            end if;

            while N /= null loop
               declare
                  Name : constant String :=
                           Format_Name (O, To_String (N.Name));
               begin
                  Text_IO.Put (File, Name);
                  Text_IO.Put (File, (Max_Len - Name'Length) * ' ');
               end;

               Text_IO.Put (File, " : ");

               case N.Mode is
                  when WSDL.Types.K_Simple =>
                     Text_IO.Put
                       (File,
                        SOAP.WSDL.To_Ada
                          (SOAP.WSDL.To_Type (WSDL.Types.Name (N.Typ))));

                  when WSDL.Types.Compound_Type =>
                     Text_IO.Put
                       (File,
                        Format_Name (O, WSDL.Types.Name (N.Typ) & "_Type"));

                  when WSDL.Types.K_Derived =>
                     Text_IO.Put
                       (File,
                        SOAP.Utils.To_Name
                          (WSDL.Types.Name
                            (N.Typ,
                             NS => not SOAP.WSDL.Name_Spaces.Is_XSD
                                    (WSDL.Types.NS (N.Typ)))) & "_Type");

                  when others =>
                     Text_IO.Put
                       (File, WSDL.Types.Name (N.Typ) & "_Type");
               end case;

               if N.Next /= null then
                  Text_IO.Put_Line (File, ";");
                  Put_Indent;
               end if;

               N := N.Next;
            end loop;
         end if;
      end Input_Parameters;

      -------------------------------
      -- Output_Parameters_And_End --
      -------------------------------

      procedure Output_Parameters_And_End is
         use type WSDL2AWS.WSDL.Types.Kind;
      begin
         if Is_Simple_Wrapped_Parameter (O, Output) then
            N := Output.P;
         else
            N := Output;
         end if;

         if N /= null then
            Text_IO.New_Line (File);
            Put_Indent;
            Text_IO.Put (File, "return ");

            if Is_Simple_Wrapped_Parameter (O, Output)
              and then N.Mode = WSDL.Types.K_Record
            then
               --  A record inside a record in Document style binding
               Text_IO.Put
                 (File,
                  Format_Name (O, WSDL.Types.Name (N.Typ) & "_Type"));
            else
               Text_IO.Put (File, Result_Type (O, Proc, N));
            end if;
         end if;

         --  End header depending on the mode

         case Mode is
            when Stub_Spec | Skel_Spec | C_Stub_Spec =>
               Text_IO.Put_Line (File, ";");

            when Stub_Body | C_Stub_Body =>
               Text_IO.New_Line (Stub_Adb);
               Text_IO.Put_Line (Stub_Adb, "   is");

            when Skel_Body =>
               null;
         end case;
      end Output_Parameters_And_End;

      ----------------
      -- Put_Indent --
      ----------------

      procedure Put_Indent (Last : Character := ' ') is
      begin
         if Mode = Skel_Spec then
            Text_IO.Put (File, "   ");
         end if;
         Text_IO.Put (File, "     " & Last);
      end Put_Indent;

      L_Proc : constant String := Format_Name (O, Proc);

   begin
      --  Compute maximum name length

      if Mode in Con_Stub_Header then
         --  Size of connection parameter
         Max_Len := 10;
      end if;

      if Is_Simple_Wrapped_Parameter (O, Input) then
         N := Input.P;
      else
         N := Input;
      end if;

      while N /= null loop
         Max_Len := Positive'Max
           (Max_Len, Format_Name (O, To_String (N.Name))'Length);
         N := N.Next;
      end loop;

      if Mode in Con_Stub_Header then
         --  Ouput header for connection based spec

         if Output = null then
            Text_IO.Put (File, "   procedure " & L_Proc);

            if Mode in Stub_Header or else Input /= null then
               Text_IO.New_Line (File);
            end if;

         else
            Text_IO.Put_Line (File, "   function " & L_Proc);
         end if;

         Put_Indent ('(');
         Text_IO.Put (File, "Connection : AWS.Client.HTTP_Connection");

         if Input /= null then
            Text_IO.Put_Line (File, ";");
            Put_Indent;
            Input_Parameters;
         end if;

         if O.Traces then
            Text_IO.Put_Line (File, ";");
            Put_Indent;
            Text_IO.Put_Line
               (File,
                "Pre_Call_Callback  : Pre_Call_CB "
                & ":= Null_Pre_Call_Callback'Access;");
            Put_Indent;
            Text_IO.Put
               (File,
                "Post_Call_Callback : Post_Call_CB "
                & ":= Null_Post_Call_Callback'Access");
         end if;

         if Input /= null or else Mode in Stub_Header then
            Text_IO.Put (File, ")");
         end if;

      else
         --  Ouput header for endpoint based spec

         if Output = null then
            Text_IO.Put (File, "procedure " & L_Proc);

            if Mode in Stub_Header or else Input /= null then
               Text_IO.New_Line (File);
            end if;

         else
            Text_IO.Put_Line (File, "function " & L_Proc);
         end if;

         if Input /= null or else Mode in Stub_Header then
            Put_Indent ('(');
         end if;

         Input_Parameters;

         if Mode in Stub_Header then
            if Input /= null then
               Text_IO.Put_Line (File, ";");
               Put_Indent;
            end if;

            Text_IO.Put (File, "Endpoint");
            Text_IO.Put (File, (Max_Len - 8) * ' ');
            Text_IO.Put_Line
              (File, " : String := " & To_String (O.Unit) & ".URL;");

            Put_Indent;
            Text_IO.Put (File, "Timeouts");
            Text_IO.Put (File, (Max_Len - 8) * ' ');
            Text_IO.Put
              (File, " : AWS.Client.Timeouts_Values := "
                  & To_String (O.Unit) & ".Timeouts");

            if O.Traces then
               Text_IO.Put_Line (File, ";");
               Put_Indent;
               Text_IO.Put_Line
                  (File,
                   "Pre_Call_Callback  : Pre_Call_CB "
                   & ":= Null_Pre_Call_Callback'Access;");
               Put_Indent;
               Text_IO.Put
                  (File,
                   "Post_Call_Callback : Post_Call_CB "
                   & ":= Null_Post_Call_Callback'Access");
            end if;

         end if;

         if Input /= null or else Mode in Stub_Header then
            Text_IO.Put (File, ")");
         end if;
      end if;

      Output_Parameters_And_End;
   end Put_Header;

   ---------------
   -- Put_Types --
   ---------------

   procedure Put_Types
     (O          : Object;
      Proc       : String;
      SOAPAction : String;
      Input      : WSDL.Parameters.P_Set;
      Output     : WSDL.Parameters.P_Set)
   is
      use Characters.Handling;
      use type WSDL.Parameters.P_Set;
      use type WSDL.Types.Kind;

      use type SOAP.WSDL.Schema.Binding_Style;

      W_Name : constant String :=
                 (if O.Style = SOAP.WSDL.Schema.Document
                  then SOAPAction
                  else Proc);

      procedure Generate_Record
        (Name      : String;
         Suffix    : String;
         P         : WSDL.Parameters.P_Set;
         Is_Output : Boolean               := False);
      --  Output record definitions (type and routine conversion). Note that
      --  this routine also handles choice records. The current implementation
      --  only handles single occurence of a choice.

      function Type_Name (N : WSDL.Parameters.P_Set) return String;
      --  Returns the name of the type for parameter on node N

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

      procedure Generate_References
        (File        : Text_IO.File_Type;
         P           : WSDL.Parameters.P_Set;
         For_Derived : Boolean := False);
      --  Generates with/use clauses for all referenced types

      procedure Initialize_Types_Package
        (P            : WSDL.Parameters.P_Set;
         Name         : String;
         Output       : Boolean;
         Prefix       : out Unbounded_String;
         F_Ads, F_Adb : out Text_IO.File_Type;
         Def          : WSDL.Types.Definition := WSDL.Types.No_Definition;
         Regen        : Boolean := False);
      --  Creates the full namespaces if needed and return it in Prefix.
      --  Creates also the package hierarchy. Returns a spec and body file
      --  descriptor.

      procedure Finalize_Types_Package
        (Prefix       : Unbounded_String;
         F_Ads, F_Adb : in out Text_IO.File_Type;
         No_Body      : Boolean := False);
      --  Generate code to terminate the package and close files

      procedure Output_Types (P : WSDL.Parameters.P_Set);
      --  Output types conversion routines

      function Get_Routine (P : WSDL.Parameters.P_Set) return String;
      --  Returns the Get routine for the given type

      function Set_Routine (P : WSDL.Parameters.P_Set) return String;
      --  Returns the constructor routine for the given type

      function Is_Inside_Record (Name : String) return Boolean;
      --  Returns True if Name is defined inside a record in the Input
      --  or Output parameter list.

      ----------------------------
      -- Finalize_Types_Package --
      ----------------------------

      procedure Finalize_Types_Package
        (Prefix       : Unbounded_String;
         F_Ads, F_Adb : in out Text_IO.File_Type;
         No_Body      : Boolean := False) is
      begin
         Text_IO.New_Line (F_Ads);
         Text_IO.Put_Line
           (F_Ads, "end " & To_Unit_Name (To_String (Prefix)) & ';');
         Close_File (F_Ads);

         if No_Body then
            Text_IO.Delete (F_Adb);
         else
            Text_IO.New_Line (F_Adb);
            Text_IO.Put_Line
              (F_Adb, "end " & To_Unit_Name (To_String (Prefix)) & ';');
            Close_File (F_Adb);
         end if;
      end Finalize_Types_Package;

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

         Prefix  : Unbounded_String;
         Arr_Ads : Text_IO.File_Type;
         Arr_Adb : Text_IO.File_Type;

      begin
         Initialize_Types_Package
           (P, F_Name, False, Prefix, Arr_Ads, Arr_Adb, Regen => Regen);

         if not Regen then
            Text_IO.New_Line (Tmp_Ads);
            Text_IO.Put_Line
              (Tmp_Ads, "   " & String'(1 .. 12 + F_Name'Length => '-'));
            Text_IO.Put_Line
              (Tmp_Ads, "   -- Array " & F_Name & " --");
            Text_IO.Put_Line
              (Tmp_Ads, "   " & String'(1 .. 12 + F_Name'Length => '-'));

            Text_IO.New_Line (Tmp_Ads);
         end if;

         --  Is types are to be reused from an Ada  spec ?

         Text_IO.New_Line (Arr_Ads);

         if Types_Spec (O) = "" then
            --  No user's spec, generate all type definitions

            --  Array type

            if P.Length = 0 then
               --  Unconstrained array
               Text_IO.Put_Line
                 (Arr_Ads,
                  "   type " & F_Name & " is array (Positive range <>) of "
                    & To_Ada_Type (T_Name) & ";");

            else
               --  A constrained array

               Text_IO.Put_Line
                 (Arr_Ads,
                  "   subtype " & F_Name & "_Index is Positive range 1 .. "
                    & AWS.Utils.Image (P.Length) & ";");
               Text_IO.New_Line (Arr_Ads);
               Text_IO.Put_Line
                 (Arr_Ads,
                  "   type " & F_Name & " is array (" & F_Name & "_Index)"
                    & " of " & To_Ada_Type (T_Name) & ";");
            end if;

            if P.Doc /= Null_Unbounded_String then
               Output_Comment (Arr_Ads, To_String (P.Doc), Indent => 3);
               Text_IO.New_Line (Arr_Ads);
            end if;

            if not Regen then
               Text_IO.Put_Line
                 (Tmp_Ads, "   subtype " & F_Name);
               Text_IO.Put_Line
                 (Tmp_Ads, "     is "
                  & To_Unit_Name (To_String (Prefix)) & '.' & F_Name & ';');
            end if;

            --  Access to it

            --  Safe pointer, needed only for unconstrained arrays

            if P.Length = 0 then
               Text_IO.Put_Line
                 (Arr_Ads, "   type "
                    & F_Name & "_Access" & " is access all " & F_Name & ';');

               Text_IO.New_Line (Arr_Ads);
               Text_IO.Put_Line
                 (Arr_Ads, "   package " & F_Name & "_Safe_Pointer is");
               Text_IO.Put_Line
                 (Arr_Ads, "      new SOAP.Utils.Safe_Pointers");
               Text_IO.Put_Line
                 (Arr_Ads,
                  "            (" & F_Name & ", " & F_Name & "_Access);");

               Text_IO.New_Line (Arr_Ads);
               Text_IO.Put_Line
                 (Arr_Ads, "   subtype " & F_Name & "_Safe_Access");
               Text_IO.Put_Line
                 (Arr_Ads, "      is " & F_Name
                    & "_Safe_Pointer.Safe_Pointer;");

               Text_IO.New_Line (Arr_Ads);
               Text_IO.Put_Line
                 (Arr_Ads, "   function ""+""");
               Text_IO.Put_Line
                 (Arr_Ads, "     (O : " & F_Name & ')');
               Text_IO.Put_Line
                 (Arr_Ads, "      return " & F_Name & "_Safe_Access");
               Text_IO.Put_Line
                 (Arr_Ads, "      renames "
                  & F_Name & "_Safe_Pointer.To_Safe_Pointer;");
               Text_IO.Put_Line
                 (Arr_Ads, "   --  Convert an array to a safe pointer");

               if not Regen then
                  Text_IO.New_Line (Tmp_Ads);
                  Text_IO.Put_Line
                    (Tmp_Ads, "   function ""+""");
                  Text_IO.Put_Line
                    (Tmp_Ads, "     (O : "
                     & To_Unit_Name (To_String (Prefix)) & '.' & F_Name & ')');
                  Text_IO.Put_Line
                    (Tmp_Ads, "      return "
                     & To_Unit_Name (To_String (Prefix))
                     & '.' & F_Name & "_Safe_Access");
                  Text_IO.Put_Line
                    (Tmp_Ads, "      renames "
                     & To_Unit_Name (To_String (Prefix)) & '.' & F_Name
                     & "_Safe_Pointer.To_Safe_Pointer;");
                  Text_IO.Put_Line
                    (Tmp_Ads, "   --  Convert an array to a safe pointer");
               end if;
            end if;

         else
            --  Here we have a reference to a spec, just build alias to it

            if P.Length /= 0 then
               --  This is a constrained array, create the index subtype
               Text_IO.Put_Line
                 (Arr_Ads,
                  "   subtype " & F_Name & "_Index is Positive range 1 .. "
                  & AWS.Utils.Image (P.Length) & ";");

               if not Regen then
                  Text_IO.Put_Line
                    (Tmp_Ads,
                     "   subtype " & F_Name & "_Index is Positive range 1 .. "
                     & AWS.Utils.Image (P.Length) & ";");
               end if;
            end if;

            Text_IO.Put_Line
              (Arr_Ads, "   subtype " & F_Name & " is "
               & Types_Spec (O) & "." & WSDL.Types.Name (P.Typ) & ";");

            if not Regen then
               Text_IO.Put_Line
                 (Tmp_Ads, "   subtype " & F_Name & " is "
                  & Types_Spec (O) & "." & WSDL.Types.Name (P.Typ) & ";");
            end if;

            if Is_Inside_Record (S_Name) then
               --  Only if this array is inside a record and we don't have
               --  generated this support yet.

               if not Regen then
                  Text_IO.New_Line (Tmp_Ads);

                  Header_Box (O, Tmp_Ads, "Safe Array " & F_Name);

                  Text_IO.New_Line (Tmp_Ads);
                  Text_IO.Put_Line
                    (Tmp_Ads, "   subtype " & F_Name & "_Safe_Access");
                  Text_IO.Put_Line
                    (Tmp_Ads, "      is " & Types_Spec (O) & "."
                     & WSDL.Types.Name (P.Typ)
                     & "_Safe_Pointer.Safe_Pointer;");

                  Text_IO.New_Line (Tmp_Ads);
                  Text_IO.Put_Line
                    (Tmp_Ads, "   function ""+""");
                  Text_IO.Put_Line
                    (Tmp_Ads, "     (O : " & F_Name & ')');
                  Text_IO.Put_Line
                    (Tmp_Ads, "      return " & F_Name & "_Safe_Access");
                  Text_IO.Put_Line
                    (Tmp_Ads, "      renames " & Types_Spec (O) & "."
                     & WSDL.Types.Name (P.Typ)
                     & "_Safe_Pointer.To_Safe_Pointer;");
                  Text_IO.Put_Line
                    (Tmp_Ads, "   --  Convert an array to a safe pointer");
               end if;

               Text_IO.New_Line (Arr_Ads);

               Header_Box (O, Arr_Ads, "Safe Array " & F_Name);

               Text_IO.New_Line (Arr_Ads);
               Text_IO.Put_Line
                 (Arr_Ads, "   subtype " & F_Name & "_Safe_Access");
               Text_IO.Put_Line
                 (Arr_Ads, "      is " & Types_Spec (O) & "."
                  & WSDL.Types.Name (P.Typ) & "_Safe_Pointer.Safe_Pointer;");

               Text_IO.New_Line (Arr_Ads);
               Text_IO.Put_Line
                 (Arr_Ads, "   function ""+""");
               Text_IO.Put_Line
                 (Arr_Ads, "     (O : " & F_Name & ')');
               Text_IO.Put_Line
                 (Arr_Ads, "      return " & F_Name & "_Safe_Access");
               Text_IO.Put_Line
                 (Arr_Ads, "      renames " & Types_Spec (O) & "."
                  & WSDL.Types.Name (P.Typ)
                  & "_Safe_Pointer.To_Safe_Pointer;");
               Text_IO.Put_Line
                 (Arr_Ads, "   --  Convert an array to a safe pointer");
            end if;
         end if;

         Text_IO.New_Line (Arr_Ads);

         if P.Length = 0 then
            Text_IO.Put_Line
              (Arr_Ads, "   function To_" & F_Name
               & " is new SOAP.Utils.To_T_Array");
         else
            Text_IO.Put_Line
              (Arr_Ads, "   function To_" & F_Name
                 & " is new SOAP.Utils.To_T_Array_C");
         end if;

         if not Regen then
            Text_IO.New_Line (Tmp_Ads);
            Text_IO.Put_Line
              (Tmp_Ads, "   function To_" & F_Name);
            Text_IO.Put_Line
              (Tmp_Ads, "     (From : SOAP.Types.Object_Set)");
            Text_IO.Put_Line (Tmp_Ads, "      return " & F_Name);
            Text_IO.Put_Line
              (Tmp_Ads, "      renames "
               & To_Unit_Name (To_String (Prefix)) & ".To_" & F_Name & ';');
         end if;

         Text_IO.Put
           (Arr_Ads, "     (" & To_Ada_Type (T_Name) & ", ");

         if P.Length = 0 then
            Text_IO.Put (Arr_Ads, F_Name);
         else
            Text_IO.Put (Arr_Ads, F_Name & "_Index, " & F_Name);
         end if;

         Text_IO.Put_Line (Arr_Ads, ", " & Get_Routine (P) & ");");

         Text_IO.New_Line (Arr_Ads);

         if P.Length = 0 then
            Text_IO.Put_Line
              (Arr_Ads, "   function To_Object_Set"
                 & " is new SOAP.Utils.To_Object_Set");
         else
            Text_IO.Put_Line
              (Arr_Ads, "   function To_Object_Set"
                 & " is new SOAP.Utils.To_Object_Set_C");
         end if;

         if not Regen then
            Text_IO.New_Line (Tmp_Ads);
            Text_IO.Put_Line
              (Tmp_Ads, "   function To_Object_Set");
            Text_IO.Put_Line
              (Tmp_Ads, "     (From : " & F_Name & ';');
            Text_IO.Put_Line
              (Tmp_Ads, "      NS   : SOAP.Name_Space.Object :=");
            Text_IO.Put_Line
              (Tmp_Ads, "               SOAP.Name_Space.No_Name_Space)");
            Text_IO.Put_Line (Tmp_Ads, "      return SOAP.Types.Object_Set");
            Text_IO.Put_Line
              (Tmp_Ads, "      renames "
               & To_Unit_Name (To_String (Prefix)) & ".To_Object_Set;");
         end if;

         Text_IO.Put
           (Arr_Ads, "     (" & To_Ada_Type (T_Name) & ", ");

         if P.Length = 0 then
            Text_IO.Put_Line (Arr_Ads, F_Name & ",");
         else
            Text_IO.Put_Line (Arr_Ads, F_Name & "_Index, " & F_Name & ",");
         end if;

         Text_IO.Put_Line
           (Arr_Ads,
            "      " & Set_Type (WSDL.Types.Find (Def.E_Type))
            & ", """ & To_String (Def.E_Name) & """"
            & ", """ & ET_Name & """, " & Set_Routine (P) & ");");

         Finalize_Types_Package (Prefix, Arr_Ads, Arr_Adb, No_Body => True);
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
         Prefix  : Unbounded_String;
         Der_Ads : Text_IO.File_Type;
         Der_Adb : Text_IO.File_Type;

      begin
         Initialize_Types_Package
           (P, U_Name, False, Prefix, Der_Ads, Der_Adb, Def);

         Text_IO.New_Line (Tmp_Ads);

         Text_IO.New_Line (Der_Ads);

         --  Is types are to be reused from an Ada  spec ?

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
                  Text_IO.Put_Line
                    (Der_Ads,
                     "   Compiled_Pattern : constant GNAT.Regexp.Regexp :=");
                  Text_IO.Put_Line
                    (Der_Ads, "                        GNAT.Regexp.Compile ("""
                     & To_String (Constraints.Pattern) & """);");
                  Text_IO.New_Line (Der_Ads);
               end if;

               Text_IO.Put
                 (Der_Ads, "   type " & F_Name & " is new " & B_Name);

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
                           Text_IO.New_Line (Der_Ads);
                           Text_IO.Put
                             (Der_Ads,
                              "     range"
                              & (if not L_Set or else Lower < 0.0
                                then " " else "")
                              & (if L_Set
                                then Float'Image (Lower)
                                else " " & B_Name & "'First")
                              & " .."
                              & (if not U_Set or else Upper < 0.0
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
                           Text_IO.New_Line (Der_Ads);
                           Text_IO.Put
                             (Der_Ads,
                              "     range"
                              & (if not L_Set or else Lower < 0.0
                                then " " else "")
                              & (if L_Set
                                then Long_Float'Image (Lower)
                                else B_Name & "'First")
                              & " .."
                              & (if not U_Set or else Upper < 0.0
                                then " " else "")
                              & (if U_Set
                                then Long_Float'Image (Upper)
                                else B_Name & "'Last"));
                        end if;
                     end;

                  when SOAP.WSDL.P_String =>
                     if WSDL.Types.Is_Constrained (Def) then
                        Text_IO.Put
                          (Der_Ads,
                           " (1 .."
                           & Natural'Image (Constraints.Length) & ')');

                        if Constraints.Pattern /= Null_Unbounded_String then
                           Text_IO.New_Line (Der_Ads);
                           Text_IO.Put_Line
                             (Der_Ads, "     with Dynamic_Predicate => ");
                           Text_IO.Put
                             (Der_Ads, "        GNAT.Regexp.Match (String ("
                             & F_Name & "), Compiled_Pattern)");
                        end if;

                     else
                        declare
                           Unset : Integer renames WSDL.Types.Unset;
                           Empty : Unbounded_String
                                     renames Null_Unbounded_String;
                           Pred  : Boolean := False;
                        begin
                           if Constraints.Min_Length /= WSDL.Types.Unset
                             or else Constraints.Max_Length /= WSDL.Types.Unset
                             or else Constraints.Pattern /= Empty
                           then
                              Text_IO.New_Line (Der_Ads);
                              Text_IO.Put_Line
                                (Der_Ads, "     with Dynamic_Predicate => ");

                              if Constraints.Min_Length /= Unset then
                                 Text_IO.Put
                                   (Der_Ads,
                                    "       Length (Unbounded_String ("
                                    & F_Name & ")) >="
                                    & Natural'Image (Constraints.Min_Length));
                                 Pred := True;
                              end if;

                              if Constraints.Max_Length /= Unset then
                                 if Pred then
                                    Text_IO.New_Line (Der_Ads);
                                 end if;

                                 Text_IO.Put
                                   (Der_Ads,
                                    "       "
                                    & (if Pred
                                      then "and then "
                                      else "")
                                    & "Length (Unbounded_String ("
                                    & F_Name & ")) <="
                                    & Natural'Image (Constraints.Max_Length));
                                    Pred := True;
                              end if;

                              if Constraints.Pattern /= Empty then
                                 if Pred then
                                    Text_IO.New_Line (Der_Ads);
                                 end if;

                                 Text_IO.Put
                                   (Der_Ads,
                                    "       "
                                    & (if Pred
                                      then "and then "
                                      else "")
                                    & "GNAT.Regexp.Match (To_String ("
                                    & "Unbounded_String ("
                                    & F_Name & ")), Compiled_Pattern)");
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
                           Text_IO.New_Line (Der_Ads);
                           Text_IO.Put
                             (Der_Ads,
                              "     range"
                              & (if L_Set
                                then Long_Long_Integer'Image (Lower)
                                else " " & B_Name & "'First")
                                & " .."
                                & (if U_Set
                                  then Long_Long_Integer'Image (Upper)
                                  else " " & B_Name & "'Last"));
                        end if;
                     end;
               end case;
            end;

            Text_IO.Put_Line (Der_Ads, ";");

            --  Routine to convert to base type

            if SOAP.WSDL.Is_Standard (P_Name) then
               Text_IO.New_Line (Der_Ads);

               Text_IO.Put_Line
                 (Der_Ads,
                  "   function To_" & SOAP.Utils.No_NS (P_Name) & "_Type");
               Text_IO.Put_Line (Der_Ads, "     (D : " & F_Name & ")");
               Text_IO.Put_Line (Der_Ads, "      return " & B_Name & " is");
               Text_IO.Put_Line (Der_Ads, "       (" & B_Name & " (D));");

               Text_IO.New_Line (Der_Ads);

               Text_IO.Put_Line
                 (Der_Ads,
                  "   function From_" & SOAP.Utils.No_NS (P_Name) & "_Type");
               Text_IO.Put_Line (Der_Ads, "     (D : " & B_Name & ")");
               Text_IO.Put_Line (Der_Ads, "      return " & F_Name & " is");
               Text_IO.Put_Line (Der_Ads, "       (" & F_Name & " (D));");

               Text_IO.New_Line (Der_Ads);

               Text_IO.Put_Line (Der_Ads, "   function To_" & F_Name);
               Text_IO.Put_Line (Der_Ads, "     (D : " & B_Name & ")");
               Text_IO.Put_Line
                 (Der_Ads,
                  "      return " & F_Name & " renames From_"
                  & SOAP.Utils.No_NS (P_Name) & "_Type;");

            else
               Text_IO.New_Line (Der_Ads);

               Text_IO.Put_Line (Der_Ads, "   function To_" & B_Name);
               Text_IO.Put_Line (Der_Ads, "     (D : " & F_Name & ")");
               Text_IO.Put_Line (Der_Ads, "      return " & B_Name & " is");
               Text_IO.Put_Line (Der_Ads, "       (" & B_Name & " (D));");

               Text_IO.New_Line (Der_Ads);

               Text_IO.Put_Line (Der_Ads, "   function From_" & B_Name);
               Text_IO.Put_Line (Der_Ads, "     (D : " & B_Name & ")");
               Text_IO.Put_Line (Der_Ads, "      return " & F_Name & " is");
               Text_IO.Put_Line (Der_Ads, "       (" & F_Name & " (D));");

               Text_IO.New_Line (Der_Ads);

               Text_IO.Put_Line (Der_Ads, "   function To_" & F_Name);
               Text_IO.Put_Line (Der_Ads, "     (D : " & B_Name & ")");
               Text_IO.Put_Line
                 (Der_Ads,
                  "      return " & F_Name & " renames From_" & B_Name & ";");
            end if;

            --  For array support

            Text_IO.New_Line (Der_Ads);
            Text_IO.Put_Line (Der_Ads, "   function To_" & F_Name);
            Text_IO.Put_Line (Der_Ads, "     (O : SOAP.Types.Object'Class)");
            Text_IO.Put_Line (Der_Ads, "      return " & F_Name & " is");
            Text_IO.Put_Line
              (Der_Ads,
               "       ("
               & WSDL.Types.From_SOAP (Def, Object => "O") & ");");

            Text_IO.New_Line (Der_Ads);
            Text_IO.Put_Line (Der_Ads, "   function To_SOAP_Object");
            Text_IO.Put_Line (Der_Ads, "     (D         : " & F_Name & ";");
            Text_IO.Put_Line
              (Der_Ads, "      Name      : String := ""item"";");
            Text_IO.Put_Line
              (Der_Ads, "      Type_Name : String := Q_Type_Name;");
            Text_IO.Put_Line
              (Der_Ads, "      NS        : SOAP.Name_Space.Object := "
               & "Name_Space)");
            Text_IO.Put_Line
              (Der_Ads, "      return "
               &  SOAP.WSDL.Set_Type
                    (SOAP.WSDL.To_Type (WSDL.Types.Root_Type_For (Def)))
               & " is");
            Text_IO.Put_Line
              (Der_Ads,
               "        ("
               & WSDL.Types.To_SOAP
                 (Def,
                  Object    => "D",
                  Name      => "Name",
                  Type_Name => "Type_Name",
                  Name_Kind => WSDL.Types.Both_Var,
                  NS        => "NS") & ");");

            --  For Types child package

            Text_IO.Put_Line
              (Tmp_Ads, "   subtype " & F_Name);
            Text_IO.Put_Line
              (Tmp_Ads, "     is " & To_Unit_Name (To_String (Prefix)) & '.'
               & F_Name & ';');

            if SOAP.WSDL.Is_Standard (P_Name) then
               Text_IO.New_Line (Tmp_Ads);

               Text_IO.Put_Line
                 (Tmp_Ads,
                  "   function To_" & SOAP.Utils.No_NS (P_Name) & "_Type");
               Text_IO.Put_Line (Tmp_Ads, "     (D : " & F_Name & ")");
               Text_IO.Put_Line (Tmp_Ads, "      return " & B_Name);
               Text_IO.Put_Line
                 (Tmp_Ads, "      renames "
                  & To_Unit_Name (To_String (Prefix))
                  & ".To_" & SOAP.Utils.No_NS (P_Name) & "_Type;");

               Text_IO.Put_Line
                 (Tmp_Ads,
                  "   function From_" & SOAP.Utils.No_NS (P_Name) & "_Type");
               Text_IO.Put_Line (Tmp_Ads, "     (D : " & B_Name & ")");
               Text_IO.Put_Line (Tmp_Ads, "      return " & F_Name);
               Text_IO.Put_Line
                 (Tmp_Ads, "      renames "
                  & To_Unit_Name (To_String (Prefix))
                  & ".From_" & SOAP.Utils.No_NS (P_Name) & "_Type;");

               Text_IO.New_Line (Tmp_Ads);

               Text_IO.Put_Line (Tmp_Ads, "   function To_" & F_Name);
               Text_IO.Put_Line (Tmp_Ads, "     (D : " & B_Name & ")");
               Text_IO.Put_Line
                 (Tmp_Ads,
                  "      return " & F_Name & " renames "
                  & To_Unit_Name (To_String (Prefix))
                  & ".From_"  & SOAP.Utils.No_NS (P_Name) & "_Type;");

               --  The following routine give an alias without the namespace to
               --  routine with a standard base name. This is mostly for upward
               --  compatibility with existing code.

               Text_IO.Put_Line
                 (Tmp_Ads,
                  "   function To_" & SOAP.Utils.No_NS (Name) & "_Type");
               Text_IO.Put_Line (Tmp_Ads, "     (D : " & B_Name & ")");
               Text_IO.Put_Line
                 (Tmp_Ads,
                  "      return " & F_Name & " renames "
                  & To_Unit_Name (To_String (Prefix))
                  & ".From_"  & SOAP.Utils.No_NS (P_Name) & "_Type;");

            else
               Text_IO.Put_Line
                 (Tmp_Ads,
                  "   function To_" & B_Name & " (D : " & F_Name & ")");
               Text_IO.Put_Line
                 (Tmp_Ads, "     return " & B_Name);
               Text_IO.Put_Line
                 (Tmp_Ads, "     renames "
                  & To_Unit_Name (To_String (Prefix)) & ".To_" & B_Name & ';');

               Text_IO.Put_Line
                 (Tmp_Ads,
                  "   function From_" & B_Name & " (D : " & B_Name & ")");
               Text_IO.Put_Line
                 (Tmp_Ads, "     return " & F_Name);
               Text_IO.Put_Line
                 (Tmp_Ads, "     renames "
                  & To_Unit_Name (To_String (Prefix))
                  & ".From_" & B_Name & ';');

               Text_IO.New_Line (Tmp_Ads);

               Text_IO.Put_Line (Tmp_Ads, "   function To_" & F_Name);
               Text_IO.Put_Line (Tmp_Ads, "     (D : " & B_Name & ")");
               Text_IO.Put_Line
                 (Tmp_Ads,
                  "      return " & F_Name & " renames "
                  & To_Unit_Name (To_String (Prefix))
                  & ".From_" & B_Name & ";");
            end if;

         else
            Text_IO.Put_Line
              (Der_Ads, "   subtype " & F_Name & " is "
               & Types_Spec (O) & "." & SOAP.Utils.No_NS (Name) & ";");

            --  Routine to convert to base type, as this is a subtype
            --  just returns the value as-is.

            Text_IO.New_Line (Der_Ads);

            Text_IO.Put_Line (Der_Ads, "   function To_" & Q_Name & "_Type");
            Text_IO.Put_Line (Der_Ads, "     (D : " & F_Name & ")");
            Text_IO.Put_Line
              (Der_Ads,
               "      return " & Types_Spec (O)
               & "." & SOAP.Utils.No_NS (Name) & " is (D);");

            Text_IO.New_Line (Der_Ads);

            Text_IO.Put_Line (Der_Ads, "   function From_" & Q_Name & "_Type");
            Text_IO.Put_Line
              (Der_Ads, "     (D : " & Types_Spec (O) & "."
               & SOAP.Utils.No_NS (Name) & ")");
            Text_IO.Put_Line (Der_Ads, "      return " & F_Name & " is (D);");

            Text_IO.New_Line (Der_Ads);
            Text_IO.Put_Line (Der_Ads, "   function To_" & F_Name);
            Text_IO.Put_Line
              (Der_Ads, "     (D : " & Types_Spec (O)
               & "." & SOAP.Utils.No_NS (Name) & ")");
            Text_IO.Put_Line
              (Der_Ads, "      return " & F_Name
               & " renames From_" & Q_Name & "_Type;");

            if SOAP.WSDL.Is_Standard (P_Name) then
               Text_IO.New_Line (Der_Ads);

               Text_IO.Put_Line
                 (Der_Ads,
                  "   function To_" & SOAP.Utils.No_NS (P_Name) & "_Type");
               Text_IO.Put_Line (Der_Ads, "     (D : " & F_Name & ")");
               Text_IO.Put_Line (Der_Ads, "      return " & B_Name & " is");
               Text_IO.Put_Line (Der_Ads, "       (" & B_Name & " (D));");

               Text_IO.Put_Line
                 (Der_Ads,
                  "   function From_" & SOAP.Utils.No_NS (P_Name) & "_Type");
               Text_IO.Put_Line (Der_Ads, "     (D : " & B_Name & ")");
               Text_IO.Put_Line (Der_Ads, "      return " & F_Name & " is");
               Text_IO.Put_Line (Der_Ads, "       (" & F_Name & " (D));");
            end if;

            Output_Comment (Der_Ads, To_String (P.Doc), Indent => 3);

            --  For array support

            Text_IO.New_Line (Der_Ads);
            Text_IO.Put_Line (Der_Ads, "   function To_" & F_Name);
            Text_IO.Put_Line (Der_Ads, "     (O : SOAP.Types.Object'Class)");
            Text_IO.Put_Line (Der_Ads, "      return " & F_Name & " is");
            Text_IO.Put_Line
              (Der_Ads,
               "       ("
               & WSDL.Types.From_SOAP (Def, Object => "O") & ");");

            Text_IO.New_Line (Der_Ads);
            Text_IO.Put_Line (Der_Ads, "   function To_SOAP_Object");
            Text_IO.Put_Line (Der_Ads, "     (D         : " & F_Name & ";");
            Text_IO.Put_Line
              (Der_Ads, "      Name      : String := ""item"";");
            Text_IO.Put_Line
              (Der_Ads, "      Type_Name : String := Q_Type_Name;");
            Text_IO.Put_Line
              (Der_Ads, "      NS        : SOAP.Name_Space.Object := "
               & "Name_Space)");
            Text_IO.Put_Line
              (Der_Ads, "      return "
               &  SOAP.WSDL.Set_Type
                    (SOAP.WSDL.To_Type (WSDL.Types.Root_Type_For (Def)))
               & " is");
            Text_IO.Put_Line
              (Der_Ads,
               "        ("
               & WSDL.Types.To_SOAP
                 (Def,
                  Object    => "D",
                  Name      => "Name",
                  Type_Name => "Type_Name",
                  Name_Kind => WSDL.Types.Both_Var,
                  NS        => "NS") & ");");

            --  For Types child package

            Text_IO.Put_Line
              (Tmp_Ads, "   subtype " & F_Name);
            Text_IO.Put_Line
              (Tmp_Ads, "     is " & To_Unit_Name (To_String (Prefix)) & '.'
               & F_Name & ';');

            Output_Comment (Tmp_Ads, To_String (P.Doc), Indent => 3);

            Text_IO.Put_Line
              (Tmp_Ads, "   function To_" & Q_Name & " (D : " & F_Name & ")");
            Text_IO.Put_Line
              (Tmp_Ads, "     return " & Types_Spec (O)
               & "." & SOAP.Utils.No_NS (Name));
            Text_IO.Put_Line
              (Tmp_Ads, "     renames "
               & To_Unit_Name (To_String (Prefix))
               & ".To_" & Q_Name & "_Type;");

            Text_IO.Put_Line
              (Tmp_Ads,
               "   function From_" & Q_Name
               & " (D : " & Types_Spec (O) & "." & SOAP.Utils.No_NS (Name)
               & ")");
            Text_IO.Put_Line
              (Tmp_Ads, "     return " & F_Name);
            Text_IO.Put_Line
              (Tmp_Ads, "     renames "
               & To_Unit_Name (To_String (Prefix))
               & ".From_" & Q_Name & "_Type;");

            if SOAP.WSDL.Is_Standard (P_Name) then
               Text_IO.New_Line (Tmp_Ads);

               Text_IO.Put_Line
                 (Tmp_Ads,
                  "   function To_" & SOAP.Utils.No_NS (P_Name) & "_Type");
               Text_IO.Put_Line (Tmp_Ads, "     (D : " & F_Name & ")");
               Text_IO.Put_Line (Tmp_Ads, "      return " & B_Name);
               Text_IO.Put_Line (Tmp_Ads, "      renames "
                  & To_Unit_Name (To_String (Prefix))
                  & ".To_" & SOAP.Utils.No_NS (P_Name) & "_Type;");

               Text_IO.Put_Line
                 (Tmp_Ads,
                  "   function From_" & SOAP.Utils.No_NS (P_Name) & "_Type");
               Text_IO.Put_Line (Tmp_Ads, "     (D : " & B_Name & ")");
               Text_IO.Put_Line
                 (Tmp_Ads, "      return " & F_Name);
               Text_IO.Put_Line
                 (Tmp_Ads, "      renames "
                  & To_Unit_Name (To_String (Prefix))
                  & ".From_" & SOAP.Utils.No_NS (P_Name) & "_Type;");
            end if;
         end if;

         Finalize_Types_Package (Prefix, Der_Ads, Der_Adb, No_Body => True);
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

         function Image (E : WSDL.Types.E_Node_Access) return String;
         --  Returns the enumeration definition

         -----------
         -- Image --
         -----------

         function Image (E : WSDL.Types.E_Node_Access) return String is
            Col    : constant Natural := 13 + F_Name'Length;
            Sep    : constant String := ASCII.LF & "     ";
            Result : Unbounded_String;
            N      : WSDL.Types.E_Node_Access := E;
         begin
            while N /= null loop

               if Result = Null_Unbounded_String then
                  Append (Result, "(");
               else
                  Append (Result, ", ");
               end if;

               Append (Result, Format_Name (O, To_String (N.Value)));

               N := N.Next;
            end loop;

            Append (Result, ")");

            if Col + Length (Result) > 80 then
               --  Split the result in multiple line
               Result := Sep & Result;

               declare
                  Line_Size : constant := 70;
                  K         : Natural := Line_Size;
               begin
                  while K < Length (Result) loop
                     for I in reverse 1 .. K loop
                        if Element (Result, I) = ',' then
                           Insert (Result, I + 1, Sep);
                           exit;
                        end if;
                     end loop;

                     K := K + Line_Size;
                  end loop;
               end;
            end if;

            return To_String (Result);
         end Image;

         Def     : constant WSDL.Types.Definition := WSDL.Types.Find (P.Typ);
         N       : WSDL.Types.E_Node_Access := Def.E_Def;
         Prefix  : Unbounded_String;
         Enu_Ads : Text_IO.File_Type;
         Enu_Adb : Text_IO.File_Type;

      begin
         Initialize_Types_Package (P, F_Name, False, Prefix, Enu_Ads, Enu_Adb);

         Text_IO.New_Line (Enu_Ads);

         --  Is types are to be reused from an Ada  spec ?

         Text_IO.New_Line (Tmp_Ads);

         Text_IO.Put_Line
           (Tmp_Ads, "   subtype " & F_Name & " is "
            & To_Unit_Name (To_String (Prefix)) & "." & F_Name & ';');

         if Types_Spec (O) = "" then
            Text_IO.Put_Line
              (Enu_Ads,
               "   type " & F_Name & " is " & Image (Def.E_Def) & ";");

            Text_IO.Put_Line
              (Enu_Ads, "   function To_" & F_Name);
            Text_IO.Put_Line
              (Enu_Ads, "     (D : " & F_Name & ')');
            Text_IO.Put_Line
              (Enu_Ads,
               "      return " & F_Name & " is (D);");
            Text_IO.Put_Line
              (Enu_Ads, "   function From_" & F_Name);
            Text_IO.Put_Line
              (Enu_Ads,
               "     (D : " & F_Name & ')');
            Text_IO.Put_Line
              (Enu_Ads,
               "     return " & F_Name & " is (D);");
         else
            Text_IO.Put_Line
              (Enu_Ads, "   subtype " & F_Name & " is "
               & Types_Spec (O) & "." & WSDL.Types.Name (Def.Ref) & ";");

            Text_IO.Put_Line
              (Enu_Ads, "   function To_" & F_Name);
            Text_IO.Put_Line
              (Enu_Ads, "     (D : " & F_Name & ')');
            Text_IO.Put_Line
              (Enu_Ads,
               "      return "
               & Types_Spec (O) & "." & WSDL.Types.Name (Def.Ref)
               & " is (D);");
            Text_IO.Put_Line
              (Enu_Ads, "   function From_" & F_Name);
            Text_IO.Put_Line
              (Enu_Ads,
               "     (D : "
               & Types_Spec (O) & "." & WSDL.Types.Name (Def.Ref)  & ')');
            Text_IO.Put_Line
              (Enu_Ads,
               "     return " & F_Name & " is (D);");
         end if;

         --  Generate Image function

         Text_IO.New_Line (Enu_Ads);
         Text_IO.Put_Line
           (Enu_Ads,
            "   function Image (E : " & F_Name & ") return String;");

         Text_IO.New_Line (Tmp_Ads);
         Text_IO.Put_Line
           (Tmp_Ads, "   function Image (E : " & F_Name & ")");
         Text_IO.Put_Line
           (Tmp_Ads, "      return String ");
         Text_IO.Put_Line
           (Tmp_Ads, "      renames "
            & To_Unit_Name (To_String (Prefix)) & ".Image;");

         Text_IO.New_Line (Enu_Adb);
         Text_IO.Put_Line
           (Enu_Adb,
            "   function Image (E : " & F_Name & ") return String is");
         Text_IO.Put_Line (Enu_Adb, "   begin");
         Text_IO.Put_Line (Enu_Adb, "      case E is");

         while N /= null loop
            Text_IO.Put (Enu_Adb, "         when ");

            if Types_Spec (O) /= "" then
               Text_IO.Put (Enu_Adb, Types_Spec (O) & '.');
            end if;

            Text_IO.Put_Line
              (Enu_Adb, Format_Name (O, To_String (N.Value))
                 & " => return """ & To_String (N.Value) & """;");

            N := N.Next;
         end loop;

         Text_IO.Put_Line (Enu_Adb, "      end case;");
         Text_IO.Put_Line (Enu_Adb, "   end Image;");

         --  From/To string

         Text_IO.Put_Line
           (Enu_Ads,
            "   function To_String_Type");
         Text_IO.Put_Line
           (Enu_Ads,
            "     (D : " & F_Name & ")");
         Text_IO.Put_Line
           (Enu_Ads,
            "      return String is (Image (D));");

         Text_IO.Put_Line
           (Enu_Ads,
            "   function From_String_Type");
         Text_IO.Put_Line
           (Enu_Ads,
            "     (D : String)");
         Text_IO.Put_Line
           (Enu_Ads,
            "      return " & F_Name & " is (" & F_Name & "'Value (D));");

         --  Value function

         Text_IO.Put_Line
           (Enu_Ads,
            "   function Value (S : String) return " & F_Name
            & " renames From_String_Type;");

         --  For array support

         Text_IO.New_Line (Enu_Ads);
         Text_IO.Put_Line
           (Enu_Ads,
            "   function To_" & F_Name);
         Text_IO.Put_Line
           (Enu_Ads,
            "     (O : SOAP.Types.Object'Class)");
         Text_IO.Put_Line
           (Enu_Ads,
            "      return " & F_Name & " is");
         Text_IO.Put_Line
           (Enu_Ads,
            "       (From_String_Type "
            & "(SOAP.Types.V (SOAP.Types.SOAP_Enumeration (O))));");

         Text_IO.New_Line (Enu_Ads);
         Text_IO.Put_Line
           (Enu_Ads,
            "   function To_SOAP_Object");
         Text_IO.Put_Line
           (Enu_Ads,
            "     (D         : " & F_Name & ';');
         Text_IO.Put_Line
           (Enu_Ads,
            "      Name      : String := ""item"";");
         Text_IO.Put_Line
           (Enu_Ads,
            "      Type_Name : String := Q_Type_Name;");
         Text_IO.Put_Line
           (Enu_Ads,
            "      NS        : SOAP.Name_Space.Object := Name_Space)");
         Text_IO.Put_Line
           (Enu_Ads,
            "      return SOAP.Types.SOAP_Enumeration is");
         Text_IO.Put_Line
           (Enu_Ads,
            "        (SOAP.Types.E (Image (D), Type_Name, Name, NS));");

         Finalize_Types_Package (Prefix, Enu_Ads, Enu_Adb);
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
            File : Text_IO.File_Type;

         begin
            if Create then
               Text_IO.Create (File, Text_IO.Out_File, To_Lower (N) & ".ads");
               Put_File_Header (O, File);

               if Leaf then
                  With_Unit (File, "SOAP.Name_Space");
               end if;

               Text_IO.Put_Line (File, "package " & To_Unit_Name (N) & " is");

               if not Leaf then
                  Text_IO.Put_Line (File, "   pragma Pure;");
               end if;

               if Leaf then
                  Text_IO.Put_Line (File, "   pragma Style_Checks (Off);");
                  Text_IO.Put_Line
                    (File,
                     "   Name_Space : constant SOAP.Name_Space.Object :=");
                  Text_IO.Put_Line
                    (File,
                     "                  SOAP.Name_Space.Create ("""
                     & SOAP.Name_Space.Name (NS) & """, """
                     & SOAP.Name_Space.Value (NS) & """);");
               end if;

               Text_IO.Put_Line (File, "end " & To_Unit_Name (N) & ';');

               Close_File (File);
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

         Max     : Positive;
         Count   : Natural := 0;

         Prefix  : Unbounded_String;

         Rec_Ads : Text_IO.File_Type;
         Rec_Adb : Text_IO.File_Type;

      begin
         Initialize_Types_Package
           (P, F_Name, Is_Output, Prefix, Rec_Ads, Rec_Adb);

         if Is_Output then
            R := P;
         else
            R := P.P;
         end if;

         --  Generate record type

         Text_IO.New_Line (Tmp_Ads);
         Header_Box (O, Tmp_Ads, "Record " & F_Name);

         --  Is types are to be reused from an Ada spec ?

         if Types_Spec (O) = ""
              or else
            (Is_Simple_Wrapped_Parameter (O, Input) and then P = Input)
              or else
            (Is_Simple_Wrapped_Parameter (O, Output) and then P = Output)
         then
            --  Compute max field width, compute also the number of fields.
            --  During this first iteration we also generate the record fields
            --  information for the schema definition.

            N := R;

            Max := 1;

            while N /= null loop
               Count := Count + 1;
               Max := Positive'Max
                 (Max, Format_Name (O, To_String (N.Name))'Length);
               N := N.Next;
            end loop;

            --  Output field

            N := R;

            Output_Comment (Rec_Ads, To_String (P.Doc), Indent => 3);
            Text_IO.New_Line (Rec_Ads);

            if N = null then
               Text_IO.Put_Line
                 (Rec_Ads, "   type " & F_Name & " is null record;");

            else
               if Is_Choice then
                  Text_IO.Put
                    (Rec_Ads, "   type Choice is (");

                  for K in 1 .. Count loop
                     Text_IO.Put (Rec_Ads, "C" & AWS.Utils.Image (K));
                     if K < Count then
                        Text_IO.Put (Rec_Ads, ", ");
                     end if;
                  end loop;

                  Text_IO.Put_Line (Rec_Ads, ");");
                  Text_IO.New_Line (Rec_Ads);

                  Text_IO.Put_Line
                    (Rec_Ads,
                     "   type " & F_Name & " (C : Choice := C1) is record");
                  Text_IO.Put_Line
                    (Rec_Ads,
                     "      case C is");

               else
                  Text_IO.Put_Line
                    (Rec_Ads, "   type " & F_Name & " is record");
               end if;

               Count := 0;

               while N /= null loop
                  Count := Count + 1;

                  if Is_Choice then
                     Text_IO.Put_Line
                       (Rec_Ads,
                        "         when C" & AWS.Utils.Image (Count) & " =>");
                     Text_IO.Put (Rec_Ads, "      ");
                  end if;

                  declare
                     F_Name : constant String :=
                                Format_Name (O, To_String (N.Name));
                  begin
                     Text_IO.Put
                       (Rec_Ads, "      "
                        & F_Name
                        & String'(1 .. Max - F_Name'Length => ' ') & " : ");
                  end;

                  Text_IO.Put (Rec_Ads, Format_Name (O, Type_Name (N)));

                  Text_IO.Put_Line (Rec_Ads, ";");
                  Output_Comment (Rec_Ads, To_String (N.Doc), Indent => 6);

                  if N.Mode = WSDL.Types.K_Array then
                     Text_IO.Put_Line
                       (Rec_Ads,
                        "      --  Access items with : result.Item (n)");
                  end if;

                  N := N.Next;
               end loop;

               if Is_Choice then
                  Text_IO.Put_Line (Rec_Ads, "      end case;");
               end if;

               Text_IO.Put_Line
                 (Rec_Ads, "   end record;");
            end if;

            Text_IO.New_Line (Tmp_Ads);
            Text_IO.Put_Line (Tmp_Ads, "   subtype " & F_Name);
            Text_IO.Put_Line
              (Tmp_Ads, "     is "
               & To_Unit_Name (To_String (Prefix)) & '.' & F_Name & ';');

         else
            Text_IO.New_Line (Rec_Ads);
            Text_IO.Put_Line
              (Rec_Ads, "   subtype " & F_Name & " is "
               & Types_Spec (O) & "." & WSDL.Types.Name (P.Typ) & ";");

            Text_IO.New_Line (Tmp_Ads);
            Text_IO.Put_Line
              (Tmp_Ads, "   subtype " & F_Name & " is "
               & Types_Spec (O) & "." & WSDL.Types.Name (P.Typ) & ";");
         end if;

         --  Generate conversion spec

         Text_IO.New_Line (Rec_Ads);
         Text_IO.Put_Line (Rec_Ads, "   function To_" & F_Name);
         Text_IO.Put_Line (Rec_Ads, "     (O : SOAP.Types.Object'Class)");
         Text_IO.Put_Line (Rec_Ads, "      return " & F_Name & ';');

         Text_IO.New_Line (Tmp_Ads);
         Text_IO.Put_Line (Tmp_Ads, "   function To_" & F_Name);
         Text_IO.Put_Line (Tmp_Ads, "     (O : SOAP.Types.Object'Class)");
         Text_IO.Put_Line (Tmp_Ads, "      return " & F_Name);
         Text_IO.Put_Line
           (Tmp_Ads, "      renames "
            & To_Unit_Name (To_String (Prefix)) & ".To_" & F_Name & ';');

         Text_IO.New_Line (Rec_Ads);
         Text_IO.Put_Line (Rec_Ads, "   function To_SOAP_Object");
         Text_IO.Put_Line (Rec_Ads, "     (R         : " & F_Name & ';');
         Text_IO.Put_Line (Rec_Ads, "      Name      : String := ""item"";");
         Text_IO.Put_Line
           (Rec_Ads, "      Type_Name : String := Q_Type_Name;");
         Text_IO.Put_Line
           (Rec_Ads, "      NS        : SOAP.Name_Space.Object := "
            & "Name_Space)");
         Text_IO.Put_Line (Rec_Ads, "      return SOAP.Types.SOAP_Record;");

         if not NS_Generated.Contains (SOAP.Name_Space.Name (NS)) then
            Text_IO.New_Line (Tmp_Ads);
            Text_IO.Put_Line
              (Tmp_Ads,
               "   " & SOAP.Name_Space.Name (NS) & "_Name_Space : "
               & "SOAP.Name_Space.Object ");
            Text_IO.Put_Line
              (Tmp_Ads,
               "     renames " & Pck_NS & ".Name_Space;");
            NS_Generated.Insert (SOAP.Name_Space.Name (NS));
         end if;

         Text_IO.New_Line (Tmp_Ads);
         Text_IO.Put_Line (Tmp_Ads, "   function To_SOAP_Object");
         Text_IO.Put_Line (Tmp_Ads, "     (R         : " & F_Name & ';');
         Text_IO.Put_Line (Tmp_Ads, "      Name      : String := ""item"";");
         Text_IO.Put_Line
           (Tmp_Ads, "      Type_Name : String := "
            & To_Unit_Name (To_String (Prefix)) & ".Q_Type_Name;");
         Text_IO.Put_Line
           (Tmp_Ads, "      NS        : SOAP.Name_Space.Object := "
            & SOAP.Name_Space.Name (NS) & "_Name_Space)");
         Text_IO.Put_Line (Tmp_Ads, "      return SOAP.Types.SOAP_Record");
         Text_IO.Put_Line
           (Tmp_Ads, "      renames "
            & To_Unit_Name (To_String (Prefix)) & ".To_SOAP_Object;");

         --  Generate conversion body

         Header_Box (O, Rec_Adb, "Record " & F_Name);

         --  SOAP to Ada

         Text_IO.New_Line (Rec_Adb);
         Text_IO.Put_Line (Rec_Adb, "   function To_" & F_Name);
         Text_IO.Put_Line (Rec_Adb, "     (O : SOAP.Types.Object'Class)");
         Text_IO.Put_Line (Rec_Adb, "      return " & F_Name);
         Text_IO.Put_Line (Rec_Adb, "   is");

         --  Declare the SOAP record object

         Text_IO.Put_Line
           (Rec_Adb,
            "      R : constant SOAP.Types.SOAP_Record "
              & ":= SOAP.Types.SOAP_Record (O);");

         if Is_Choice and then R /= null then
            --  Generate C_Name helper routine

            Text_IO.New_Line (Rec_Adb);
            Text_IO.Put_Line
              (Rec_Adb,
               "      function C_Name return String is");
            Text_IO.Put
              (Rec_Adb,
               "        (");

            N := R;

            while N /= null loop
               if N = R then
                  Text_IO.Put (Rec_Adb, "if");
               elsif N.Next = null then
                  Text_IO.Put (Rec_Adb, "         else --");
               else
                  Text_IO.Put (Rec_Adb, "         elsif");
               end if;

               Text_IO.Put_Line
                 (Rec_Adb,
                  " SOAP.Types.Exists (R, """
                  & Format_Name (O, To_String (N.Name)) & """) then");
               Text_IO.Put
                 (Rec_Adb,
                  "           """
                  & Format_Name (O, To_String (N.Name)) & """");
               N := N.Next;

               if N = null then
                  Text_IO.Put_Line (Rec_Adb, ");");
               else
                  Text_IO.New_Line (Rec_Adb);
               end if;
            end loop;

            --  Generate C_Disc helper routine

            Text_IO.New_Line (Rec_Adb);
            Text_IO.Put_Line
              (Rec_Adb,
               "      function C_Disc return Choice is");
            Text_IO.Put
              (Rec_Adb,
               "        (");

            N := R;

            Count := 0;

            while N /= null loop
               Count := Count + 1;
               if N = R then
                  Text_IO.Put (Rec_Adb, "if");
               elsif N.Next = null then
                  Text_IO.Put (Rec_Adb, "         else --");
               else
                  Text_IO.Put (Rec_Adb, "         elsif");
               end if;

               Text_IO.Put_Line
                 (Rec_Adb,
                  " SOAP.Types.Exists (R, """
                  & Format_Name (O, To_String (N.Name)) & """) then");
               Text_IO.Put
                 (Rec_Adb,
                  "           C" & AWS.Utils.Image (Count));
               N := N.Next;

               if N = null then
                  Text_IO.Put_Line (Rec_Adb, ");");
               else
                  Text_IO.New_Line (Rec_Adb);
               end if;
            end loop;

            Text_IO.New_Line (Rec_Adb);
            Text_IO.Put_Line
              (Rec_Adb,
               "      E : constant SOAP.Types.Object'Class "
               & ":= SOAP.Types.V (R, C_Name);");

         else
            --  Declare all record's fields

            N := R;

            while N /= null loop
               Text_IO.Put_Line
                 (Rec_Adb,
                  "      " & Format_Name (O, To_String (N.Name))
                  & " : constant SOAP.Types."
                  & (if N.Mode = WSDL.Types.K_Array
                       and then not WSDL.Parameters.Is_Uniq (N.all)
                     then "Object_Set"
                     else "Object'Class")
                  & " := SOAP.Types.V (R, """
                  & To_String (N.Name) & """);");

               N := N.Next;
            end loop;
         end if;

         Text_IO.Put_Line (Rec_Adb, "   begin");

         --  Check size of set minOccurs / maxOccurs

         N := R;

         while N /= null loop
            if N.Mode = WSDL.Types.K_Array
              and then not WSDL.Parameters.Is_Uniq (N.all)
            then
               Text_IO.Put_Line
                 (Rec_Adb,
                  "      if "
                  & Format_Name (O, To_String (N.Name))
                  & "'Length not in " & AWS.Utils.Image (N.Min)
                  & " .. " & AWS.Utils.Image (N.Max) & " then");
               Text_IO.Put_Line
                 (Rec_Adb, "         raise SOAP.SOAP_Error");
               Text_IO.Put_Line
                 (Rec_Adb, "            with ""Length of "
                  & Format_Name (O, To_String (N.Name))
                  & " violate schema definition"";");
               Text_IO.Put_Line
                 (Rec_Adb,
                  "      end if;");
            end if;

            N := N.Next;
         end loop;

         if Is_Choice and then R /= null then
            Text_IO.Put_Line (Rec_Adb, "      case C_Disc is");
         else
            Text_IO.Put (Rec_Adb, "      return (");
         end if;

         --  Aggregate to build the record object

         N := R;

         if N = null then
            --  An empty record
            Text_IO.Put_Line (Rec_Adb, "null record);");

         elsif not Is_Choice and then N.Next = null then
            --  We have a single element into this record, we must use a named
            --  notation for the aggregate.
            Text_IO.Put
              (Rec_Adb, Format_Name (O, To_String (N.Name)) & " => ");
         end if;

         Count := 0;

         while N /= null loop
            Count := Count + 1;

            if Is_Choice then
               Text_IO.Put_Line
                 (Rec_Adb,
                  "         when C" & AWS.Utils.Image (Count) & " =>");
               Text_IO.Put
                 (Rec_Adb,
                  "            return (C" & AWS.Utils.Image (Count) & ", ");

            elsif N /= R then
               Text_IO.Put      (Rec_Adb, "              ");
            end if;

            declare
               Def  : constant WSDL.Types.Definition :=
                        WSDL.Types.Find (N.Typ);
               Name : constant String :=
                        (if Is_Choice
                         then "E"
                         else Format_Name (O, To_String (N.Name)));
            begin
               case N.Mode is
                  when WSDL.Types.K_Simple
                     | WSDL.Types.K_Derived
                     | WSDL.Types.K_Enumeration
                     =>
                     Text_IO.Put
                       (Rec_Adb,
                        WSDL.Parameters.From_SOAP (N.all, Object => Name));

                  when WSDL.Types.K_Array =>
                     Text_IO.Put
                       (Rec_Adb,
                        WSDL.Parameters.From_SOAP
                          (N.all,
                           Object    => Name,
                           Type_Name =>
                              Format_Name (O, WSDL.Types.Name (Def.Ref))));

                  when WSDL.Types.K_Record =>
                     Text_IO.Put
                       (Rec_Adb,
                        WSDL.Parameters.From_SOAP
                          (N.all,
                           Object    => Name,
                           Type_Name => Format_Name (O, Type_Name (N))));
               end case;
            end;

            if Is_Choice or else N.Next = null then
               Text_IO.Put_Line (Rec_Adb, ");");
            else
               Text_IO.Put_Line (Rec_Adb, ",");
            end if;

            N := N.Next;
         end loop;

         if Is_Choice and then R /= null then
            Text_IO.Put_Line (Rec_Adb, "      end case;");
         end if;

         --  Generate exception handler

         N := R;

         if N /= null then
            declare
               procedure Emit_Check (F_Name, I_Type : String);
               --  Emit a check for F_Name'Tag = I_Type'Tag

               ----------------
               -- Emit_Check --
               ----------------

               procedure Emit_Check (F_Name, I_Type : String) is
               begin
                  if Is_Choice then
                     Text_IO.Put_Line
                       (Rec_Adb,
                        "         if C_Disc = C" & AWS.Utils.Image (Count)
                        & " and then E'Tag /= "
                        & I_Type & "'Tag then");
                  else
                     Text_IO.Put_Line
                       (Rec_Adb,
                        "         if " & F_Name & "'Tag /= "
                        & I_Type & "'Tag then");
                  end if;
                  Text_IO.Put_Line
                    (Rec_Adb, "            raise SOAP.SOAP_Error");
                  Text_IO.Put_Line
                    (Rec_Adb, "               with SOAP.Types.Name (R)");
                  Text_IO.Put_Line
                    (Rec_Adb, "                  & ""."
                     & F_Name & " expected "
                     & I_Type & ", """);
                  if Is_Choice then
                     Text_IO.Put_Line
                       (Rec_Adb,
                        "                  & ""found "" & External_Tag ("
                        & "E'Tag);");
                  else
                     Text_IO.Put_Line
                       (Rec_Adb,
                        "                  & ""found "" & External_Tag ("
                        & F_Name & "'Tag);");
                  end if;
                  Text_IO.Put_Line
                    (Rec_Adb, "         end if;");
               end Emit_Check;

            begin
               Text_IO.Put_Line (Rec_Adb, "   exception");
               Text_IO.Put_Line (Rec_Adb, "      when Constraint_Error =>");

               Count := 0;

               while N /= null loop
                  Count := Count + 1;
                  declare
                     Def    : constant WSDL.Types.Definition :=
                                WSDL.Types.Find (N.Typ);
                     T_Name : constant String := WSDL.Types.Name (Def.Ref);
                  begin
                     case N.Mode is
                        when WSDL.Types.K_Simple =>
                           Emit_Check
                             (Format_Name (O, To_String (N.Name)),
                              SOAP.WSDL.Set_Type (SOAP.WSDL.To_Type (T_Name)));

                        when WSDL.Types.K_Derived =>
                           Emit_Check
                             (Format_Name (O, To_String (N.Name)),
                              SOAP.WSDL.Set_Type
                                (SOAP.WSDL.To_Type
                                  (WSDL.Types.Root_Type_For (Def))));

                        when WSDL.Types.K_Enumeration =>
                           Emit_Check
                             (Format_Name (O, To_String (N.Name)),
                              "SOAP.Types.SOAP_Enumeration");

                        when WSDL.Types.K_Array =>
                           if WSDL.Parameters.Is_Uniq (N.all) then
                              Emit_Check
                                (Format_Name (O, To_String (N.Name)),
                                 "SOAP.Types.SOAP_Array");
                           end if;

                        when WSDL.Types.K_Record =>
                           Emit_Check
                             (Format_Name (O, To_String (N.Name)),
                              "SOAP.Types.SOAP_Record");
                     end case;
                  end;
                  N := N.Next;
               end loop;
            end;

            Text_IO.Put_Line
              (Rec_Adb,
               "         raise SOAP.SOAP_Error");
            Text_IO.Put_Line
              (Rec_Adb,
               "            with ""Record "" & SOAP.Types.Name (R) &"
               & " "" not well formed"";");
         end if;

         Text_IO.Put_Line (Rec_Adb, "   end To_" & F_Name & ';');

         --  To_SOAP_Object

         Text_IO.New_Line (Rec_Adb);
         Text_IO.Put_Line (Rec_Adb, "   function To_SOAP_Object");

         Text_IO.Put_Line (Rec_Adb, "     (R         : " & F_Name & ';');
         Text_IO.Put_Line (Rec_Adb, "      Name      : String := ""item"";");
         Text_IO.Put_Line
           (Rec_Adb, "      Type_Name : String := Q_Type_Name;");
         Text_IO.Put_Line
           (Rec_Adb, "      NS        : SOAP.Name_Space.Object := "
            & "Name_Space)");
         Text_IO.Put_Line (Rec_Adb, "      return SOAP.Types.SOAP_Record");
         Text_IO.Put_Line (Rec_Adb, "   is");
         Text_IO.Put_Line (Rec_Adb, "      Result : SOAP.Types.SOAP_Record;");
         Text_IO.Put_Line (Rec_Adb, "   begin");

         N := R;

         if Is_Choice and then N /= null then
            Text_IO.Put_Line (Rec_Adb, "      case R.C is");
         else
            Text_IO.Put_Line (Rec_Adb, "      Result := SOAP.Types.R");
         end if;

         if N = null then
            Text_IO.Put_Line
              (Rec_Adb, "        (SOAP.Types.Empty_Object_Set,");

         else
            Count := 0;

            while N /= null loop
               Count := Count + 1;

               if Is_Choice then
                  Text_IO.Put_Line
                    (Rec_Adb,
                     "         when C" & AWS.Utils.Image (Count) & " =>");
                  Text_IO.Put_Line
                    (Rec_Adb, "            Result := SOAP.Types.R");
                  Text_IO.Put
                    (Rec_Adb, "              ((1 => +");

               else
                  if N = R then
                     if R.Next = null or else Is_Choice then
                        --  We have a single element into this record, we must
                        --  use a named notation for the aggregate.
                        Text_IO.Put (Rec_Adb, "        ((1 => +");
                     else
                        Text_IO.Put (Rec_Adb, "        ((+");
                     end if;

                  else
                     Text_IO.Put      (Rec_Adb, "          +");
                  end if;
               end if;

               --  All fields in the record are using the name-space of the
               --  record itself.

               declare
                  T_Name : constant String :=
                             WSDL.Types.Name (N.Typ, NS => True);
                  Field  : constant String :=
                             "R." & Format_Name (O, To_String (N.Name));
                  NS     : constant SOAP.Name_Space.Object :=
                             WSDL.Types.NS (N.Typ);
                  NS_Ref : constant String :=
                             To_Unit_Name (Generate_Namespace (NS, False))
                             & ".Name_Space";
               begin
                  case N.Mode is
                     when WSDL.Types.K_Simple =>
                        Text_IO.Put
                          (Rec_Adb,
                           WSDL.Parameters.To_SOAP
                             (N.all,
                              Object    => Field,
                              Name      => To_String (N.Name),
                              Type_Name => T_Name,
                              NS        => "NS"));

                     when WSDL.Types.K_Record =>
                        Text_IO.Put
                          (Rec_Adb,
                           WSDL.Parameters.To_SOAP
                             (N.all,
                              Object    => Field,
                              Name      => To_String (N.Name),
                              Type_Name => T_Name,
                              NS        => NS_Ref));

                     when WSDL.Types.K_Derived =>
                        Text_IO.Put
                          (Rec_Adb,
                           WSDL.Parameters.To_SOAP
                             (N.all,
                              Object    => Field,
                              Name      => To_String (N.Name),
                              Type_Name => T_Name,
                              NS        => NS_Ref));

                     when WSDL.Types.K_Enumeration =>
                        Text_IO.Put
                          (Rec_Adb,
                           WSDL.Parameters.To_SOAP
                             (N.all,
                              Object    => Field,
                              Name      => To_String (N.Name),
                              Type_Name => Format_Name (O, T_Name),
                              NS        => "NS"));

                     when WSDL.Types.K_Array =>
                        declare
                           E_Name : constant String :=
                                      To_String (N.Elmt_Name);
                        begin
                           Text_IO.Put
                             (Rec_Adb,
                              WSDL.Parameters.To_SOAP
                                (N.all,
                                 Object    => Field & ".Item.all",
                                 Name      => To_String (N.Name),
                                 Type_Name =>
                                   (if O.Style = SOAP.WSDL.Schema.RPC
                                    then E_Name
                                    else T_Name),
                                 NS        => NS_Ref));
                        end;
                  end case;
               end;

               if N.Next = null or else Is_Choice then
                  Text_IO.Put_Line (Rec_Adb, "),");

                  if Is_Choice then
                     Text_IO.Put (Rec_Adb, "      ");
                  end if;

                  Text_IO.Put_Line
                    (Rec_Adb,
                     "         Name, Q_Type_Name, NS => NS);");

               else
                  Text_IO.Put_Line (Rec_Adb, ",");
               end if;

               N := N.Next;
            end loop;
         end if;

         if R = null then
            Text_IO.Put_Line
              (Rec_Adb,
               "         Name, Q_Type_Name, NS => NS);");
         elsif Is_Choice then
            Text_IO.Put_Line (Rec_Adb, "      end case;");
         end if;

         Text_IO.Put_Line (Rec_Adb, "      return Result;");
         Text_IO.Put_Line (Rec_Adb, "   end To_SOAP_Object;");

         Finalize_Types_Package (Prefix, Rec_Ads, Rec_Adb);
      end Generate_Record;

      -------------------------
      -- Generate_References --
      -------------------------

      procedure Generate_References
        (File        : Text_IO.File_Type;
         P           : WSDL.Parameters.P_Set;
         For_Derived : Boolean := False)
      is
         procedure Output_Refs (Def : WSDL.Types.Definition; Gen : Boolean);
         --  Recursivelly output with/use clauses for derived types

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

                  With_Unit
                    (File,
                     To_Unit_Name
                       (Generate_Namespace (WSDL.Types.NS (Def.Ref), True)),
                     Elab       => Off,
                     Use_Clause => True);
               else
                  With_Unit
                    (File,
                     To_Unit_Name (Prefix) & '.' & F_Name & "_Type_Pkg",
                     Elab       => Off,
                     Use_Clause => True);
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
      end Generate_References;

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
               return "To_" & Type_Name (P);
         end case;
      end Get_Routine;

      ------------------------------
      -- Initialize_Types_Package --
      ------------------------------

      procedure Initialize_Types_Package
        (P            : WSDL.Parameters.P_Set;
         Name         : String;
         Output       : Boolean;
         Prefix       : out Unbounded_String;
         F_Ads, F_Adb : out Text_IO.File_Type;
         Def          : WSDL.Types.Definition := WSDL.Types.No_Definition;
         Regen        : Boolean := False)
      is
         use type WSDL.Types.Definition;
         use WSDL.Parameters;
         F_Name : constant String := Name & "_Pkg";
      begin
         Prefix := To_Unbounded_String
           (Generate_Namespace
              (WSDL.Types.NS
                (if Def = WSDL.Types.No_Definition
                 then P.Typ
                 else Def.Ref), True) & '-' & F_Name);

         --  Add references into the main types package

         if not Regen then
            With_Unit
              (Type_Ads,
               To_Unit_Name (To_String (Prefix)),
               Use_Clause => True);
         end if;

         Text_IO.Create
           (F_Ads, Text_IO.Out_File, To_Lower (To_String (Prefix)) & ".ads");

         Text_IO.Create
           (F_Adb, Text_IO.Out_File, To_Lower (To_String (Prefix)) & ".adb");

         Put_File_Header (O, F_Ads);

         --  Either a compound type or an anonymous returned compound type

         if Output then
            Generate_References (F_Ads, P);
         elsif P.Mode in WSDL.Types.Compound_Type then
            Generate_References (F_Ads, P.P);
         end if;

         if Def.Mode = WSDL.Types.K_Derived then
            if SOAP.WSDL.Is_Standard (WSDL.Types.Name (Def.Parent)) then
               With_Unit
                 (F_Ads,
                  To_Unit_Name
                    (Generate_Namespace (WSDL.Types.NS (Def.Parent), True)),
                  Elab       => Off,
                  Use_Clause => True);
            else
               With_Unit
                 (F_Ads,
                  To_Unit_Name
                    (Generate_Namespace (WSDL.Types.NS (Def.Parent), False))
                  & '.' & WSDL.Types.Name (Def.Parent) & "_Type_Pkg",
                  Elab       => Off,
                  Use_Clause => True);
            end if;

            Text_IO.New_Line (F_Ads);
         end if;

         Put_Types_Header_Spec
           (O, F_Ads, To_Unit_Name (To_String (Prefix)), Is_NS => True);

         Put_File_Header (O, F_Adb);
         Put_Types_Header_Body (O, F_Adb, To_Unit_Name (To_String (Prefix)));

         --  Generate qualified type name

         if Def.Mode /= WSDL.Types.K_Simple then
            Text_IO.New_Line (F_Ads);
            Text_IO.Put_Line
              (F_Ads,
               "   Q_Type_Name : constant String := """
               & WSDL.Types.Name
                 ((if Def = WSDL.Types.No_Definition
                   then P.Typ
                   else Def.Ref), NS => True) & """;");
         end if;
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

      ---------------
      -- Type_Name --
      ---------------

      function Type_Name (N : WSDL.Parameters.P_Set) return String is
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
               return Format_Name (O, T_Name) & "_Type_Safe_Access";

            when WSDL.Types.K_Record =>
               return Format_Name (O, T_Name) & "_Type";
         end case;
      end Type_Name;

      L_Proc : constant String := Format_Name (O, Proc);

   begin
      Output_Types (Input);

      Output_Types (Output);

      Text_IO.Put_Line (Type_Adb, "   --  Definitions for procedure " & Proc);

      Output_Schema_Definition
        (Key   => '@' & To_String (O.Prefix) & W_Name & ".encoding",
         Value => SOAP.Types.Encoding_Style'Image
                    (O.Encoding (WSDL.Parser.Input)));

      Output_Schema_Definition
        (Key   => '@' & To_String (O.Prefix) & W_Name & "Response.encoding",
         Value =>
           SOAP.Types.Encoding_Style'Image (O.Encoding (WSDL.Parser.Output)));

      Output_Schema_Definition
        (Key   => '@' & To_String (O.Prefix)
                  & SOAP.Utils.No_NS (W_Name) & ".encoding",
         Value => SOAP.Types.Encoding_Style'Image
                    (O.Encoding (WSDL.Parser.Input)));

      Output_Schema_Definition
        (Key   => '@' & To_String (O.Prefix)
                  & SOAP.Utils.No_NS (W_Name) & "Response.encoding",
         Value =>
           SOAP.Types.Encoding_Style'Image (O.Encoding (WSDL.Parser.Output)));

      if Output /= null then
         --  Something in the SOAP procedure output

         Output_Schema_Definition
           (Key   => '@' & To_String (Output.Name) & ".encoding",
            Value =>
              SOAP.Types.Encoding_Style'Image
                (O.Encoding (WSDL.Parser.Output)));

         if Output.Next = null then
            --  A single parameter

            if Output.Mode /= WSDL.Types.K_Simple then
               declare
                  Def : constant WSDL.Types.Definition :=
                          WSDL.Types.Find (Output.Typ, False);
               begin
                  Text_IO.New_Line (Tmp_Ads);

                  Text_IO.Put_Line
                    (Tmp_Ads,
                     "   subtype " & L_Proc & "_Result is "
                     & Format_Name
                       (O,
                        SOAP.Utils.To_Name
                          (WSDL.Types.Name
                            (Output.Typ, Def.Mode = WSDL.Types.K_Derived)))
                     & "_Type;");
               end;
            end if;

         else
            --  Multiple parameters in the output, generate a record in this
            --  case.

            Generate_Record (L_Proc, "_Result", Output, Is_Output => True);
         end if;
      end if;
   end Put_Types;

   ---------------------------
   -- Put_Types_Header_Body --
   ---------------------------

   procedure Put_Types_Header_Body
     (O : Object; File : Text_IO.File_Type; Unit_Name : String)
   is
      pragma Unreferenced (O);
   begin
      With_Unit (File, "Ada.Tags", Elab => Off);
      Text_IO.New_Line (File);

      Text_IO.Put_Line
        (File, "package body " & Unit_Name & " is");
      Text_IO.New_Line (File);
      Text_IO.Put_Line (File, "   use Ada.Tags;");
      Text_IO.Put_Line (File, "   use SOAP.Types;");
      Text_IO.New_Line (File);
   end Put_Types_Header_Body;

   ---------------------------
   -- Put_Types_Header_Spec --
   ---------------------------

   procedure Put_Types_Header_Spec
     (O         : Object;
      File      : Text_IO.File_Type;
      Unit_Name : String;
      Elab_Body : Boolean := False;
      Is_NS     : Boolean := False) is
   begin
      With_Unit (File, "Ada.Calendar", Elab => Off);
      With_Unit (File, "Ada.Strings.Unbounded", Elab => Off);
      Text_IO.New_Line (File);

      if not Is_NS then
         With_Unit (File, "SOAP.Name_Space");
      end if;
      With_Unit (File, "SOAP.Types", Elab => Children);
      With_Unit (File, "SOAP.Utils");
      Text_IO.New_Line (File);
      With_Unit (File, "GNAT.Regexp");
      Text_IO.New_Line (File);

      if Types_Spec (O) /= "" then
         With_Unit (File, Types_Spec (O, With_Clause => True));
         Text_IO.New_Line (File);
      end if;

      if Procs_Spec (O) /= "" and then Procs_Spec (O) /= Types_Spec (O) then
         With_Unit (File, Procs_Spec (O, With_Clause => True));
         Text_IO.New_Line (File);
      end if;

      Text_IO.Put_Line
        (File, "package " & Unit_Name & " is");
      Text_IO.New_Line (File);

      if Elab_Body then
         Text_IO.Put_Line (File, "   pragma Elaborate_Body;");
      end if;

      Text_IO.Put_Line (File, "   pragma Warnings (Off, Ada.Calendar);");
      Text_IO.Put_Line
        (File, "   pragma Warnings (Off, Ada.Strings.Unbounded);");
      Text_IO.Put_Line (File, "   pragma Warnings (Off, SOAP.Types);");
      Text_IO.Put_Line (File, "   pragma Warnings (Off, SOAP.Utils);");
      Text_IO.Put_Line (File, "   pragma Warnings (Off, GNAT.Regexp);");

      if Types_Spec (O) /= "" then
         Text_IO.Put_Line
           (File,
            "   pragma Warnings (Off, " & Types_Spec (O) & ");");
         Text_IO.New_Line (File);
      end if;

      if Procs_Spec (O) /= "" and then Procs_Spec (O) /= Types_Spec (O) then
         Text_IO.Put_Line
           (File,
            "   pragma Warnings (Off, " & Procs_Spec (O) & ");");
         Text_IO.New_Line (File);
      end if;

      Text_IO.New_Line (File);
      Text_IO.Put_Line (File, "   pragma Style_Checks (Off);");
      Text_IO.New_Line (File);
      Text_IO.Put_Line (File, "   use Ada.Strings.Unbounded;");
      Text_IO.New_Line (File);
      Text_IO.Put_Line (File, "   function ""+""");
      Text_IO.Put_Line (File, "     (Str : String)");
      Text_IO.Put_Line (File, "      return Unbounded_String");
      Text_IO.Put_Line (File, "      renames To_Unbounded_String;");
      Text_IO.Put_Line (File, "   function ""-""");
      Text_IO.Put_Line (File, "     (Str : Unbounded_String)");
      Text_IO.Put_Line (File, "      return String");
      Text_IO.Put_Line (File, "      renames To_String;");
   end Put_Types_Header_Spec;

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

      U_Name : constant String := To_Unit_Name (Format_Name (O, Name));

      procedure Create (File : in out Text_IO.File_Type; Filename : String);
      --  Create Filename, raise execption Generator_Error if the file already
      --  exists and overwrite mode not activated.

      procedure Generate_Main (Filename : String);
      --  Generate the main server's procedure. Either the file exists and is
      --  a template use it to generate the main otherwise just generate a
      --  standard main procedure.

      function Timeout_Image (Timeout : Duration) return String;

      ------------
      -- Create --
      ------------

      procedure Create
        (File     : in out Text_IO.File_Type;
         Filename : String) is
      begin
         if AWS.Utils.Is_Regular_File (Filename) and then not O.Force then
            raise Generator_Error
              with "File " & Filename & " exists, activate overwrite mode.";
         else
            Text_IO.Create (File, Text_IO.Out_File, Filename);
         end if;
      end Create;

      -------------------
      -- Generate_Main --
      -------------------

      procedure Generate_Main (Filename : String) is
         use Text_IO;

         L_Filename        : constant String :=
                               Characters.Handling.To_Lower (Filename);
         Template_Filename : constant String := L_Filename & ".amt";

         File : Text_IO.File_Type;

      begin
         Create (File, L_Filename & ".adb");

         Put_File_Header (O, File);

         if AWS.Utils.Is_Regular_File (Template_Filename) then
            --  Use template file
            declare
               Translations : constant Templates.Translate_Table :=
                                (1 => Templates.Assoc
                                   ("SOAP_SERVICE", U_Name),
                                 2 => Templates.Assoc
                                   ("SOAP_VERSION", SOAP.Version),
                                 3 => Templates.Assoc
                                   ("AWS_VERSION",  AWS.Version),
                                 4 => Templates.Assoc
                                   ("UNIT_NAME", To_Unit_Name (Filename)));
            begin
               Put (File, Templates.Parse (Template_Filename, Translations));
            end;

         else
            --  Generate a minimal main for the server
            With_Unit (File, "AWS.Config.Set");
            With_Unit (File, "AWS.Server");
            With_Unit (File, "AWS.Status");
            With_Unit (File, "AWS.Response");
            With_Unit (File, "SOAP.Dispatchers.Callback");
            New_Line (File);
            With_Unit (File, U_Name & ".CB");
            With_Unit (File, U_Name & ".Server");
            New_Line (File);
            Put_Line (File, "procedure " & To_Unit_Name (Filename) & " is");
            New_Line (File);
            Put_Line (File, "   use AWS;");
            New_Line (File);
            Put_Line (File, "   function CB ");
            Put_Line (File, "      (Request : Status.Data)");
            Put_Line (File, "       return Response.Data");
            Put_Line (File, "   is");
            Put_Line (File, "      R : Response.Data;");
            Put_Line (File, "   begin");
            Put_Line (File, "      return R;");
            Put_Line (File, "   end CB;");
            New_Line (File);
            Put_Line (File, "   WS   : AWS.Server.HTTP;");
            Put_Line (File, "   Conf : Config.Object := Config.Get_Current;");
            Put_Line (File, "   Disp : " & U_Name & ".CB.Handler;");
            New_Line (File);
            Put_Line (File, "begin");
            Put_Line (File, "   Config.Set.Server_Port");
            Put_Line (File, "      (Conf, " & U_Name & ".Server.Port);");
            Put_Line (File, "   Disp := SOAP.Dispatchers.Callback.Create");
            Put_Line (File, "     (CB'Unrestricted_Access,");
            Put_Line (File, "      " & U_Name & ".CB.SOAP_CB'Access,");
            Put_Line (File, "      " & U_Name & ".Schema);");
            New_Line (File);
            Put_Line (File, "   AWS.Server.Start (WS, Disp, Conf);");
            New_Line (File);
            Put_Line (File, "   AWS.Server.Wait (AWS.Server.Forever);");
            Put_Line (File, "end " & To_Unit_Name (Filename) & ";");
         end if;

         Close_File (File);
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
      O.Location := To_Unbounded_String (Location);

      if not O.Quiet then
         Text_IO.New_Line;
         Text_IO.Put_Line ("Service " & Name);
         Text_IO.Put_Line ("   " & Root_Documentation);
      end if;

      Create (Root, LL_Name & ".ads");

      Create (Type_Ads, LL_Name & "-types.ads");
      Text_IO.Create (Tmp_Ads, Text_IO.Out_File);

      Create (Type_Adb, LL_Name & "-types.adb");

      if O.Gen_Stub then
         Create (Stub_Ads, LL_Name & "-client.ads");
         Create (Stub_Adb, LL_Name & "-client.adb");
      end if;

      if O.Gen_Skel then
         Create (Skel_Ads, LL_Name & "-server.ads");
         Create (Skel_Adb, LL_Name & "-server.adb");
      end if;

      if O.Gen_CB then
         Create (CB_Ads, LL_Name & "-cb.ads");
         Create (CB_Adb, LL_Name & "-cb.adb");
      end if;

      --  Types

      Put_File_Header (O, Type_Ads);
      Put_Types_Header_Spec (O, Tmp_Ads, U_Name & ".Types", Elab_Body => True);

      Put_File_Header (O, Type_Adb);
      Put_Types_Header_Body (O, Type_Adb, U_Name & ".Types");

      --  We have only elaboration code to fill the Schema map with the
      --  definitions needed to parse literal SOAP messages.

      Text_IO.Put_Line (Type_Adb, "begin");

      --  Root

      Put_File_Header (O, Root);

      if Root_Documentation /= "" then
         Output_Comment (Root, Root_Documentation, Indent => 0);
         Text_IO.New_Line (Root);
      end if;

      With_Unit (Root, "AWS.Client");
      With_Unit (Root, "SOAP.WSDL.Schema");
      Text_IO.New_Line (Root);

      Text_IO.Put_Line (Root, "package " & U_Name & " is");
      Text_IO.New_Line (Root);

      if O.Endpoint = Null_Unbounded_String then
         Text_IO.Put_Line
           (Root,
            "   URL      : constant String := """ & Location & """;");
      else
         Text_IO.Put_Line
           (Root,
            "   URL      : constant String := """
            & To_String (O.Endpoint) & """;");
      end if;

      Text_IO.Put_Line
        (Root,
         "   Timeouts : constant AWS.Client.Timeouts_Values :=");

      if O.Timeouts = Client.No_Timeout then
         Text_IO.Put_Line
           (Root, "                AWS.Client.No_Timeout;");

      else
         Text_IO.Put_Line
           (Root, "                AWS.Client.Timeouts");
         Text_IO.Put_Line
           (Root, "                  (Connect  => "
            & Timeout_Image (Client.Connect_Timeout (O.Timeouts)) & ',');
         Text_IO.Put_Line
           (Root, "                   Send     => "
            & Timeout_Image (Client.Send_Timeout (O.Timeouts)) & ',');
         Text_IO.Put_Line
           (Root, "                   Receive  => "
            & Timeout_Image (Client.Receive_Timeout (O.Timeouts)) & ',');
         Text_IO.Put_Line
           (Root, "                   Response => "
            & Timeout_Image (Client.Response_Timeout (O.Timeouts)) & ");");
      end if;

      Text_IO.New_Line (Root);
      Text_IO.Put_Line
        (Root, "   Schema   : SOAP.WSDL.Schema.Definition;");

      --  Add namespaces in schema

      Text_IO.Put_Line (Type_Adb, "   --  Definitions for SOAP name-spaces");
      Output_Schema_Definition
        (Key   => SOAP.Name_Space.Value (O.xsd),
         Value => SOAP.Utils.No_NS (SOAP.Name_Space.Name (O.xsd)));
      Output_Schema_Definition
        (Key   => SOAP.Name_Space.Value (O.xsi),
         Value => SOAP.Utils.No_NS (SOAP.Name_Space.Name (O.xsi)));
      Output_Schema_Definition
        (Key   => SOAP.Name_Space.Value (O.env),
         Value => SOAP.Utils.No_NS (SOAP.Name_Space.Name (O.env)));
      Output_Schema_Definition
        (Key   => SOAP.Name_Space.Value (O.enc),
         Value => SOAP.Utils.No_NS (SOAP.Name_Space.Name (O.enc)));
      Text_IO.New_Line (Type_Adb);

      --  Then the user's name-spaces

      declare
         procedure Write_NS (Key, Value : String);

         --------------
         -- Write_NS --
         --------------

         procedure Write_NS (Key, Value : String) is
         begin
            Output_Schema_Definition (Key, Value);
         end Write_NS;

      begin
         SOAP.WSDL.Name_Spaces.Iterate (Write_NS'Access);
         Text_IO.New_Line (Type_Adb);
      end;

      if O.WSDL_File /= Null_Unbounded_String then
         Text_IO.New_Line (Root);
         Text_IO.Put_Line (Root, "   pragma Style_Checks (Off);");

         declare
            File   : Text_IO.File_Type;
            Buffer : String (1 .. 1_024);
            Last   : Natural;
         begin
            Text_IO.Open (File, Text_IO.In_File, To_String (O.WSDL_File));

            while not Text_IO.End_Of_File (File) loop
               Text_IO.Get_Line (File, Buffer, Last);
               Text_IO.Put_Line (Root, "--  " & Buffer (1 .. Last));
            end loop;

            Close_File (File);
         end;

         Text_IO.Put_Line (Root, "   pragma Style_Checks (On);");
         Text_IO.New_Line (Root);
      end if;

      O.Unit := To_Unbounded_String (U_Name);

      --  Stubs

      if O.Gen_Stub then
         Put_File_Header (O, Stub_Ads);
         Put_File_Header (O, Stub_Adb);
         Stub.Start_Service
           (O, Name, Root_Documentation, Documentation, Location);
      end if;

      --  Skeletons

      if O.Gen_Skel then
         Put_File_Header (O, Skel_Ads);
         Put_File_Header (O, Skel_Adb);
         Skel.Start_Service
           (O, Name, Root_Documentation, Documentation, Location);
      end if;

      --  Callbacks

      if O.Gen_CB then
         Put_File_Header (O, CB_Ads);
         Put_File_Header (O, CB_Adb);
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
   -- With_Unit --
   ---------------

   procedure With_Unit
     (File       : Text_IO.File_Type;
      Name       : String;
      Elab       : Elab_Pragma := Single;
      Use_Clause : Boolean := False)
   is
      Key : constant String := Text_IO.Name (File) & "?" & Name;
   begin
      if not Withed_Unit.Contains (Key) then
         Withed_Unit.Append (Key);

         Text_IO.Put_Line (File, "with " & Name & ';');

         if Elab = Children then
            declare
               Index : Natural := Name'First;
            begin
               loop
                  Index :=
                    Strings.Fixed.Index (Name (Index .. Name'Last), ".");
                  exit when Index = 0;

                  Text_IO.Put_Line
                    (File,
                     "pragma Elaborate_All (" & Name (Name'First .. Index - 1)
                     & ");");
                  Index := Index + 1;
               end loop;
            end;
         end if;

         case Elab is
            when Off =>
               null;

            when Single | Children =>
               Text_IO.Put_Line (File, "pragma Elaborate_All (" & Name & ");");
         end case;

         if Use_Clause then
            Text_IO.Put_Line (File, "use " & Name & ';');
            Text_IO.New_Line (File);
         end if;
      end if;
   end With_Unit;

   ---------------
   -- WSDL_File --
   ---------------

   procedure WSDL_File (O : in out Object; Filename : String) is
   begin
      O.WSDL_File := To_Unbounded_String (Filename);
   end WSDL_File;

end WSDL2AWS.Generator;
