------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2003-2005                          --
--                                 AdaCore                                  --
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

with Ada.Calendar;
with Ada.Exceptions;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with GNAT.Calendar.Time_IO;

with AWS.Utils;
with SOAP.Name_Space;
with SOAP.Types;
with SOAP.WSDL;

with Strings_Maps;

with Ada2WSDL.Options;

package body Ada2WSDL.Generator is

   use Ada;
   use Ada.Strings.Unbounded;
   use SOAP;

   --  Data structure for API's description

   type Parameter;
   type Parameter_Access is access Parameter;

   type Parameter is record
      Name      : Unbounded_String;
      Type_Name : Unbounded_String;
      XSD_Name  : Unbounded_String;
      Next      : Parameter_Access;
   end record;

   type Mode is (Routine, Safe_Pointer_Definition,
                 Structure, Table, Derived, Enumeration);

   type Definition (Def_Mode : Mode := Routine) is record
      Name        : Unbounded_String;
      NS          : Unbounded_String;
      Parameters  : Parameter_Access;
      Last        : Parameter_Access;

      case Def_Mode is
         when Routine =>
            Return_Type : Parameter_Access;

         when Table =>
            Length : Natural;

         when Structure | Derived | Enumeration =>
            null;

         when Safe_Pointer_Definition =>
            Type_Name, Access_Name : Unbounded_String;
      end case;
   end record;

   type Profiles is array (Positive range <>) of Definition;

   Max_Definition : constant := 1_024;

   API   : Profiles (1 .. Max_Definition);
   Index : Natural := 0;

   Schema_Needed    : Boolean := False;
   --  Set to True if a WSDL schema is to be writed

   Character_Schema : Boolean := False;
   --  Set to Trus if a WSDL Character schema must be generated

   package NS_Maps is new Strings_Maps (Positive);
   use NS_Maps;

   Name_Spaces : NS_Maps.Map;
   NS_Num      : Natural := 0;

   procedure Insert_NS (Value : in String);
   --  Insert a new namespace value into Name_Spaces table

   function NS_Prefix (Value : in String) return String;
   --  Returns the name space prefix for the given name space value

   function "+" (S : in String) return Unbounded_String
      renames To_Unbounded_String;

   function "-" (S : in Unbounded_String) return String
      renames To_String;

   function To_XSD (NS, Ada_Type : in String) return String;

   procedure Check_Routine (Name : in String);
   --  Checks the routine name, no overloading is allowed, raises
   --  Constraint_Error if name already exits.

   -------------------
   -- Check_Routine --
   -------------------

   procedure Check_Routine (Name : in String) is
      use Exceptions;
   begin
      for I in 1 .. Index loop
         if API (I).Def_Mode = Routine
           and then To_String (API (I).Name) = Name
         then
            Raise_Exception
              (Spec_Error'Identity,
               "routine " & Name & " already found, no overloading allowed.");
         end if;
      end loop;
   end Check_Routine;

   ---------------
   -- Insert_NS --
   ---------------

   procedure Insert_NS (Value : in String) is
      P       : Cursor;
      Success : Boolean;
   begin
      if not NS_Maps.Containers.Is_In (Value, Name_Spaces) then
         NS_Num := NS_Num + 1;
         NS_Maps.Containers.Insert (Name_Spaces, Value, NS_Num, P, Success);
      end if;
   end Insert_NS;

   -------------------
   -- New_Component --
   -------------------

   procedure New_Component (NS, Comp_Name, Comp_Type : in String) is

      function Check_Safe_Pointer (Type_Name : in String) return String;

      ------------------------
      -- Check_Safe_Pointer --
      ------------------------

      function Check_Safe_Pointer (Type_Name : in String) return String is
      begin
         for I in 1 .. Index loop
            if API (I).Def_Mode = Safe_Pointer_Definition then
               if To_String (API (I).Name) & ".Safe_Pointer" = Type_Name then
                  return To_String (API (I).Type_Name);
               end if;
            end if;
         end loop;

         return Type_Name;
      end Check_Safe_Pointer;

      L_Comp_Type : constant String := Check_Safe_Pointer (Comp_Type);

      New_P       : constant Parameter_Access
        := new Parameter'(+Comp_Name, +L_Comp_Type,
                          +To_XSD (NS, L_Comp_Type), null);
   begin
      if Options.Verbose then
         Text_IO.Put_Line
           ("        " & Comp_Name & " : " & L_Comp_Type
              & " (" & (-New_P.XSD_Name) & ')');
      end if;

      if API (Index).Parameters = null then
         API (Index).Parameters := New_P;
      else
         API (Index).Last.Next := New_P;
      end if;

      API (Index).Last := New_P;
   end New_Component;

   ----------------
   -- New_Formal --
   ----------------

   procedure New_Formal (NS, Var_Name, Var_Type : in String) is
      New_P : constant Parameter_Access
        := new Parameter'
                 (+Var_Name, +Var_Type, +To_XSD (NS, Var_Type), null);
   begin
      if Options.Verbose then
         Text_IO.Put_Line
           ("        " & Var_Name & " : " & Var_Type
              & " (" & (-New_P.XSD_Name) & ')');
      end if;

      if API (Index).Parameters = null then
         API (Index).Parameters := New_P;
      else
         API (Index).Last.Next := New_P;
      end if;

      API (Index).Last := New_P;
   end New_Formal;

   -----------------
   -- New_Literal --
   -----------------

   procedure New_Literal (Name : in String) is
      New_P : constant Parameter_Access
        := new Parameter'(+Name, +"", +"", null);
   begin
      if Options.Verbose then
         Text_IO.Put_Line ("        " & Name);
      end if;

      if API (Index).Parameters = null then
         API (Index).Parameters := New_P;
      else
         API (Index).Last.Next := New_P;
      end if;

      API (Index).Last := New_P;
   end New_Literal;

   ---------------
   -- NS_Prefix --
   ---------------

   function NS_Prefix (Value : in String) return String is
      use AWS;
   begin
      if Value = "" then
         return "tns";
      else
         return 'n' &
           Utils.Image (NS_Maps.Containers.Element (Name_Spaces, Value));
      end if;
   end NS_Prefix;

   ----------------------
   -- Register_Derived --
   ----------------------

   procedure Register_Derived (NS, Name, Parent_Name : in String) is
      New_P : constant Parameter_Access
        := new Parameter'
          (+Name, +Parent_Name, +To_XSD (NS, Parent_Name), null);

      D : Definition (Derived);
   begin
      --  We need to write a schema for this derived type
      Schema_Needed := True;

      D.NS         := +NS;
      D.Name       := +Name;
      D.Parameters := New_P;

      Index := Index + 1;
      API (Index) := D;

      if not Options.Quiet then
         Text_IO.Put
           ("   - derived         " & Name & " is new " & Parent_Name);

         if Options.Verbose then
            Text_IO.Put_Line (" (" & (-New_P.XSD_Name) & ')');
         else
            Text_IO.New_Line;
         end if;
      end if;
   end Register_Derived;

   ---------------------------
   -- Register_Safe_Pointer --
   ---------------------------

   procedure Register_Safe_Pointer
     (Name, Type_Name, Access_Name : in String)
   is
      use Exceptions;
      D : Definition (Safe_Pointer_Definition);
   begin
      if Name /= Type_Name & "_Safe_Pointer" then
         Raise_Exception
           (Spec_Error'Identity,
            "Package Safe_Pointers instantiation must be named "
              & Type_Name & "_Safe_Pointer.");
      end if;

      D.Name        := +Name;
      D.Type_Name   := +Type_Name;
      D.Access_Name := +Access_Name;

      Index := Index + 1;
      API (Index) := D;

      if not Options.Quiet then
         Text_IO.Put_Line
           ("   - safe pointer  " & Name
              & " (" & Type_Name & ", " & Access_Name & ")");
      end if;
   end Register_Safe_Pointer;

   -----------------
   -- Return_Type --
   -----------------

   procedure Return_Type (NS, Name : in String) is
      New_P : constant Parameter_Access
        := new Parameter'(+"Result", +Name, +To_XSD (NS, Name), null);
   begin
      if Options.Verbose then
         Text_IO.Put_Line
           ("        return " & Name & " (" & (-New_P.XSD_Name) & ')');
      end if;

      API (Index).Return_Type := New_P;
   end Return_Type;

   -----------------
   -- Start_Array --
   -----------------

   procedure Start_Array
     (NS, Name, Component_Type : in String;
      Length                   : in Natural := 0)
   is
      New_P : constant Parameter_Access
        := new Parameter'(+"item", +Component_Type,
                          +To_XSD (NS, Component_Type), null);

      D : Definition (Table);
   begin
      --  We need to write a schema for this record
      Schema_Needed := True;

      D.NS         := +NS;
      D.Name       := +Name;
      D.NS         := +NS;
      D.Parameters := New_P;
      D.Length     := Length;

      Index := Index + 1;
      API (Index) := D;

      if not Options.Quiet then
         Text_IO.Put ("   - array (");
         if Length = 0 then
            --  An unconstrained array
            Text_IO.Put ("<>");
         else
            Text_IO.Put (AWS.Utils.Image (Length));
         end if;
         Text_IO.Put (")");
         Text_IO.Set_Col (22);
         Text_IO.Put (Name & " of " & Component_Type);

         if Options.Verbose then
            Text_IO.Put_Line (" (" & (-New_P.XSD_Name) & ')');
         else
            Text_IO.New_Line;
         end if;
      end if;
   end Start_Array;

   -----------------------
   -- Start_Enumeration --
   -----------------------

   procedure Start_Enumeration (NS, Name : in String) is
      D : Definition (Enumeration);
   begin
      --  We need to write a schema for this derived type
      Schema_Needed := True;

      Insert_NS (NS);

      D.NS         := +NS;
      D.Name       := +Name;
      D.Parameters := null;

      Index := Index + 1;
      API (Index) := D;

      if not Options.Quiet then
         Text_IO.Put_Line ("   - enumeration     " & Name);
      end if;
   end Start_Enumeration;

   ------------------
   -- Start_Record --
   ------------------

   procedure Start_Record (NS, Name : in String) is
      D : Definition (Structure);
   begin
      --  We need to write a schema for this record
      Schema_Needed := True;

      Insert_NS (NS);

      D.NS   := +NS;
      D.Name := +Name;

      Index := Index + 1;
      API (Index) := D;

      if not Options.Quiet then
         Text_IO.Put_Line ("   - record          " & Name);
      end if;
   end Start_Record;

   -------------------
   -- Start_Routine --
   -------------------

   procedure Start_Routine (Name, Comment : in String) is
      D : Definition (Routine);
   begin
      Check_Routine (Name);

      D.Name := +Name;

      Index := Index + 1;
      API (Index) := D;

      if not Options.Quiet then
         Text_IO.Put_Line ("   > " & Comment & "       " & Name);
      end if;
   end Start_Routine;

   ------------
   -- To_XSD --
   ------------

   function To_XSD (NS, Ada_Type : in String) return String is
      P        : WSDL.Parameter_Type;
      Standard : Boolean;
   begin
      Insert_NS (NS);

      if Ada_Type = "character" then
         Character_Schema := True;
         return NS_Prefix
           (Name_Space.Value (Name_Space.AWS) & "Standard_pkg/")
           & ":Character";

      elsif Ada_Type = "SOAP_Base64" then
         return Types.XML_Base64_Binary;
      end if;

      WSDL.From_Ada (Ada_Type, P, Standard);

      if Standard then
         return WSDL.To_XSD (P);

      else
         --  We suppose here that this is a composite type (record/array)
         --  and that a corresponding entry will be found in the schema.
         return NS_Prefix (NS) & ':' & Ada_Type;
      end if;
   end To_XSD;

   -----------------
   -- Type_Exists --
   -----------------

   function Type_Exists (NS, Name : in String) return Boolean is
   begin
      for I in 1 .. Index loop
         if API (I).Def_Mode = Structure
           or else API (I).Def_Mode = Derived
           or else API (I).Def_Mode = Enumeration
           or else API (I).Def_Mode = Table
         then
            if -API (I).Name = Name and then -API (I).NS = NS then
               return True;
            end if;
         end if;
      end loop;

      return False;
   end Type_Exists;

   -----------
   -- Write --
   -----------

   procedure Write (Filename : in String) is

      use Ada.Text_IO;

      WS_Name : constant String := -Options.WS_Name;

      NS      : constant String
        := SOAP.Name_Space.Value (SOAP.Name_Space.AWS) & WS_Name & "_def/";

      procedure Write_Header;
      --  Write WSDL header

      procedure Write_Schema;
      --  Write WSDL schema, types tag

      procedure Write_Messages;
      --  Write WSDL message tags

      procedure Write_Port_Type;
      --  Write WSDL portType tag

      procedure Write_Binding;
      --  Write WSDL binding tag

      procedure Write_Service;
      --  Write WSDL service tag

      procedure Write_Footer;
      --  Write WSDL footer, close definition tag

      -------------------
      -- Write_Binding --
      -------------------

      procedure Write_Binding is

         procedure Write_Operation (R : in Definition);

         ---------------------
         -- Write_Operation --
         ---------------------

         procedure Write_Operation (R : in Definition) is

            procedure Write_SOAP_Body;

            ---------------------
            -- Write_SOAP_Body --
            ---------------------

            procedure Write_SOAP_Body is
            begin
               Put_Line ("            <soap:body");
               Put_Line ("               encodingStyle="""
                           & "http://schemas.xmlsoap.org/soap/encoding/""");
               Put_Line ("               namespace=""" & NS & '"');
               Put_Line ("               use=""encoded""/>");
            end Write_SOAP_Body;

            Name : constant String := -R.Name;

         begin
            Put_Line ("      <operation name=""" & Name & """>");
            Put_Line ("         <soap:operation soapAction="""
                        & Name & """/>");
            Put_Line ("         <input>");
            Write_SOAP_Body;
            Put_Line ("         </input>");
            Put_Line ("         <output>");
            Write_SOAP_Body;
            Put_Line ("         </output>");
            Put_Line ("      </operation>");
         end Write_Operation;

      begin
         New_Line;
         Put_Line ("   <binding name=""" & WS_Name & "_Binding"" type=""tns:"
                     & WS_Name & "_PortType"">");
         Put_Line ("      <soap:binding style=""rpc""");
         Put_Line ("         transport="""
                     & "http://schemas.xmlsoap.org/soap/http""/>");

         --  Output all operations info

         for I in 1 .. Index loop
            if API (I).Def_Mode = Routine then
               New_Line;
               Write_Operation (API (I));
            end if;
         end loop;

         Put_Line ("   </binding>");
      end Write_Binding;

      ------------------
      -- Write_Footer --
      ------------------

      procedure Write_Footer is
      begin
         Put_Line ("</definitions>");
      end Write_Footer;

      ------------------
      -- Write_Header --
      ------------------

      procedure Write_Header is
         use AWS;
         P : Cursor;
         N : Positive;
      begin
         Put_Line ("<?xml version=""1.0"" encoding=""UTF-8""?>");
         Put_Line ("<definitions name=""" & WS_Name  & """");
         Put_Line ("   targetNamespace=""" & NS & '"');
         Put_Line ("   xmlns:tns=""" & NS & '"');
         Put_Line ("   xmlns=""" & WSDL.NS_WSDL & '"');
         Put_Line ("   xmlns:soap=""" & WSDL.NS_SOAP & '"');
         Put_Line ("   xmlns:soapenc=""" & WSDL.NS_SOAPENC & '"');
         Put_Line ("   xmlns:wsdl=""" & WSDL.NS_WSDL & '"');
         Put_Line ("   xmlns:xsi=""" & WSDL.NS_XSI & '"');
         Put ("   xmlns:xsd=""" & WSDL.NS_XSD & '"');

         --  Write all name spaces

         P := NS_Maps.Containers.First (Name_Spaces);

         while Has_Element (P) loop
            N := NS_Maps.Containers.Element (P);

            New_Line;
            Put ("   xmlns:n" & Utils.Image (N)
                 & "=""" & NS_Maps.Containers.Key (P) & '"');
            P := NS_Maps.Containers.Next (P);
         end loop;

         --  Close definition

         Put_Line (">");

         --  Write AWS/Ada2WSDL tag

         New_Line;
         Put_Line ("   <!-- Generated by AWS/Ada2WSDL v" & Version);
         Put_Line ("        on " & GNAT.Calendar.Time_IO.Image
                     (Ada.Calendar.Clock, "%A %d %B %Y at %T") & " -->");
      end Write_Header;

      --------------------
      -- Write_Messages --
      --------------------

      procedure Write_Messages is

         procedure Write_Message (R : in Definition);

         ---------------------
         -- Write_Operation --
         ---------------------

         procedure Write_Message (R : in Definition) is

            procedure Write_Part (P : in Parameter_Access);

            ----------------
            -- Write_Part --
            ----------------

            procedure Write_Part (P : in Parameter_Access) is
               A : Parameter_Access := P;
            begin
               while A /= null loop
                  Put_Line
                    ("      <part name=""" & (-A.Name)
                     & """ type=""" & (-A.XSD_Name) & """/>");
                  A := A.Next;
               end loop;
            end Write_Part;

            Name : constant String := -R.Name;

         begin
            New_Line;

            if R.Parameters /= null then
               Put_Line ("   <message name=""" & Name & "_Request"">");
               Write_Part (R.Parameters);
               Put_Line ("   </message>");
            end if;

            New_Line;

            if R.Return_Type /= null then
               Put_Line ("   <message name=""" & Name & "_Response"">");
               Write_Part (R.Return_Type);
               Put_Line ("   </message>");
            end if;
         end Write_Message;

      begin
         for I in 1 .. Index loop
            if API (I).Def_Mode = Routine then
               Write_Message (API (I));
            end if;
         end loop;
      end Write_Messages;

      ---------------------
      -- Write_Port_Type --
      ---------------------

      procedure Write_Port_Type is

         procedure Write_Operation (R : in Definition);

         ---------------------
         -- Write_Operation --
         ---------------------

         procedure Write_Operation (R : in Definition) is

            Name : constant String := -R.Name;

         begin
            Put_Line ("      <operation name=""" & Name & """>");

            if R.Parameters /= null then
               --  Notification operation
               Put_Line
                 ("         <input message=""tns:" & Name & "_Request""/>");
            end if;

            if R.Return_Type /= null then
               --  Request-response operation
               Put_Line
                 ("         <output message=""tns:" & Name & "_Response""/>");
            end if;

            Put_Line ("      </operation>");
         end Write_Operation;

      begin
         New_Line;
         Put_Line ("   <portType name=""" & WS_Name & "_PortType"">");

         --  Output all operations info

         for I in 1 .. Index loop
            if API (I).Def_Mode = Routine then
               New_Line;
               Write_Operation (API (I));
            end if;
         end loop;

         Put_Line ("   </portType>");
      end Write_Port_Type;

      ------------------
      -- Write_Schema --
      ------------------

      procedure Write_Schema is

         procedure Write_Array (E : in Definition);
         --  Write array element tags

         procedure Write_Record (E : in Definition);
         --  Write record element tags

         procedure Write_Derived (E : in Definition);
         --  Write a derived type (simpleType)

         procedure Write_Enumeration (E : in Definition);
         --  Write an enumeration type definition (simpleType)

         procedure Write_Character;
         --  Write the Character schema

         -----------------
         -- Write_Array --
         -----------------

         procedure Write_Array (E : in Definition) is

            function Array_Constraint return String;
            --  Returns the array constaint

            ----------------------
            -- Array_Constraint --
            ----------------------

            function Array_Constraint return String is
            begin
               if E.Length = 0 then
                  --  This is an unconstrained array
                  return "[]";
               else
                  return "[" & AWS.Utils.Image (E.Length) & "]";
               end if;
            end Array_Constraint;

         begin
            New_Line;
            Put_Line ("         <complexType name=""" & (-E.Name) & '"');
            Put_Line ("                 targetNamespace=""" & (-E.NS) & """>");
            Put_Line ("            <complexContent>");
            Put_Line ("               <restriction base=""soapenc:Array"">");
            Put_Line ("                  <attribute ref=""soapenc:arrayType"""
                        & " wsdl:arrayType=""" & (-E.Parameters.XSD_Name)
                        & Array_Constraint & """/>");
            Put_Line ("               </restriction>");
            Put_Line ("            </complexContent>");
            Put_Line ("         </complexType>");
         end Write_Array;

         ---------------------
         -- Write_Character --
         ---------------------

         procedure Write_Character is
         begin
            New_Line;
            Put_Line ("         <simpleType name=""Character""");
            Put_Line ("                 targetNamespace="""
                      & Name_Space.Value (Name_Space.AWS)
                      & "Standard_pkg/" & """>");
            Put_Line ("            <restriction base=""xsd:string"">");
            Put_Line ("               <length value=""1""/>");
            Put_Line ("            </restriction>");
            Put_Line ("         </simpleType>");
         end Write_Character;

         -------------------
         -- Write_Derived --
         -------------------

         procedure Write_Derived (E : in Definition) is
            P : constant Parameter_Access := E.Parameters;
         begin
            New_Line;
            Put_Line ("         <simpleType name=""" & (-E.Name) & '"');
            Put_Line ("                 targetNamespace=""" & (-E.NS) & """>");
            Put_Line ("            <restriction base="""
                        & (-P.XSD_Name) & """/>");
            Put_Line ("         </simpleType>");
         end Write_Derived;

         -----------------------
         -- Write_Enumeration --
         -----------------------

         procedure Write_Enumeration (E : in Definition) is
            P : Parameter_Access := E.Parameters;
         begin
            New_Line;
            Put_Line ("         <simpleType name=""" & (-E.Name) & '"');
            Put_Line ("                 targetNamespace=""" & (-E.NS) & """>");
            Put_Line ("            <restriction base=""xsd:string"">");

            while P /= null loop
               Put_Line ("               <enumeration value="""
                           & (-P.Name) & """/>");
               P := P.Next;
            end loop;

            Put_Line ("            </restriction>");
            Put_Line ("         </simpleType>");
         end Write_Enumeration;

         ------------------
         -- Write_Record --
         ------------------

         procedure Write_Record (E : in Definition) is
            P : Parameter_Access := E.Parameters;
         begin
            New_Line;
            Put_Line ("         <complexType name=""" & (-E.Name) & '"');
            Put_Line ("                 targetNamespace=""" & (-E.NS) & """>");
            Put_Line ("            <all>");

            while P /= null loop
               Put_Line ("               <element name=""" & (-P.Name)
                           & """ type=""" & (-P.XSD_Name) & """/>");
               P := P.Next;
            end loop;

            Put_Line ("            </all>");
            Put_Line ("         </complexType>");
         end Write_Record;

      begin
         if Schema_Needed or else Character_Schema then
            New_Line;
            Put_Line ("   <types>");
            Put_Line
              ("      <schema xmlns=""http://www.w3.org/2000/10/XMLSchema"">");

            if Character_Schema then
               Write_Character;
            end if;

            --  Output all structures

            for I in 1 .. Index loop
               case API (I).Def_Mode is
                  when Structure   => Write_Record (API (I));
                  when Table       => Write_Array (API (I));
                  when Derived     => Write_Derived (API (I));
                  when Enumeration => Write_Enumeration (API (I));

                  when Safe_Pointer_Definition
                     | Routine
                       => null;
               end case;
            end loop;

            Put_Line ("      </schema>");
            Put_Line ("   </types>");
         end if;
      end Write_Schema;

      -------------------
      -- Write_Service --
      -------------------

      procedure Write_Service is
      begin
         New_Line;
         Put_Line ("   <service name=""" & WS_Name & "_Service"">");
         Put_Line ("      <port name=""" & WS_Name & "_Port"" binding=""tns:"
                     & WS_Name & "_Binding"">");
         Put_Line ("         <soap:address location="""
                     & To_String (Options.SOAP_Address) & """/>");
         Put_Line ("      </port>");
         Put_Line ("   </service>");
      end Write_Service;

      File : Text_IO.File_Type;

   begin
      Create (File, Out_File, Filename);

      Text_IO.Set_Output (File);

      Write_Header;

      Write_Schema;

      Write_Messages;

      Write_Port_Type;

      Write_Binding;

      Write_Service;

      Write_Footer;

      Set_Output (Current_Output);
   end Write;

end Ada2WSDL.Generator;
