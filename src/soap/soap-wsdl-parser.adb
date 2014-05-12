------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2003-2014, AdaCore                     --
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

pragma Ada_2012;

with Ada.Characters.Handling;
with Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Text_IO;

with DOM.Core.Nodes;

with AWS.Containers.Key_Value;
with AWS.Utils;
with SOAP.Types;
with SOAP.Utils;
with SOAP.XML;

package body SOAP.WSDL.Parser is

   use Ada;
   use type DOM.Core.Node;

   Verbose_Mode  : Verbose_Level := 0;
   Skip_Error    : Boolean       := False;
   NS_SOAP       : Unbounded_String;

   No_Name_Space : Name_Space.Object renames Name_Space.No_Name_Space;

   package Name_Spaces renames AWS.Containers.Key_Value;
   NS     : Name_Spaces.Map;
   NS_Num : Natural := 0;

   function Get_Node
     (Parent  : DOM.Core.Node;
      Element : String;
      Name    : String        := "";
      NS      : Boolean       := False) return DOM.Core.Node;
   --  Returns child node named Element having the value Name for attribute
   --  "name" if specified.

   function "+" (Str : String) return Unbounded_String
     renames To_Unbounded_String;

   function "-" (Str : Unbounded_String) return String
     renames To_String;

   procedure Parse_Service
     (O        : in out Object'Class;
      Service  : DOM.Core.Node;
      Document : WSDL.Object);
   --  Parse WSDL service nodes

   procedure Parse_Binding
     (O        : in out Object'Class;
      Binding  : DOM.Core.Node;
      Document : WSDL.Object);
   --  Parse WSDL binding nodes

   procedure Parse_Definitions
     (O           : Object'Class;
      Definitions : DOM.Core.Node;
      Document    : WSDL.Object);
   --  Parse WSDL definition node

   procedure Parse_Operation
     (O         : in out Object'Class;
      Operation : DOM.Core.Node;
      Document  : WSDL.Object);
   --  Parse WSDL operation nodes

   procedure Parse_PortType
     (O         : in out Object'Class;
      Operation : DOM.Core.Node;
      Document  : WSDL.Object);
   --  Parse WSDL PortType nodes

   procedure Parse_Part
     (O        : in out Object'Class;
      Part     : DOM.Core.Node;
      Document : WSDL.Object);
   --  Parse WSDL part nodes

   procedure Parse_Message
     (O        : in out Object'Class;
      Message  : DOM.Core.Node;
      Document : WSDL.Object);
   --  Parse WSDL message nodes

   procedure Parse_Element
     (O        : in out Object'Class;
      Element  : DOM.Core.Node;
      Document : WSDL.Object);
   --  Parse WSDL element nodes

   procedure Add_Parameter
     (O      : in out Object'Class;
      Name   : String;
      P_Type : Parameter_Type)
     with Inline;
   --  Add parameter Name / P_Type into O using current mode (O.Mode)

   procedure Add_Parameter
     (O     : in out Object'Class;
      Param : Parameters.Parameter)
     with Inline;
   --  Add parameter into O using current mode (O.Mode)

   function Parse_Parameter
     (O        : Object'Class;
      N        : DOM.Core.Node;
      Document : WSDL.Object) return Parameters.Parameter;
   --  Returns parameter in node P

   function Parse_Record
     (O        : Object'Class;
      R        : DOM.Core.Node;
      Document : WSDL.Object) return Parameters.Parameter;
   --  Returns record in node N

   function Parse_Array
     (O        : Object'Class;
      R        : DOM.Core.Node;
      Document : WSDL.Object) return Parameters.Parameter;
   --  Returns array in node N

   function Parse_Simple
     (O        : Object'Class;
      R        : DOM.Core.Node;
      Document : WSDL.Object) return Parameters.Parameter;
   --  Returns the derived or enumeration type in node N (N must be a
   --  simpleType schema node).

   function Is_Array
     (O : Object'Class;
      N : DOM.Core.Node) return Boolean;
   --  Returns True if N is an array description node. Set the array element
   --  name into the object.

   function Is_Record
     (O : Object'Class;
      N : DOM.Core.Node) return Boolean;
   --  Returns True if N is a struct description node

   procedure Check_Character (R : DOM.Core.Node);
   --  Checks that N is a valid schema definition for a Character Ada type

   function Get_Target_Name_Space
     (N : DOM.Core.Node) return Name_Space.Object;
   --  Returns the targetNamespace

   function Get_NS_Name_For
     (N : DOM.Core.Node; Value : String) return String;
   --  Returns the namespace Name given the Value. The value is checked
   --  starting from N.

   -----------
   -- Debug --
   -----------

   procedure Trace (Message : String; N : DOM.Core.Node);
   --  Display trace message and info about the node

   ----------------
   -- Accept_RPC --
   ----------------

   procedure Accept_Document (O : in out Object'Class) is
   begin
      O.Accept_Document := True;
   end Accept_Document;

   -------------------
   -- Add_Parameter --
   -------------------

   procedure Add_Parameter
     (O      : in out Object'Class;
      Name   : String;
      P_Type : Parameter_Type) is
   begin
      Add_Parameter
        (O, (Parameters.K_Simple, +Name, No_Name_Space, null, P_Type));
   end Add_Parameter;

   procedure Add_Parameter
     (O     : in out Object'Class;
      Param : Parameters.Parameter) is
   begin
      Parameters.Append (O.Params (O.Mode), Param);
   end Add_Parameter;

   ---------------------
   -- Check_Character --
   ---------------------

   procedure Check_Character (R : DOM.Core.Node) is

      function Character_Facet
        (Parent : DOM.Core.Node;
         Child  : Boolean := False) return DOM.Core.Node;
      --  Returns the first node corresponding to a character type definition.
      --  It skips annotation tag for example.

      ---------------------
      -- Character_Facet --
      ---------------------

      function Character_Facet
        (Parent : DOM.Core.Node;
         Child  : Boolean := False) return DOM.Core.Node
      is
         N : DOM.Core.Node := Parent;
      begin
         if Child then
            N := XML.First_Child (N);
         else
            N := XML.Next_Sibling (N);
         end if;

         while N /= null
           and then DOM.Core.Nodes.Local_Name (N) /= "length"
           and then DOM.Core.Nodes.Local_Name (N) /= "minLength"
           and then DOM.Core.Nodes.Local_Name (N) /= "maxLength"
         loop
            N := XML.Next_Sibling (N);
         end loop;

         return N;
      end Character_Facet;

      N : DOM.Core.Node := R;

   begin
      Trace ("(Check_Character)", R);

      pragma Assert
        (R /= null
         and then Utils.No_NS (DOM.Core.Nodes.Node_Name (R)) = "simpleType");

      --  Now check that if Name is Character and base is xsd:string
      --  that this is really an Ada Character type. For this the
      --  type must be constrained to a single character.
      --
      --  Either we have the facet <length value="1">
      --  Or <minLength value="1"> and <maxLength value="1">

      declare
         Name : constant String := XML.Get_Attr_Value (R, "name", False);
      begin
         --  Get restriction node

         N := XML.First_Child (N);

         declare
            Base : constant String := XML.Get_Attr_Value (N, "base", False);
         begin
            if Characters.Handling.To_Lower (Name) /= "character"
              or else Base /= "string"
            then
               raise WSDL_Error
                 with "Schema does not correspond to Ada Character type.";
            end if;

            N := Character_Facet (N, Child => True);

            if N /= null
              and then DOM.Core.Nodes.Local_Name (N) = "length"
            then
               --  Check length

               if XML.Get_Attr_Value (N, "value", False) /= "1" then
                  raise WSDL_Error
                    with "Schema does not correspond"
                      & " to Ada Character type (length /= 1).";
               end if;

            elsif N /= null
              and then DOM.Core.Nodes.Local_Name (N) = "minLength"
            then

               if XML.Get_Attr_Value (N, "value", False) /= "1" then
                  raise WSDL_Error
                    with "Schema does not correspond"
                      & " to Ada Character type (minLength /= 1).";
               end if;

               N := Character_Facet (N);

               if N = null
                 or else DOM.Core.Nodes.Local_Name (N) /= "maxLength"
                 or else XML.Get_Attr_Value (N, "value", False) /= "1"
               then
                  if N = null then
                     Text_IO.Put_Line ("N=null");
                  end if;

                  raise WSDL_Error
                    with "Schema does not correspond"
                      & " to Ada Character type (maxLength /= 1).";
               end if;

            elsif N /= null
              and then DOM.Core.Nodes.Local_Name (N) = "maxLength"
            then

               if XML.Get_Attr_Value (N, "value", False) /= "1" then
                  raise WSDL_Error
                    with "Schema does not correspond"
                      & " to Ada Character type (maxLength /= 1).";
               end if;

               N := Character_Facet (N);

               if N = null
                 or else DOM.Core.Nodes.Local_Name (N) /= "minLength"
                 or else XML.Get_Attr_Value (N, "value", False) /= "1"
               then
                  raise WSDL_Error
                    with "Schema does not correspond"
                      & " to Ada Character type (minLength /= 1).";
               end if;

            else
               raise WSDL_Error
                 with "Schema does not correspond"
                   & " to Ada Character type (no facet).";
            end if;
         end;
      end;
   end Check_Character;

   -----------------------
   -- Continue_On_Error --
   -----------------------

   procedure Continue_On_Error is
   begin
      Skip_Error := True;
   end Continue_On_Error;

   -------------
   -- Exclude --
   -------------

   procedure Exclude (O : in out Object; Operation : String) is
      Pos     : Exclude_Set.Cursor;
      Success : Boolean;
   begin
      Exclude_Set.Insert (O.Exclude, Operation, Pos, Success);
   end Exclude;

   --------------
   -- Get_Node --
   --------------

   function Get_Node
     (Parent  : DOM.Core.Node;
      Element : String;
      Name    : String        := "";
      NS      : Boolean       := False) return DOM.Core.Node
   is
      function Get_Node_Int
        (Parent  : DOM.Core.Node;
         Element : String;
         Name    : String) return DOM.Core.Node;
      --  Recursive procedure that does the job

      ------------------
      -- Get_Node_Int --
      ------------------

      function Get_Node_Int
        (Parent  : DOM.Core.Node;
         Element : String;
         Name    : String) return DOM.Core.Node
      is
         N, R : DOM.Core.Node;
         E    : Natural;
      begin
         if Element = "" then
            --  No more element to look for
            if Name = ""
              or else XML.Get_Attr_Value (Parent, "name") = Name
            then
               --  There is no attribute to look for or we are in the right
               --  node, return this node.
               return Parent;
            else
               --  No found otherwise
               return null;
            end if;
         end if;

         E := Strings.Fixed.Index (Element, ".");

         if E = 0 then
            --  No more separator, this is the last element
            E := Element'Last;
         else
            E := E - 1;
         end if;

         --  Iterate through childs, look for element

         N := XML.First_Child (Parent);

         declare
            E_Name : constant String := Element (Element'First .. E);
         begin
            R := null;

            while N /= null loop
               if (not NS and then DOM.Core.Nodes.Local_Name (N) = E_Name)
                 or else (NS and then DOM.Core.Nodes.Node_Name (N) = E_Name)
               then
                  --  We found this element, check next one
                  R := Get_Node_Int
                    (N, Element (E + 2 .. Element'Last), Name);
                  --  Exit now ff we have found the right node, otherwise let's
                  --  try the next sibling.
                  exit when R /= null;
               end if;
               N := XML.Next_Sibling (N);
            end loop;
         end;

         return R;
      end Get_Node_Int;

   begin
      Trace ("(Get_Node) - " & Element & " -> " & Name, Parent);

      return Get_Node_Int (Parent, Element, Name);
   end Get_Node;

   ---------------------
   -- Get_NS_Name_For --
   ---------------------

   function Get_NS_Name_For
     (N : DOM.Core.Node; Value : String) return String is
   begin
      if N = null then
         return "";

      else
         declare
            Atts : constant DOM.Core.Named_Node_Map :=
                     DOM.Core.Nodes.Attributes (N);
         begin
            for K in reverse 0 .. DOM.Core.Nodes.Length (Atts) - 1 loop
               declare
                  N : constant DOM.Core.Node := DOM.Core.Nodes.Item (Atts, K);
               begin
                  if DOM.Core.Nodes.Node_Value (N) = Value
                    and then DOM.Core.Nodes.Local_Name (N) /= "targetNamespace"
                  then
                     return DOM.Core.Nodes.Local_Name (N);
                  end if;
               end;
            end loop;
         end;

         return Get_NS_Name_For (DOM.Core.Nodes.Parent_Node (N), Value);
      end if;
   end Get_NS_Name_For;

   ---------------------------
   -- Get_Target_Name_Space --
   ---------------------------

   function Get_Target_Name_Space
     (N : DOM.Core.Node) return Name_Space.Object
   is
      V : constant String := XML.Get_Attr_Value (N, "targetNamespace", True);
      P : Name_Spaces.Cursor;
      R : Boolean;
   begin
      if V = "" and then DOM.Core.Nodes.Parent_Node (N) /= null then
         return Get_Target_Name_Space (DOM.Core.Nodes.Parent_Node (N));

      else
         P := Name_Spaces.Find (NS, V);

         if Name_Spaces.Has_Element (P) then
            return Name_Space.Create (Name_Spaces.Element (P), V);

         else
            NS_Num := NS_Num + 1;
            declare
               Name : constant String := "n" & AWS.Utils.Image (NS_Num);
            begin
               Name_Spaces.Insert (NS, V, Name, P, R);
               return Name_Space.Create (Name, V);
            end;
         end if;
      end if;
   end Get_Target_Name_Space;

   --------------
   -- Is_Array --
   --------------

   function Is_Array
     (O : Object'Class;
      N : DOM.Core.Node) return Boolean
   is
      function Array_Elements return Unbounded_String;
      --  Returns array's element type encoded in node L

      L : DOM.Core.Node := N;

      --------------------
      -- Array_Elements --
      --------------------

      function Array_Elements return Unbounded_String is
         Attributes : constant  DOM.Core.Named_Node_Map :=
                        DOM.Core.Nodes.Attributes (L);
      begin
         --  Look for arrayType in Attributes list

         for K in 0 .. DOM.Core.Nodes.Length (Attributes) - 1 loop

            declare
               N : constant DOM.Core.Node :=
                     DOM.Core.Nodes.Item (Attributes, K);
            begin
               if Utils.No_NS (DOM.Core.Nodes.Node_Name (N)) = "arrayType" then
                  --  Found get the value removing []
                  declare
                     Value : constant String :=
                               Utils.No_NS (DOM.Core.Nodes.Node_Value (N));
                     First : Natural;
                     Last  : Natural;
                  begin
                     First := Strings.Fixed.Index (Value, "[");
                     Last  := Strings.Fixed.Index (Value, "]");

                     if First = 0 or else Last = 0 then
                        raise WSDL_Error with "missing [] in arrayType value.";
                     end if;

                     if Last > First + 1 then
                        O.Self.Array_Length
                          := Natural'Value (Value (First + 1 .. Last - 1));
                     else
                        O.Self.Array_Length := 0;
                     end if;

                     return To_Unbounded_String
                       (Value (Value'First .. First - 1));
                  end;
               end if;
            end;
         end loop;

         raise WSDL_Error with "array element type not found.";
      end Array_Elements;

   begin
      if Utils.No_NS (DOM.Core.Nodes.Node_Name (L)) = "complexType" then
         L := XML.First_Child (L);

         if L /= null
           and then
             Utils.No_NS (DOM.Core.Nodes.Node_Name (L)) = "complexContent"
         then
            L := XML.First_Child (L);

            if L /= null
              and then
                Utils.No_NS (DOM.Core.Nodes.Node_Name (L)) = "restriction"
            then
               L := XML.First_Child (L);

               if L /= null
                 and then
                   Utils.No_NS (DOM.Core.Nodes.Node_Name (L)) = "attribute"
               then
                  O.Self.Array_Elements := Array_Elements;
                  return True;
               end if;
            end if;
         end if;
      end if;

      return False;
   end Is_Array;

   ---------------
   -- Is_Record --
   ---------------

   function Is_Record
     (O : Object'Class;
      N : DOM.Core.Node) return Boolean
   is
      pragma Unreferenced (O);
      L : DOM.Core.Node := N;
   begin
      if Utils.No_NS (DOM.Core.Nodes.Node_Name (L)) = "element"
        and then XML.First_Child (L) /= null
      then
         --  Handle an element enclosing the complexType
         L := XML.First_Child (L);
      end if;

      if Utils.No_NS (DOM.Core.Nodes.Node_Name (L)) = "complexType" then
         L := XML.First_Child (L);

         --  Empty complexType

         if L = null then
            return True;

         else
            if Utils.No_NS (DOM.Core.Nodes.Node_Name (L))
              = "complexContent"
            then
               L := XML.First_Child (L);
            end if;

            if L = null then
               raise WSDL_Error with "empty complexContent.";

            elsif Utils.No_NS (DOM.Core.Nodes.Node_Name (L))
              = "extension"
            then
               L := XML.First_Child (L);
            end if;
         end if;

         --  Empty extension

         if L = null then
            return True;
         end if;

         if Utils.No_NS (DOM.Core.Nodes.Node_Name (L)) = "all"
           or else Utils.No_NS (DOM.Core.Nodes.Node_Name (L)) = "sequence"
         then
            L := XML.First_Child (L);

            if L /= null
               and then Utils.No_NS (DOM.Core.Nodes.Node_Name (L)) = "element"
            then
               return True;
            end if;
         end if;
      end if;

      return False;
   end Is_Record;

   -----------
   -- Parse --
   -----------

   procedure Parse
     (O        : in out Object'Class;
      Document : WSDL.Object)
   is
      N     : constant DOM.Core.Node :=
                XML.First_Child (DOM.Core.Node (Document));
      NL    : constant DOM.Core.Node_List := DOM.Core.Nodes.Child_Nodes (N);
      Found : Boolean := False;
   begin
      --  First we want to parse the definitions node to get the namespaces

      Parse_Definitions (O, N, Document);

      --  Look for the service node

      for K in 0 .. DOM.Core.Nodes.Length (NL) - 1 loop
         declare
            S : constant DOM.Core.Node := DOM.Core.Nodes.Item (NL, K);
         begin
            if DOM.Core.Nodes.Local_Name (S) = "service" then
               Parse_Service (O, S, Document);
               Found := True;
            end if;
         end;
      end loop;

      if Verbose_Mode > 0 and then not Found then
         Text_IO.New_Line;
         Text_IO.Put_Line ("No service found in this document.");
      end if;
   end Parse;

   -----------------
   -- Parse_Array --
   -----------------

   function Parse_Array
     (O        : Object'Class;
      R        : DOM.Core.Node;
      Document : WSDL.Object) return Parameters.Parameter
   is
      P : Parameters.Parameter (Parameters.K_Array);
   begin
      Trace ("(Parse_Array)", R);

      pragma Assert
        (R /= null
         and then Utils.No_NS (DOM.Core.Nodes.Node_Name (R)) = "complexType");

      P.NS := Get_Target_Name_Space (R);

      declare
         Name : constant String := XML.Get_Attr_Value (R, "name", False);
      begin
         --  Set array name, R is a complexType node

         P.Name   := O.Current_Name;
         P.T_Name := +Name;
         P.E_Type := O.Array_Elements;
         P.Length := O.Array_Length;

         if not WSDL.Is_Standard (To_String (O.Array_Elements)) then
            --  This is not a standard type, parse it
            declare
               N : constant DOM.Core.Node :=
                     Get_Node (DOM.Core.Node (Document),
                               "definitions.types.schema.complexType",
                               To_String (O.Array_Elements));
            begin
               --  ??? Right now pretend that it is a record, there is
               --  certainly some cases not covered here.
               Parameters.Append (P.P, Parse_Record (O, N, Document));
            end;
         end if;

         return P;
      end;
   end Parse_Array;

   -------------------
   -- Parse_Binding --
   -------------------

   procedure Parse_Binding
     (O        : in out Object'Class;
      Binding  : DOM.Core.Node;
      Document : WSDL.Object)
   is
      N : DOM.Core.Node;
   begin
      Trace ("(Parse_Binding)", Binding);

      N := Get_Node (Binding, Utils.With_NS (-NS_SOAP, "binding"), NS => True);

      if N = null then
         raise WSDL_Error with "Binding style/transport definition not found.";
      end if;

      --  Check for style (only Document is supported)

      if not O.Accept_Document
        and then XML.Get_Attr_Value (N, "style") = "document"
      then
         raise WSDL_Error with "Document Web Service style not supported.";
      end if;

      --  Check for transport (only HTTP is supported)

      declare
         T : constant String := XML.Get_Attr_Value (N, "transport");
      begin
         if T (T'Last - 4 .. T'Last) /= "/http" then
            raise WSDL_Error with "Only HTTP transport supported.";
         end if;
      end;

      --  Read all operations

      declare
         NL : constant DOM.Core.Node_List :=
                DOM.Core.Nodes.Child_Nodes (Binding);
      begin
         for K in 0 .. DOM.Core.Nodes.Length (NL) - 1 loop
            declare
               S : constant DOM.Core.Node := DOM.Core.Nodes.Item (NL, K);
            begin
               if Utils.No_NS (DOM.Core.Nodes.Node_Name (S)) = "operation"
                 and then not Exclude_Set.Contains
                   (O.Exclude, XML.Get_Attr_Value (S, "name"))
               then
                  begin
                     Parse_Operation
                       (O, DOM.Core.Nodes.Item (NL, K), Document);
                  exception
                     when E : WSDL_Error =>
                        if Skip_Error then
                           Text_IO.Put_Line
                             ("     "
                                & XML.Get_Attr_Value (S, "name")
                                & " skipped : "
                                & Exceptions.Exception_Message (E));
                        else
                           raise WSDL_Error
                             with "(" & XML.Get_Attr_Value (S, "name")
                                & ") " & Exceptions.Exception_Message (E);
                        end if;
                  end;
               end if;
            end;
         end loop;
      end;
   end Parse_Binding;

   -----------------------
   -- Parse_Definitions --
   -----------------------

   procedure Parse_Definitions
     (O           : Object'Class;
      Definitions : DOM.Core.Node;
      Document    : WSDL.Object)
   is
      pragma Unreferenced (O, Document);

      Atts : constant DOM.Core.Named_Node_Map :=
               DOM.Core.Nodes.Attributes (Definitions);
   begin
      Trace ("(Parse_Definitions)", Definitions);

      for K in 0 .. DOM.Core.Nodes.Length (Atts) - 1 loop
         declare
            N : constant DOM.Core.Node := DOM.Core.Nodes.Item (Atts, K);
         begin
            if DOM.Core.Nodes.Node_Value (N) = WSDL.NS_SOAP then
               NS_SOAP := +DOM.Core.Nodes.Local_Name (N);
            end if;
         end;
      end loop;
   end Parse_Definitions;

   -------------------
   -- Parse_Element --
   -------------------

   procedure Parse_Element
     (O        : in out Object'Class;
      Element  : DOM.Core.Node;
      Document : WSDL.Object)
   is
      N       : DOM.Core.Node := Element;
      CT_Node : DOM.Core.Node;
   begin
      Trace ("(Parse_Element)", Element);

      while N /= null
        and then DOM.Core.Nodes.Local_Name (N) /= "complexType"
        and then DOM.Core.Nodes.Local_Name (N) /= "simpleType"
        and then DOM.Core.Nodes.Local_Name (N) /= "element"
      loop
         N := XML.First_Child (N);
      end loop;

      if N = null then
         raise WSDL_Error with "No element found in schema.";
      else
         CT_Node := N;
      end if;

      if DOM.Core.Nodes.Local_Name (N) = "simpleType" then
         Add_Parameter (O, Parse_Simple (O, CT_Node, Document));

      else
         --  This is a complexType, continue analyse

         declare
            Parent : constant DOM.Core.Node := N;
            ET     : constant String := XML.Get_Attr_Value (N, "type");
         begin
            if N /= null
              and then DOM.Core.Nodes.Local_Name (N) = "element"
            then
               if ET = "" then
                  --  Move to complexType node
                  N := XML.First_Child (N);

               else
                  --  Get the corresponding type definition

                  N := Get_Node
                    (XML.First_Child (DOM.Core.Node (Document)),
                     "types.schema.complexType", Utils.No_NS (ET));
               end if;
            end if;

            --  Enter complexType node

            N := XML.First_Child (N);

            if N = null then
               if XML.Get_Attr_Value (Parent, "abstract") = "true" then
                  raise WSDL_Error with "abstract complexType not suported.";
               end if;
            end if;
         end;

         if Is_Record (O, CT_Node) then
            --  This is a record or composite type

            Add_Parameter (O, Parse_Record (O, CT_Node, Document));

         elsif Is_Array (O, CT_Node) then

            Add_Parameter (O, Parse_Array (O, CT_Node, Document));

         else
            declare
               NL : constant DOM.Core.Node_List :=
                      DOM.Core.Nodes.Child_Nodes (N);
            begin
               for K in 0 .. DOM.Core.Nodes.Length (NL) - 1 loop
                  declare
                     N : constant DOM.Core.Node := DOM.Core.Nodes.Item (NL, K);
                  begin
                     if DOM.Core.Nodes.Node_Name (N) /= "#text" then
                        Add_Parameter (O, Parse_Parameter (O, N, Document));
                     end if;
                  end;
               end loop;
            end;
         end if;
      end if;
   end Parse_Element;

   -------------------
   -- Parse_Message --
   -------------------

   procedure Parse_Message
     (O        : in out Object'Class;
      Message  : DOM.Core.Node;
      Document : WSDL.Object)
   is
      N : DOM.Core.Node := Message;
   begin
      Trace ("(Parse_Message)", Message);

      N := XML.First_Child (N);

      while N /= null loop
         Parse_Part (O, N, Document);
         N := XML.Next_Sibling (N);
      end loop;
   end Parse_Message;

   ---------------------
   -- Parse_Operation --
   ---------------------

   procedure Parse_Operation
     (O         : in out Object'Class;
      Operation : DOM.Core.Node;
      Document  : WSDL.Object)
   is
      N : DOM.Core.Node;
   begin
      Trace ("(Parse_Operation)", Operation);

      O.Proc := +XML.Get_Attr_Value (Operation, "name");

      N := Get_Node
        (Operation, Utils.With_NS (-NS_SOAP, "operation"), NS => True);

      if N = null then
         raise WSDL_Error with "soap:operation not found.";
      end if;

      if DOM.Core.Nodes.Get_Named_Item
        (DOM.Core.Nodes.Attributes (N), "soapAction") = null
      then
         O.SOAPAction := +No_SOAPAction;
      else
         O.SOAPAction := +XML.Get_Attr_Value (N, "soapAction");
      end if;

      N := XML.Next_Sibling (N);
      N := XML.First_Child (N);

      declare
         NS_Value : constant String := XML.Get_Attr_Value (N, "namespace");
         NS_Name  : constant String :=
                      Get_NS_Name_For
                        (DOM.Core.Nodes.Parent_Node (N), NS_Value);
      begin
         if NS_Value /= "" then
            if NS_Name = "" then
               raise WSDL_Error
                 with "Missing definition for namespace " & NS_Value;
            else
               O.Namespace := Name_Space.Create (NS_Name, NS_Value);
            end if;
         end if;
      end;

      --  Check that input/output/fault is literal
      --  ???

      N := Get_Node
        (XML.First_Child (DOM.Core.Node (Document)),
         "portType.operation", -O.Proc);

      if N = null then
         raise WSDL_Error
           with "portType.operation for " & (-O.Proc) & " not found.";
      end if;

      Parse_PortType (O, N, Document);
   end Parse_Operation;

   ---------------------
   -- Parse_Parameter --
   ---------------------

   function Parse_Parameter
     (O        : Object'Class;
      N        : DOM.Core.Node;
      Document : WSDL.Object) return Parameters.Parameter
   is
      P_Type : constant String := XML.Get_Attr_Value (N, "type", False);
   begin
      Trace ("(Parse_Parameter)", N);

      if WSDL.Is_Standard (P_Type) then
         return
           (Parameters.K_Simple, +XML.Get_Attr_Value (N, "name"),
            No_Name_Space, null, To_Type (P_Type));

      elsif P_Type = "anyType" then
         raise WSDL_Error with "Type anyType is not supported.";

      else
         declare
            R : DOM.Core.Node :=
                  Get_Node (DOM.Core.Node (Document),
                            "definitions.types.schema.complexType", P_Type);
         begin
            if R = null then
               --  Now check for a simpleType
               R := Get_Node (DOM.Core.Node (Document),
                              "definitions.types.schema.simpleType", P_Type);

               if R = null then
                  raise WSDL_Error with
                    "types.schema definition for " & P_Type & " not found.";

               else
                  O.Self.Current_Name := +XML.Get_Attr_Value (N, "name");
                  return Parse_Simple (O, R, Document);
               end if;
            end if;

            if Is_Array (O, R) then
               declare
                  P : Parameters.Parameter := Parse_Array (O, R, Document);
               begin
                  P.Name := +XML.Get_Attr_Value (N, "name");
                  return P;
               end;

            else
               O.Self.Current_Name := +XML.Get_Attr_Value (N, "name");
               return Parse_Record (O, R, Document);
            end if;
         end;
      end if;
   end Parse_Parameter;

   ----------------
   -- Parse_Part --
   ----------------

   procedure Parse_Part
     (O        : in out Object'Class;
      Part     : DOM.Core.Node;
      Document : WSDL.Object)
   is
      N  : DOM.Core.Node;
      ET : Unbounded_String;
   begin
      Trace ("(Parse_Part)", Part);

      ET := +XML.Get_Attr_Value (Part, "element");

      if ET = Null_Unbounded_String then
         ET := +XML.Get_Attr_Value (Part, "type");
      end if;

      if ET = Null_Unbounded_String then
         raise WSDL_Error
           with "No type or element attribute found for part element.";
      end if;

      O.Current_Name := +XML.Get_Attr_Value (Part, "name");

      declare
         T       : constant String := -ET;
         T_No_NS : constant String := Utils.No_NS (T);
      begin
         if WSDL.Is_Standard (T_No_NS) then

            if WSDL.To_Type (T_No_NS) = WSDL.P_Character then
               Check_Character
                 (Get_Node (DOM.Core.Node (Document),
                            "definitions.types.schema.simpleType", T_No_NS));
            end if;

            Add_Parameter (O, -O.Current_Name, WSDL.To_Type (T_No_NS));

         elsif T = Types.XML_Any_Type then
            raise WSDL_Error with "Type anyType is not supported.";

         else
            --  First search for element in the schema

            N := Get_Node
              (XML.First_Child (DOM.Core.Node (Document)),
               "types.schema.element", T_No_NS);

            --  If not present look for a simpleType

            if N = null then
               N := Get_Node
                 (XML.First_Child (DOM.Core.Node (Document)),
                  "types.schema.simpleType", T_No_NS);
            end if;

            --  If not present look for a complexType

            if N = null then
               N := Get_Node
                 (XML.First_Child (DOM.Core.Node (Document)),
                  "types.schema.complexType", T_No_NS);
            end if;

            if N = null then
               raise WSDL_Error with "Definition for " & T & " not found.";
            end if;

            Parse_Element (O, N, Document);
         end if;
      end;
   end Parse_Part;

   --------------------
   -- Parse_PortType --
   --------------------

   procedure Parse_PortType
     (O         : in out Object'Class;
      Operation : DOM.Core.Node;
      Document  : WSDL.Object)
   is
      procedure Get_Element (M : DOM.Core.Node);
      --  Returns the element node which contains parameters for node M

      -----------------
      -- Get_Element --
      -----------------

      procedure Get_Element (M : DOM.Core.Node) is
         N       : DOM.Core.Node;
         Message : Unbounded_String;
      begin
         Message := +XML.Get_Attr_Value (M, "message", False);

         N := Get_Node
           (XML.First_Child (DOM.Core.Node (Document)),
            "message", -Message);

         if N = null then
            --  In this case the message reference the schema element

            N := Get_Node
              (XML.First_Child (DOM.Core.Node (Document)),
               "types.schema.element", -Message);

            if N = null then
               raise WSDL_Error
                 with "types.schema.element for " & (-Message) & " not found.";
            end if;

            Parse_Element (O, N, Document);

         else
            Parse_Message (O, N, Document);
         end if;
      end Get_Element;

      N : DOM.Core.Node;

   begin
      Trace ("(Parse_PortType)", Operation);

      --  Input parameters

      N := Get_Node (Operation, "input");

      if N /= null then
         O.Mode := Input;
         Get_Element (N);
      end if;

      --  Output parameters

      N := Get_Node (Operation, "output");

      if N /= null then
         O.Mode := Output;
         Get_Element (N);
      end if;

      --  Fault parameters

      N := Get_Node (Operation, "fault");

      if N /= null then
         O.Mode := Fault;
         Get_Element (N);
      end if;

      if Verbose_Mode > 0 then
         Text_IO.New_Line;
         Text_IO.Put_Line
           ("Procedure " & (-O.Proc) & " SOAPAction:" & (-O.SOAPAction));
         Text_IO.Put_Line ("   Input");
         Parameters.Output (O.Params (Input));

         Text_IO.Put_Line ("   Output");
         Parameters.Output (O.Params (Output));
      end if;

      New_Procedure
        (O, -O.Proc, -O.SOAPAction, O.Namespace,
         O.Params (Input), O.Params (Output), O.Params (Fault));

      Parameters.Release (O.Params (Input));
      Parameters.Release (O.Params (Output));
      Parameters.Release (O.Params (Fault));
   end Parse_PortType;

   ------------------
   -- Parse_Record --
   ------------------

   function Parse_Record
     (O        : Object'Class;
      R        : DOM.Core.Node;
      Document : WSDL.Object) return Parameters.Parameter
   is
      P : Parameters.Parameter (Parameters.K_Record);
      N : DOM.Core.Node;
   begin
      Trace ("(Parse_Record)", R);

      pragma Assert
        (R /= null
         and then
           (Utils.No_NS (DOM.Core.Nodes.Node_Name (R)) = "complexType"
            or else Utils.No_NS (DOM.Core.Nodes.Node_Name (R)) = "element"));

      P.NS := Get_Target_Name_Space (R);

      if XML.Get_Attr_Value (R, "abstract", False) = "true" then
         raise WSDL_Error with "abstract record not supported";
      end if;

      declare
         Name : constant String := XML.Get_Attr_Value (R, "name", False);
      begin
         --  Set record name, R is a complexType node

         P.Name   := O.Current_Name;
         P.T_Name := +Name;

         if Utils.No_NS (DOM.Core.Nodes.Node_Name (R)) = "element" then
            --  Skip enclosing element
            N := XML.First_Child (R);
         else
            N := R;
         end if;

         --  Enter complexType element

         N := XML.First_Child (N);

         --  Check for empty complexType

         if N /= null then
            --  Get first element, if we have a complexContent, parse

            if Utils.No_NS (DOM.Core.Nodes.Node_Name (N))
              = "complexContent"
            then
               N := XML.First_Child (N);

               --  We have an extension, we need to inline the element
               --  definition here.

               if N /= null
                 and then  Utils.No_NS (DOM.Core.Nodes.Node_Name (N))
                   = "extension"
               then
                  declare
                     Base : constant String :=
                              XML.Get_Attr_Value (N, "base", False);
                     CT   : DOM.Core.Node;
                  begin
                     --  Get type whose name is Base

                     CT := Get_Node
                       (XML.First_Child (DOM.Core.Node (Document)),
                        "types.schema.complexType", Base);

                     --  Move to the sequence

                     CT := XML.First_Child (CT);

                     --  Get all elements

                     declare
                        NL : constant DOM.Core.Node_List :=
                               DOM.Core.Nodes.Child_Nodes (CT);
                     begin
                        for K in 0 .. DOM.Core.Nodes.Length (NL) - 1 loop
                           declare
                              N : constant DOM.Core.Node :=
                                    DOM.Core.Nodes.Item (NL, K);
                           begin
                              if DOM.Core.Nodes.Node_Name (N)
                                /= "#text"
                              then
                                 Parameters.Append
                                   (P.P, Parse_Parameter (O, N, Document));
                              end if;
                           end;
                        end loop;
                     end;
                  end;

                  --  Move past extension node

                  N := XML.First_Child (N);
               end if;
            end if;

            N := XML.First_Child (N);

            while N /= null loop
               Parameters.Append (P.P, Parse_Parameter (O, N, Document));
               N := XML.Next_Sibling (N);
            end loop;
         end if;

         return P;
      end;
   end Parse_Record;

   -------------------
   -- Parse_Service --
   -------------------

   procedure Parse_Service
     (O        : in out Object'Class;
      Service  : DOM.Core.Node;
      Document : WSDL.Object)
   is
      Port, N       : DOM.Core.Node;
      Name          : Unbounded_String;
      Documentation : Unbounded_String;
      Location      : Unbounded_String;
      Binding       : Unbounded_String;
   begin
      Trace ("(Parse_Service)", Service);

      Name := +XML.Get_Attr_Value (Service, "name");

      N := Get_Node (Service, "documentation");

      if N /= null then
         DOM.Core.Nodes.Normalize (N);
         Documentation :=
           +DOM.Core.Nodes.Node_Value (DOM.Core.Nodes.First_Child (N));
      end if;

      Port := Get_Node (Service, "port");

      if Port = null then
         raise WSDL_Error with "port definition not found";
      end if;

      N := Get_Node (Port, Utils.With_NS (-NS_SOAP, "address"), NS => True);

      if N /= null then
         Location := +XML.Get_Attr_Value (N, "location");
      end if;

      Start_Service (O, -Name, -Documentation, -Location);

      --  Look for the right binding

      Binding := +XML.Get_Attr_Value (Port, "binding", False);

      N := Get_Node
        (XML.First_Child (DOM.Core.Node (Document)), "binding", -Binding);

      if N = null then
         raise WSDL_Error
           with "binding for " & (-Binding) & " not found.";
      end if;

      Parse_Binding (O, N, Document);

      End_Service (O, -Name);
   end Parse_Service;

   ------------------
   -- Parse_Simple --
   ------------------

   function Parse_Simple
     (O        : Object'Class;
      R        : DOM.Core.Node;
      Document : WSDL.Object) return Parameters.Parameter
   is
      pragma Unreferenced (Document);

      function Build_Derived
        (Name, Base : String;
         E          : DOM.Core.Node) return Parameters.Parameter;
      --  Returns the derived type definition

      function Build_Enumeration
        (Name, Base : String;
         E          : DOM.Core.Node) return Parameters.Parameter;
      --  Returns the enumeration type definition

      -------------------
      -- Build_Derived --
      -------------------

      function Build_Derived
        (Name, Base : String;
         E          : DOM.Core.Node) return Parameters.Parameter
      is
         P : Parameters.Parameter (Parameters.K_Derived);
      begin
         P.NS := Get_Target_Name_Space (DOM.Core.Nodes.Parent_Node (E));

         P.Name   := O.Current_Name;
         P.D_Name := +Name;

         if WSDL.Is_Standard (Base) then
            P.Parent_Type := To_Type (Base);

            if P.Parent_Type = WSDL.P_Character then
               Check_Character (R);
            end if;

         else
            --  We do not support derived type at more than one level for
            --  now.

            raise WSDL_Error with "Parent type must be a standard type.";
         end if;

         return P;
      end Build_Derived;

      -----------------------
      -- Build_Enumeration --
      -----------------------

      function Build_Enumeration
        (Name, Base : String;
         E          : DOM.Core.Node) return Parameters.Parameter
      is
         pragma Unreferenced (Base);

         use type Parameters.E_Node_Access;

         P : Parameters.Parameter (Parameters.K_Enumeration);
         N : DOM.Core.Node := E;
         D : Parameters.E_Node_Access;
      begin
         P.NS := Get_Target_Name_Space (DOM.Core.Nodes.Parent_Node (E));

         P.Name   := O.Current_Name;
         P.E_Name := +Name;

         while N /= null
           and then Utils.No_NS (DOM.Core.Nodes.Node_Name (E)) = "enumeration"
         loop
            declare
               Value    : constant String :=
                            XML.Get_Attr_Value (N, "value", False);
               New_Node : constant Parameters.E_Node_Access :=
                            new Parameters.E_Node'
                              (To_Unbounded_String (Value), null);
            begin
               if D = null then
                  P.E_Def := New_Node;
               else
                  D.Next := New_Node;
               end if;

               D := New_Node;
            end;

            N := XML.Next_Sibling (N);
         end loop;

         return P;
      end Build_Enumeration;

      N, E : DOM.Core.Node;

      Name : Unbounded_String;
      Base : Unbounded_String;

   begin
      Trace ("(Parse_Simple)", R);

      pragma Assert
        (R /= null
         and then Utils.No_NS (DOM.Core.Nodes.Node_Name (R)) = "simpleType");

      Name := +XML.Get_Attr_Value (R, "name", False);

      --  Enter simpleType restriction

      N := XML.First_Child (R);

      Base := +XML.Get_Attr_Value (N, "base", False);

      --  Check if this is an enumeration

      E := XML.First_Child (N);

      if E /= null
        and then Utils.No_NS (DOM.Core.Nodes.Node_Name (E)) = "enumeration"
      then
         return Build_Enumeration (-Name, -Base, E);
      else
         return Build_Derived (-Name, -Base, N);
      end if;
   end Parse_Simple;

   -----------
   -- Trace --
   -----------

   procedure Trace (Message : String; N : DOM.Core.Node) is
   begin
      if Verbose_Mode = 2 then
         Text_IO.Put_Line (Message);

         if N = null then
            Text_IO.Put_Line ("   Node is null.");
         else
            declare
               Name : constant String :=
                        DOM.Core.Nodes.Local_Name (N);
               Atts : constant DOM.Core.Named_Node_Map :=
                        DOM.Core.Nodes.Attributes (N);
            begin
               Text_IO.Put_Line ("   " & Name);

               for K in 0 .. DOM.Core.Nodes.Length (Atts) - 1 loop
                  Text_IO.Put ("      ");
                  declare
                     N     : constant DOM.Core.Node :=
                               DOM.Core.Nodes.Item (Atts, K);
                     Name  : constant String := DOM.Core.Nodes.Local_Name (N);
                     Value : constant String := DOM.Core.Nodes.Node_Value (N);
                  begin
                     Text_IO.Put (Name & " = " & Value);
                  end;
                  Text_IO.New_Line;
               end loop;
            end;
         end if;
      end if;
   end Trace;

   -------------
   -- Verbose --
   -------------

   procedure Verbose (Level : Verbose_Level := 1) is
   begin
      Verbose_Mode := Level;
   end Verbose;

end SOAP.WSDL.Parser;
