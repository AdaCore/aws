------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                       Copyright (C) 2015, AdaCore                        --
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

with AWS.Containers.Key_Value;

package SOAP.WSDL.Schema is

   use type DOM.Core.Node;

   type Binding_Style is (RPC, Document);

   type Encoding_Style is (Encoded, Literal);
   --  SOAP encoding style for the entities

   subtype Definition is AWS.Containers.Key_Value.Map;
   --  This map is used to map schema type-names and parameters to actual types
   --  in AWS. This is needed when parting in literal encoding as the type
   --  information is not given and must refer to the WSDL schema. Also in
   --  document style binding the actual procedure to call is computed based
   --  on the procedure call signature (the parameter names). For this, a
   --  definition map record the following informations:
   --
   --     1. The binding style for the schema (Document/RPC)
   --        key:   @binding.style
   --        value: document/rpc
   --
   --     2. The encoding style for each procedure's input and output messages
   --        key:   @<object>.encoding
   --        value: encoded/literal
   --
   --     3. The SOAP operation for each signature
   --        key:   @<param1>[:<param2>]
   --        value: <SOAP procedure call name>
   --
   --     4. The root type for each types in the schema
   --        key:   <type_name>
   --        value: <xsd_type>

   Empty : constant Definition;

   procedure Set_Binding_Style
     (Schema : in out Definition; Style : Binding_Style);
   --  Set the actual binding style for the schema

   function Get_Binding_Style (Schema : Definition) return Binding_Style;
   --  Get the binding style for the schema

   function Get_Encoding_Style
     (Schema    : Definition;
      Operation : String) return Encoding_Style;
   --  Returns the encoding-style for the given operation

   procedure Set_Encoding_Style
     (Schema    : in out Definition;
      Operation : String;
      Encoding  : Encoding_Style);
   --  Set the encoding-style for the given operation

   function Get_Call_For_Signature
     (Schema    : Definition;
      Signature : String) return String;
   --  Returns the SOAP operation for the given call signature

   subtype URL is String;

   procedure Register (Namespace : URL; Node : DOM.Core.Node) with
     Pre  => Node /= null,
     Post => Contains (Namespace);
   --  Register a Namespace (URL) for the given DOM tree

   function Contains (Namespace : URL) return Boolean;
   --  Returns True if the Namespace is known (has been registered)

   procedure For_All
     (Namespace : URL;
      Process   : not null access procedure (N : DOM.Core.Node));
   --  Go through all mixed namespaces

private
   Empty : constant Definition := AWS.Containers.Key_Value.Empty_Map;
end SOAP.WSDL.Schema;
