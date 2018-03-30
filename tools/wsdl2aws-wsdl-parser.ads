------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2003-2018, AdaCore                     --
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

with SOAP.Name_Space;
with SOAP.Types;
with SOAP.WSDL.Schema;

with WSDL2AWS.WSDL.Parameters;

private with Ada.Strings.Unbounded;
private with Ada.Containers.Indefinite_Ordered_Sets;
private with WSDL2AWS.WSDL.Types;

package WSDL2AWS.WSDL.Parser is

   WSDL_Error : exception renames SOAP.WSDL.WSDL_Error;

   type Parameter_Mode is (Input, Output, Fault);
   --  Kind of parameters in the WSDL

   ------------
   -- Parser --
   ------------

   type Object is tagged limited private;

   procedure Start_Service
     (O                  : in out Object;
      Name               : String;
      Root_Documentation : String;
      Documentation      : String;
      Location           : String) is null;
   --  Called for every service in the WSDL document

   procedure End_Service
     (O    : in out Object;
      Name : String) is null;
   --  Called at the end of the service

   procedure New_Procedure
     (O             : in out Object;
      Proc          : String;
      Documentation : String;
      SOAPAction    : String;
      Wrapper_Name  : String;
      Namespace     : SOAP.Name_Space.Object;
      Input         : Parameters.P_Set;
      Output        : Parameters.P_Set;
      Fault         : Parameters.P_Set) is null;
   --  Called for each SOAP procedure found in the WSDL document for the
   --  current service.

   procedure Parse
     (O        : in out Object'Class;
      Document : SOAP.WSDL.Object);
   --  Parse document, call routines above

   -------------------
   -- Configuration --
   -------------------

   type Verbose_Level is new Natural range 0 .. 2;

   procedure Verbose (Level : Verbose_Level := 1);
   --  Activate verbose mode

   procedure Accept_Document (O : in out Object'Class);
   --  Accept Document binding as RPC style binding

   procedure Continue_On_Error;
   --  Set continue on error. This means that the parser will not stop at the
   --  first error encountered, it will skip the SOAP routines having problems
   --  and it will try to parse the next one. This option is useful to
   --  generate stub/skeleton for the part of the WSDL document using
   --  supported types for example.

   procedure Exclude (O : in out Object; Operation : String);
   --  Register operation to be excluded from the code generation

   function Style (O : Object'Class) return SOAP.WSDL.Schema.Binding_Style;
   --  Returns the binding style for the parsed WSDL

   function Encoding
     (O : Object'Class; Kind : Parameter_Mode)
      return SOAP.Types.Encoding_Style;
   --  Returns the encoding style for the proc

   function xsd (O : Object'Class) return SOAP.Name_Space.Object;
   --  Returns the xsd name-space

   function xsi (O : Object'Class) return SOAP.Name_Space.Object;
   --  Returns the xsi name-space

   function env (O : Object'Class) return SOAP.Name_Space.Object;
   --  Returns the env name-space

   function enc (O : Object'Class) return SOAP.Name_Space.Object;
   --  Returns the enc name-space

private

   use Ada;
   use Ada.Strings.Unbounded;

   package Name_Set is new Containers.Indefinite_Ordered_Sets (String);

   type All_Parameters is array (Parameter_Mode) of Parameters.P_Set;

   type Object_Access is access all Object'Class;

   type Object is tagged limited record
      Self            : Object_Access := Object'Unchecked_Access;
      Proc            : Unbounded_String; -- SOAP procedure name
      Documentation   : Unbounded_String; -- Associated documentation
      SOAPAction      : Unbounded_String; -- SOAPAction string
      Namespace       : SOAP.Name_Space.Object;
      Mode            : Parameter_Mode;   -- Current parameter parsing mode
      Params          : All_Parameters;   -- All parameters
      Current_Name    : Unbounded_String; -- Current parameter name
      Elmt_Name       : Unbounded_String; -- Current element name
      Enclosing_Types : Name_Set.Set;     -- The enclosing entity type
      Array_Elements  : Types.Object;     -- Type of the array's elements
      Array_Length    : Natural;          -- Number of items (0 = unbounded)
      Accept_Document : Boolean := False;
      Exclude         : Name_Set.Set;     -- Operation to exclude from gen
      No_Param        : Boolean := False; -- Disable param generation
      Style           : SOAP.WSDL.Schema.Binding_Style := SOAP.WSDL.Schema.RPC;
      --  Input/Output encoding
      I_Encoding      : SOAP.WSDL.Schema.Encoding_Style :=
                          SOAP.WSDL.Schema.Encoded;
      O_Encoding      : SOAP.WSDL.Schema.Encoding_Style :=
                          SOAP.WSDL.Schema.Encoded;
      --  Name-spaces as defined in WSDL definition node
      xsd             : SOAP.Name_Space.Object := SOAP.Name_Space.XSD;
      xsi             : SOAP.Name_Space.Object := SOAP.Name_Space.XSI;
      env             : SOAP.Name_Space.Object := SOAP.Name_Space.SOAPENV;
      enc             : SOAP.Name_Space.Object := SOAP.Name_Space.SOAPENC;
   end record;

end WSDL2AWS.WSDL.Parser;
