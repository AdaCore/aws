------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                            Copyright (C) 2003                            --
--                                ACT-Europe                                --
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

--  $Id$

with Ada.Strings.Unbounded;

with SOAP.WSDL.Parameters;

package SOAP.WSDL.Parser is

   ------------
   -- Parser --
   ------------

   type Object is tagged limited private;

   procedure Start_Service
     (O             : in out Object;
      Name          : in     String;
      Documentation : in     String;
      Location      : in     String);
   --  Called for every service in the WSDL document

   procedure End_Service
     (O    : in out Object;
      Name : in     String);
   --  Called at the end of the service

   procedure New_Procedure
     (O          : in out Object;
      Proc       : in     String;
      SOAPAction : in     String;
      Namespace  : in     String;
      Input      : in     Parameters.P_Set;
      Output     : in     Parameters.P_Set;
      Fault      : in     Parameters.P_Set);
   --  Called for each SOAP procedure found in the WSDL document for the
   --  current service.

   procedure Parse
     (O        : in out Object'Class;
      Document : in     WSDL.Object);
   --  Parse document, call routines above.

   -------------------
   -- Configuration --
   -------------------

   type Verbose_Level is new Natural range 0 .. 2;

   procedure Verbose (Level : in Verbose_Level := 1);
   --  Activate verbose mode

   procedure Accept_Document (O : in out Object'Class);
   --  Accept Document binding as RPC style binding

   procedure Continue_On_Error;
   --  Set continue on error. This means that the parser will not stop at the
   --  first error encountered, it will skip the SOAP routines having problems
   --  and it will try to parse the next one. This option is useful to
   --  generate stub/skeleton for the part of the WSDL document using
   --  supported types for example.

private

   use Ada.Strings.Unbounded;

   type Parameter_Mode is (Input, Output, Fault);
   --  Current parameter parsing mode

   type All_Parameters is array (Parameter_Mode) of Parameters.P_Set;

   type Object_Access is access all Object'Class;

   type Object is tagged limited record
      Self            : Object_Access := Object'Unchecked_Access;
      Proc            : Unbounded_String; -- SOAP procedure name
      SOAPAction      : Unbounded_String; -- SOAPAction string
      Namespace       : Unbounded_String;
      Mode            : Parameter_Mode;   -- Current parameter parsing mode
      Params          : All_Parameters;   -- All parameters
      Current_Name    : Unbounded_String; -- Current parameter name
      Array_Elements  : Unbounded_String; -- Type of the array's elements
      Array_Length    : Natural;          -- Number of items (0 = unbounded)
      Accept_Document : Boolean := False;
   end record;

end SOAP.WSDL.Parser;
