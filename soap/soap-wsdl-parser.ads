------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                            Copyright (C) 2003                            --
--                                ACT-Europe                                --
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

--  $Id$

with Ada.Strings.Unbounded;

with SOAP.WSDL.Parameters;

package SOAP.WSDL.Parser is

   ------------
   -- Parser --
   ------------

   type Object is tagged private;

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
      Input      : in     Parameters.P_Set;
      Output     : in     Parameters.P_Set;
      Fault      : in     Parameters.P_Set);
   --  Called for each SOAP procedure found in the WSDL document for the
   --  current service.

   procedure Parse
     (O        : in out Object'Class;
      Document : in     WSDL.Object);
   --  Parse document, call routines above.

   procedure Verbose (Activate : in Boolean := True);
   --  Activate verbose mode

   procedure Accept_RPC (O : in out Object'Class);
   --  Accept RPC binding as document style binding

private

   use Ada.Strings.Unbounded;

   type Parameter_Mode is (Input, Output, Fault);
   --  Current parameter parsing mode

   type All_Parameters is array (Parameter_Mode) of Parameters.P_Set;

   type Object is tagged record
      Proc         : Unbounded_String; -- SOAP procedure name
      SOAPAction   : Unbounded_String; -- SOAPAction string
      Mode         : Parameter_Mode;   -- Current parameter parsing mode
      Params       : All_Parameters;   -- All parameters
      Current_Name : Unbounded_String; -- Current parameter name
      Accept_RPC   : Boolean := False;
   end record;

end SOAP.WSDL.Parser;
