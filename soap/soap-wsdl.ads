------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2003-2009, AdaCore                     --
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

--  This package provides services to handle WSDL

with DOM.Core;

package SOAP.WSDL is

   WSDL_Error : exception;

   NS_SOAP    : constant String := "http://schemas.xmlsoap.org/wsdl/soap/";
   NS_SOAPENC : constant String := "http://schemas.xmlsoap.org/soap/encoding/";
   NS_XSD     : constant String := "http://www.w3.org/2001/XMLSchema";
   NS_XSI     : constant String := "http://www.w3.org/1999/XMLSchema-instance";
   NS_WSDL    : constant String := "http://schemas.xmlsoap.org/wsdl/";

   type Object is private;

   function Load (Filename : String) return Object;
   --  Load and parse a WSDL document and return the XML tree representation

   type Parameter_Type is
     (P_Long, P_Integer, P_Short, P_Byte, P_Float, P_Double, P_String,
      P_Character, P_Boolean, P_Time, P_B64, P_Unsigned_Long, P_Unsigned_Int,
      P_Unsigned_Short, P_Unsigned_Byte, P_Any_Type);
   --  These are the types supported by the WSDL parser

   type Context_Type is (Parameter, Component);
   --  This is the context of the variable, either as a simple parameter or as
   --  a record or array component.

   function Is_Standard (XSD_Type : String) return Boolean;
   --  Returns true is XSD_Type is a standard type (not an array or a record)

   function To_Type (XSD_Type : String) return Parameter_Type;
   --  Returns the Ada parameter style for the XML type XSD_Type

   function To_Ada
     (P       : Parameter_Type;
      Context : Context_Type := Parameter)
      return String;
   --  Returns P's Ada type string representation

   procedure From_Ada
     (Ada_Type : String;
      Result   : out WSDL.Parameter_Type;
      Standard : out Boolean);
   --  Set Result with the type corresponding to the Ada type name

   function To_XSD (P : WSDL.Parameter_Type) return String;
   --  Returns the XSD type corresponding to P

   function V_Routine
     (P       : Parameter_Type;
      Context : Context_Type := Parameter)
      return String;
   --  Returns the V routine to use to get value for a Parameter_Type

   function Get_Routine
     (P       : Parameter_Type;
      Context : Context_Type := Parameter)
      return String;
   --  Returns the Get routine to use to get value for a Parameter_Type

   function Set_Routine
     (P       : Parameter_Type;
      Context : Context_Type := Parameter)
      return String;
   --  Returns the constructor to use to create a Parameter_Type

   function Set_Type (P : Parameter_Type) return String;
   --  Returns SOAP type for P

private

   type Object is new DOM.Core.Document;

end SOAP.WSDL;
