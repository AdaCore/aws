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

--  This package provides services to handle WSDL.

with DOM.Core;

package SOAP.WSDL is

   WSDL_Error : exception;

   type Object is private;

   function Load (Filename : in String) return Object;
   --  Load and parse a WSDL document and return the XML tree representation

   type Parameter_Type is (P_Integer, P_Float, P_String, P_Boolean, P_Time);
   --  These are the types supported by the WSDL parser

   function To_Type (XSD_Type : in String) return Parameter_Type;
   --  Returns the Ada parameter style for the XML type XSD_Type

   function Is_Standard (XSD_Type : in String) return Boolean;
   --  Returns true is XSD_Type is a standard type (not an array or a record)

   function To_Ada (P : in Parameter_Type) return String;
   --  Returns P's Ada type string representation

private

   type Object is new DOM.Core.Document;

end SOAP.WSDL;
