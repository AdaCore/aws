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

with Asis;

package Ada2WSDL is

   Version : constant String := "1.3.0";

   Fatal_Error     : exception;
   --  Raised when a non-recoverable error has been found

   Parameter_Error : exception;
   --  Raised if ada2wsdl received a wrong option/parameter

   Spec_Error      : exception;
   --  Raised if ada2wsdl has found a problem while parsing the Ada spec

   function Location (E : Asis.Element) return String;
   --  Returns E's location in the form <line>:<column>

   procedure Raise_Spec_Error
     (E       : Asis.Element;
      Message : String);
   pragma No_Return (Raise_Spec_Error);
   --  Raises Spec_Error exception with the given message. Add a source
   --  location information for entity E.

end Ada2WSDL;
