------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2003-2020, AdaCore                     --
--                                                                          --
--  This is free software;  you can redistribute it  and/or modify it       --
--  under terms of the  GNU General Public License as published  by the     --
--  Free Software  Foundation;  either version 3,  or (at your option) any  --
--  later version.  This software is distributed in the hope  that it will  --
--  be useful, but WITHOUT ANY WARRANTY;  without even the implied warranty --
--  of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU     --
--  General Public License for  more details.                               --
--                                                                          --
--  You should have  received  a copy of the GNU General  Public  License   --
--  distributed  with  this  software;   see  file COPYING3.  If not, go    --
--  to http://www.gnu.org/licenses for a complete copy of the license.      --
------------------------------------------------------------------------------

with Libadalang.Analysis;

package Ada2WSDL is

   Version : constant String := "3.0";

   Fatal_Error     : exception;
   --  Raised when a non-recoverable error has been found

   Parameter_Error : exception;
   --  Raised if ada2wsdl received a wrong option/parameter

   Spec_Error      : exception;
   --  Raised if ada2wsdl has found a problem while parsing the Ada spec

   package LaL renames Libadalang.Analysis;

   function Location (Node : LaL.Ada_Node'Class) return String;
   --  Returns E's location in the form <line>:<column>

   procedure Raise_Spec_Error
     (Node    : LaL.Ada_Node'Class;
      Message : String) with No_Return;
   --  Raises Spec_Error exception with the given message. Add a source
   --  location information for entity E.

end Ada2WSDL;
