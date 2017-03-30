------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                       Copyright (C) 2017, AdaCore                        --
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

with SOAP.Utils;

package WSDL_Enum_Array_Types is

   type Num is (First, Second, Third);

   type List_Type is array (Positive range <>) of Num;
   type List_Access is access List_Type;

   package List_Type_Safe_Pointer is
      new SOAP.Utils.Safe_Pointers (List_Type, List_Access);

   type Record_Type is record
      Value : Integer;
      C     : Character;
      L     : List_Type_Safe_Pointer.Safe_Pointer;
   end record;

end WSDL_Enum_Array_Types;
