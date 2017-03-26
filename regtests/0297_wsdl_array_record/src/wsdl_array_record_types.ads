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

package WSDL_Array_Record_Types is

   type Enumeration_Type is (First, Second, Third);

   type Sub_Record_Type is record
      Value : Enumeration_Type;
   end record;

   type Sub_Record_List_Type is array (Positive range <>) of Sub_Record_Type;
   type Sub_Record_List_Access is access Sub_Record_List_Type;

   package Sub_Record_List_Type_Safe_Pointer is new SOAP.Utils.Safe_Pointers
     (Sub_Record_List_Type, Sub_Record_List_Access);

   type Enumeration_Record_Type is record
      Value : Enumeration_Type;
      List  : Sub_Record_List_Type_Safe_Pointer.Safe_Pointer;
   end record;

end WSDL_Array_Record_Types;
