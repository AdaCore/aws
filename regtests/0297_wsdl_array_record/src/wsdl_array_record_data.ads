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

with WSDL_Array_Record_Types;

package WSDL_Array_Record_Data is

   use WSDL_Array_Record_Types;

   function Test_1 (Item : Enumeration_Record_Type) return Enumeration_Type;

   function Test_2 (Item : Enumeration_Type) return Enumeration_Record_Type;

   package Sub_Record_List_Type_Safe_Pointer
     renames WSDL_Array_Record_Types.Sub_Record_List_Type_Safe_Pointer;

end WSDL_Array_Record_Data;
