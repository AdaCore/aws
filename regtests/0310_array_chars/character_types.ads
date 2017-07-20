------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                      Copyright (C) 2017, AdaCore                         --
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

package Character_Types is

   type Character_Record_Type is record
      Value : Character;
   end record;

   --  Array of characters

   type Array_Character_Type is array (Positive range <>) of Character;

   type Array_Character_Access is access Array_Character_Type;

   package Array_Character_Type_Safe_Pointer is new SOAP.Utils.Safe_Pointers
     (Array_Character_Type, Array_Character_Access);

   type Array_Character_Record_Type is record
      Value : Array_Character_Type_Safe_Pointer.Safe_Pointer;
   end record;

   --  Array of record of characters

   type Array_Record_Character_Type is
     array (Positive range <>) of Character_Record_Type;

   type Array_Record_Character_Access is access Array_Record_Character_Type;

   package Array_Record_Character_Type_Safe_Pointer is
      new SOAP.Utils.Safe_Pointers
        (Array_Record_Character_Type, Array_Record_Character_Access);

   type Array_Record_Character_Record_Type is record
      Value : Array_Record_Character_Type_Safe_Pointer.Safe_Pointer;
   end record;

end Character_Types;
