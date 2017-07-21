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

with Character_Types;

package Character_Services is

   use Character_Types;

   procedure Test_Character_Record
     (Value_Record : Character_Record_Type);

   procedure Test_Character_Parameter
     (Value_Param : Character);

   procedure Test_Array_Character_Record
     (Value_Record : Array_Character_Record_Type);

   procedure Test_Array_Character_Parameter
     (Value_Param : Array_Character_Type);

   procedure Test_Array_Record_Character_Record
     (Value_Record : Array_Record_Character_Record_Type);

   procedure Test_Array_Record_Character_Parameter
     (Value_Param : Array_Record_Character_Type);

end Character_Services;
