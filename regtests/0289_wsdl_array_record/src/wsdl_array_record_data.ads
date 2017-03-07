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

package WSDL_Array_Record_Data is

   type T1L is array (Positive range <>) of Integer;
   type T1L_Access is access T1L;

   package T1L_Safe_Pointer is new SOAP.Utils.Safe_Pointers (T1L, T1L_Access);

   type T2 is record
      Id : Integer;
      I2 : T1L_Safe_Pointer.Safe_Pointer;
   end record;

   type T2L is array (Positive range <>) of T2;
   type T2L_Access is access T2L;
   package T2L_Safe_Pointer is new SOAP.Utils.Safe_Pointers (T2L, T2L_Access);

   type T3 is record
      I3 : T2L_Safe_Pointer.Safe_Pointer;
   end record;

   procedure Notification (Item : T3);

end WSDL_Array_Record_Data;
