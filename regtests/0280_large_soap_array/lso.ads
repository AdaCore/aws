------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                      Copyright (C) 2016, AdaCore                         --
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

package LSO is

   type Set_Of_Int is array (Positive range <>) of Integer;
   type Set_Of_Int_Access is access Set_Of_Int;

   package Set_Of_Int_Safe_Pointer is new SOAP.Utils.Safe_Pointers
     (T        => Set_Of_Int,
      T_Access => Set_Of_Int_Access);

   function Echo (Object : Set_Of_Int) return Set_Of_Int;

end LSO;
