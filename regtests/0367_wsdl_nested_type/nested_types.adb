------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                       Copyright (C) 2025, AdaCore                        --
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

with Ada.Text_IO;

with myCompany.myDataService.myDataService_Type_Pkg;
with myCompany.myDataService.myIncludedData_Type_Pkg;

package body Nested_Types is

   use Ada;
   use myCompany.myDataService;

   ----------
   -- Dump --
   ----------

   procedure Dump
     (P : Data_Service.Types.myDataService_Type)
   is
   begin
      --  Dump P
      Text_IO.Put_Line ("P " & P.C'Img);

      case P.C is
         when myDataService_Type_Pkg.C0 =>
            null;
         when myDataService_Type_Pkg.C1 =>
            Text_IO.Put_Line
              ("   * myOutput " & P.myOutput'Img);
         when myDataService_Type_Pkg.C2 =>
            Text_IO.Put_Line
              ("   * myIncludedData   ");
            Dump (P.myIncludedData);
      end case;
   end Dump;

   procedure Dump
     (P : Data_Service.Types.myIncludedData_Type)
   is
      use myCompany.myDataService;
   begin
      --  Dump P
      Text_IO.Put_Line ("P " & P.C'Img);

      case P.C is
         when myIncludedData_Type_Pkg.C0 =>
            null;
         when myIncludedData_Type_Pkg.C1 =>
            Text_IO.Put_Line
              ("   * intdInput " & P.intdInput'Img);
         when myIncludedData_Type_Pkg.C2 =>
            Text_IO.Put_Line
              ("   * intdOutput " & P.intdOutput'Img);
      end case;
   end Dump;

end Nested_Types;
