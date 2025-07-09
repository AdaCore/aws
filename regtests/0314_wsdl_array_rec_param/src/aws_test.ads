------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2018-2025, AdaCore                     --
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

with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;

with SOAP.Utils;

package AWS_Test is

   use Ada.Strings.Unbounded;

   type Data_Rec_Type is record
      A_Int : Integer;
      A_Flt : Float;
      A_Str : Unbounded_String;
   end record;

   package Data_Rec_Array_Type_Pkg is
     new Ada.Containers.Vectors (Positive, Data_Rec_Type);

  function Get_Test_Name (Data : Data_Rec_Type) return String;
  function Get_Test_Name2
    (Data : Data_Rec_Array_Type_Pkg.Vector) return String;

end AWS_Test;
