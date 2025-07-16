------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2015-2025, AdaCore                     --
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

package D_Management is

   type Facility_Mark_Type is (Add, Delete, Modify);

   type Mark_Rec is record
      Mark : Facility_Mark_Type;
   end record;

   type Facility_Change is record
      Old_Value : Ada.Strings.Unbounded.Unbounded_String;
      New_Value : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   package Field_Array_Pkg is
     new Ada.Containers.Vectors (Positive, Facility_Change);

   type Facility_Type is record
      Fields : Field_Array_Pkg.Vector;
      Change : Mark_Rec;
   end record;

   function Update_Target_Changes
      (A : String;
       B : String) return Facility_Type;

end D_Management;
