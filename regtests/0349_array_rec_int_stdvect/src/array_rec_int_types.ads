------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                       Copyright (C) 2022, AdaCore                        --
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

with Defs;

package Array_Rec_Int_Types is

   subtype My_Int is Defs.My_Int;
   use type Defs.My_Int;

   package Integer_List_Type is
     new Ada.Containers.Vectors (Positive, Integer);

   package My_Int_List_Type is
     new Ada.Containers.Vectors (Positive, Defs.My_Int);

   type Array_Integer_Type is record
      Value1 : Integer_List_Type.Vector;
      Value2 : My_Int_List_Type.Vector;
   end record;

end Array_Rec_Int_Types;
