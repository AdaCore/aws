------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2003-2025, AdaCore                     --
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

package WSDL_3 is

   use Ada.Strings.Unbounded;

   type Rec1 is record
      Item1 : Integer;
      Item2 : Natural;
      Item3 : Positive;
   end record;

   type Rec2 is record
      Field1 : Rec1;
      Field2 : Character;
      Field3 : Unbounded_String;
      Field4 : Long_Float;
   end record;

   package My_Set_Pkg is
     new Ada.Containers.Vectors (Positive, Integer);

   type Rec3 is record
      S : My_Set_Pkg.Vector;
   end record;

   function Image_Rec1 (Rec : Rec1) return String;

   function Image_Rec2 (Rec : Rec2) return String;

   function Image_Rec3 (Rec : Rec3) return String;

end WSDL_3;
