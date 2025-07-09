------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                    Copyright (C) 2017-2025, AdaCore                      --
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

with Ada.Strings.Unbounded;
with Ada.Containers.Vectors;

with SOAP.Utils;

package API is

   use Ada.Strings.Unbounded;

   type R is record
      Code, Variant : Unbounded_String;
   end record;

   package Arr_Pkg is new Ada.Containers.Vectors (Positive, R);

   type Part is record
      V  : Integer;
      C1 : Unbounded_String;
      It : Arr_Pkg.Vector;
      C2 : Unbounded_String;
   end record;

   procedure Call (P : Part);

end API;
