------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2008-2012, AdaCore                     --
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

with SOAP.Utils;

package Pck is

   type Small is new Integer range 1 .. 8;

   type Color is (Red, Green, Blue);

   type Rec is record
      V : Natural;
      S : Small;
      C : Color;
   end record;

   procedure Call (R : Rec);

   procedure Print (X : Integer; Name : String);

   package Tab_Pck is
      new Ada.Containers.Vectors (Positive, Boolean);

   type R_Rec is record
      V : Natural;
      S : Small;
      C : Color;
      F : Long_Float;
      G : Rec;
      A : Tab_Pck.Vector;
   end record;

   procedure Big (R : R_Rec);

end Pck;
