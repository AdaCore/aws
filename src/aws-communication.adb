------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                            Copyright (C) 2000                            --
--                      Dmitriy Anisimkov & Pascal Obry                     --
--                                                                          --
--  This library is free software; you can redistribute it and/or modify    --
--  it under the terms of the GNU General Public License as published by    --
--  the Free Software Foundation; either version 2 of the License, or (at   --
--  your option) any later version.                                         --
--                                                                          --
--  This library is distributed in the hope that it will be useful, but     --
--  WITHOUT ANY WARRANTY; without even the implied warranty of              --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU       --
--  General Public License for more details.                                --
--                                                                          --
--  You should have received a copy of the GNU General Public License       --
--  along with this library; if not, write to the Free Software Foundation, --
--  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.          --
--                                                                          --
--  As a special exception, if other files instantiate generics from this   --
--  unit, or you link this unit with other files to produce an executable,  --
--  this  unit  does not  by itself cause  the resulting executable to be   --
--  covered by the GNU General Public License. This exception does not      --
--  however invalidate any other reasons why the executable file  might be  --
--  covered by the  GNU Public License.                                     --
------------------------------------------------------------------------------

--  $Id$

package body AWS.Communication is

   ---------------
   -- Parameter --
   ---------------

   function Parameters
     (P1, P2, P3, P4, P5 : in String := "")
      return Parameter_Set
   is
      procedure Add (P : in String);
      --  Add parameter P into PS.

      PS : Parameter_Set (1 .. 5);
      I  : Natural := 0;

      procedure Add (P : in String) is
      begin
         I := I + 1;
         PS (I) := To_Unbounded_String (P);
      end Add;

   begin
      if P1 /= "" then
         Add (P1);
      end if;

      if P2 /= "" then
         Add (P2);
      end if;

      if P3 /= "" then
         Add (P3);
      end if;

      if P4 /= "" then
         Add (P4);
      end if;

      if P5 /= "" then
         Add (P5);
      end if;

      return PS (1 .. I);
   end Parameters;

end AWS.Communication;

