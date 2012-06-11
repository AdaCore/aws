------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2011-2012, AdaCore                     --
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

package body API2 is

   use Ada.Strings.Unbounded;

   K : Unbounded_String;
   V : Integer;

   ---------
   -- Set --
   ---------

   procedure Set (Key : String; Value : Integer) is
   begin
      K := To_Unbounded_String (Key);
      V := Value;
   end Set;

   ---------
   -- Get --
   ---------

   function Get (Key : String) return Integer is
   begin
      return V;
   end Get;

   ------------------
   -- Get_Last_Key --
   ------------------

   function Get_Last_Key return String is
   begin
      return To_String (K);
   end Get_Last_Key;

end API2;
