------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2009-2012, AdaCore                     --
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

package body API is

   use Ada;

   procedure Callme (X : Natural) is
   begin
      Text_IO.Put_Line ("Callme:" & Natural'Image (X));
   end Callme;

   procedure Empty is
   begin
      null;
   end Empty;

   procedure Callme_Message (X : Natural; Message : String) is
   begin
      Text_IO.Put_Line (Message & Natural'Image (X));
   end Callme_Message;

   function Get_Value return Natural is
   begin
      return 1;
   end Get_Value;

   function Length (Str : String) return Positive is
   begin
      return Str'Length;
   end Length;

end API;
