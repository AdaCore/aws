------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2000-2001                          --
--                                ACT-Europe                                --
--                                                                          --
--  Authors: Dmitriy Anisimkov - Pascal Obry                                --
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

with Ada.Strings.Fixed;
with AWS.Parameters.Set;
with Ada.Text_IO;

package body AWS.Headers.Set is

   subtype List is Parameters.List;

   ---------
   -- Add --
   ---------

   procedure Add (C : in out Container; Name, Value : in String)
   is
   begin
      Parameters.Set.Add (List (C), Name, Value);
   end Add;

   -----------
   -- Free --
   -----------

   procedure Free (C : in out Container) is
   begin
      Parameters.Set.Free (List (C));
   end Free;

   -----------
   -- Parse --
   -----------

   procedure Parse (C : in out Container; Line : in String) is
      use Ada.Strings;
      Delimiter : Natural := Fixed.Index (Line, ":");
   begin

      if Debug_Flag then
         Ada.Text_IO.Put_Line ('>' & Line);
      end if;

      if Delimiter = 0 then
         --  just ignore for now.
         raise Header_Format_Error;
      end if;

      Parameters.Set.Add (List (C), Line (Line'First .. Delimiter - 1),
         Fixed.Trim (Line (Delimiter + 1 .. Line'Last), Both));

   end Parse;

   -----------
   -- Reset --
   -----------

   procedure Reset (C : in out Container) is
   begin
      Parameters.Set.Reset (List (C));
      Parameters.Set.Case_Sensitive (List (C), False);
   end Reset;

end AWS.Headers.Set;

