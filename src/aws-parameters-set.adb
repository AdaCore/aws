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

with Ada.Strings.Unbounded;
with Ada.Strings.Fixed;

with AWS.URL;
with AWS.Containers.Tables.Set;

package body AWS.Parameters.Set is

   use Ada.Strings.Unbounded;
   use AWS.Containers;

   ---------
   -- Add --
   ---------

   procedure Add
     (Parameter_List : in out List;
      Name, Value    : in     String)
   is
   begin
      Tables.Set.Add
        (Tables.Table_Type (Parameter_List),
         URL.Decode (Name),
         URL.Decode (Value));
   end Add;

   ---------
   -- Add --
   ---------

   procedure Add (Parameter_List : in out List; Parameters : in String) is
      use Ada.Strings;
      P : String renames Parameters;
      C : Positive := P'First;
      I : Natural;
      S : Positive := P'First;
      E : Natural;
   begin
      --  Skip leading question mark if present.

      if P /= "" and then P (C) = '?' then
         C := Positive'Succ (C);
         S := Positive'Succ (S);
      end if;

      Parameter_List.Parameters := To_Unbounded_String ('?' & P (C .. P'Last));

      loop
         I := Fixed.Index (P (C .. P'Last), "=");

         exit when I = 0;

         S := I + 1;

         E := Fixed.Index (P (S .. P'Last), "&");

         if E = 0 then
            --  last parameter

            Add (Parameter_List, P (C .. I - 1), P (S .. P'Last));
            exit;

         else
            Add (Parameter_List, P (C .. I - 1), P (S .. E - 1));
            C := E + 1;
         end if;
      end loop;
   end Add;

   --------------------
   -- Case_Sensitive --
   --------------------

   procedure Case_Sensitive
     (Parameter_List : in out List;
      Mode           : in     Boolean) is
   begin
      Tables.Set.Case_Sensitive (Tables.Table_Type (Parameter_List), Mode);
   end Case_Sensitive;

   ----------
   -- Free --
   ----------

   procedure Free (Parameter_List : in out List) is
   begin
      Tables.Set.Free (Tables.Table_Type (Parameter_List));
   end Free;

   -----------
   -- Reset --
   -----------

   procedure Reset (Parameter_List : in out List) is
   begin
      Tables.Set.Reset (Tables.Table_Type (Parameter_List));
      Parameter_List.Parameters := Null_Unbounded_String;
   end Reset;

end AWS.Parameters.Set;
