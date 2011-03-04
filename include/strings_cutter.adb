------------------------------------------------------------------------------
--                             Strings  Cutter                              --
--                                                                          --
--                   Copyright (C) 1995-2011, Pascal Obry                   --
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

with Ada.Strings.Unbounded;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with Ada.Unchecked_Deallocation;

package body Strings_Cutter is

   use Ada.Strings.Unbounded;

   type Slices_Index is array (Index_Values) of Natural;

   type Cut_String_Record is record
      Value       : Unbounded_String;
      Separators  : Unbounded_String;
      Field_Count : Index_Values      := 0;
      Index       : Slices_Index;
   end record;

   ----------
   -- Free --
   ----------

   procedure Unchecked_Free is
      new Ada.Unchecked_Deallocation (Cut_String_Record, Cut_String);

   ----------------
   -- String_Cut --
   ----------------

   procedure String_Cut (S : in out Cut_String) is
      use Ada.Strings;
      Value          : constant String := To_String (S.Value);
      Separators_Set : Maps.Character_Set;
      I              : Natural      := 0;
      K              : Index_Values := 1;
   begin
      S.Index := (others => 1);

      if Value'Length = 0 then
         S.Field_Count := 0;

      else
         Separators_Set := Maps.To_Set (To_String (S.Separators));

         loop
            I := Fixed.Index (Value (I + 1 .. Value'Last), Separators_Set);
            exit when I = 0;
            S.Index (K) := I - 1;
            K := K + 1;
         end loop;

         S.Index (K) := Value'Last;
         S.Field_Count := K;
      end if;
   end String_Cut;

   ------------
   -- Create --
   ------------

   procedure Create
     (S          :    out Cut_String;
      From       : in     String;
      Separators : in     String) is
   begin

      S := new Cut_String_Record;

      S.Value      := To_Unbounded_String (From);
      S.Separators := To_Unbounded_String (Separators);

      String_Cut (S);
   end Create;

   ---------
   -- Set --
   ---------

   procedure Set
     (S          : in out Cut_String;
      Separators : in     String) is
   begin
      S.Separators := To_Unbounded_String (Separators);
      String_Cut (S);
   end Set;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (S : in out Cut_String) is
   begin
      if S /= null then
         Unchecked_Free (S);
      end if;
   end Destroy;

   -----------------
   -- Field_Count --
   -----------------

   function Field_Count (S : in Cut_String) return Index_Values is
   begin
      return S.Field_Count;
   end Field_Count;

   -----------
   -- Field --
   -----------

   function Field
     (S     : in Cut_String;
      Index : in Index_Values) return String is
   begin
      case Index is
         when 0 =>
            return To_String (S.Value);
         when 1 =>
            return Slice (S.Value, 1, S.Index (1));
         when others =>
            return Slice (S.Value, S.Index (Index - 1) + 2, S.Index (Index));
      end case;
   end Field;

end Strings_Cutter;
