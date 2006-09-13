------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                            Copyright (C) 2004                            --
--                                ACT-Europe                                --
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

with Ada.Characters.Handling;
with Ada.Strings.Fixed;

with AWS.Services.Split_Pages.Shared;
with AWS.Utils;

package body AWS.Services.Split_Pages.Uniform.Alpha is

   use Ada;

   ---------------------
   -- Get_Page_Ranges --
   ---------------------

   function Get_Page_Ranges
     (This  : in Splitter;
      Table : in Templates.Translate_Set)
      return Ranges_Table
   is
      use Templates;

      procedure Add_Entry (C : in Character; Position : in Positive);
      --  Set index for character C entry

      Self    : Splitter renames Splitter (This.Self.all);
      Result  : constant Ranges_Table
        := Get_Page_Ranges (Uniform.Splitter (This), Table);
      Key_Vec : Tag := Shared.Associated_Vector (Table, To_String (Self.Key));
      Initial : Character;

      ---------------
      -- Add_Entry --
      ---------------

      procedure Add_Entry (C : in Character; Position : in Positive) is
      begin
         if C < Initial then
            raise Splitter_Error;
         end if;

         case C is
            when ' ' =>
               Self.Lines (1) := Position;
            when '0' .. '9' =>
               Self.Lines (2) := Position;
            when 'A' .. 'Z' =>
               Self.Lines (Alpha_Value (C)) := Position;
            when others =>
               raise Splitter_Error;
         end case;
      end Add_Entry;

   begin
      --  Clear context from (possible) previous runs

      Clear (Self.S_HREFS_V);
      Clear (Self.S_INDEXES_V);
      Self.Lines := (others => 0);

      --  Build table

      declare
         use Ada.Characters.Handling;
         use Ada.Strings.Fixed;
         Name : constant String := Trim (Item (Key_Vec, 1), Strings.Left);
      begin
         if Name = "" then
            Initial := ' ';
         else
            Initial := To_Upper (Name (Name'First));
         end if;
         Add_Entry (Initial, 1);
      end;

      for I in 2 .. Size (Key_Vec) loop
         declare
            use Ada.Characters.Handling;
            use Ada.Strings.Fixed;
            Name : constant String := Trim (Item (Key_Vec, I), Strings.Left);
            New_Initial : Character;
         begin
            if Name = "" then
               New_Initial := ' ';
            else
               New_Initial := To_Upper (Name (Name'First));
            end if;

            if New_Initial /= Initial then
               Add_Entry (New_Initial, I);
               Initial := New_Initial;
            end if;
         end;
      end loop;

      return Result;
   end Get_Page_Ranges;

   ----------------------
   -- Get_Translations --
   ----------------------

   function Get_Translations
     (This   : in Splitter;
      Page   : in Positive;
      URIs   : in URI_Table;
      Ranges : in Ranges_Table)
      return Templates.Translate_Set
   is
      use Templates;

      procedure Add_Ref (Line : in Natural);
      --  ???

      Self    : Splitter renames Splitter (This.Self.all);
      Result  : Templates.Translate_Set
        := Get_Translations (Uniform.Splitter (This), Page, URIs, Ranges);
      R_Index : Natural := 1;

      -------------
      -- Add_Ref --
      -------------

      procedure Add_Ref (Line : in Natural) is
      begin
         if Line = 0 then
            Self.S_HREFS_V :=  Self.S_HREFS_V & " ";

         else
            while Ranges (R_Index).Last < Line loop
               R_Index := R_Index + 1;
            end loop;

            Self.S_HREFS_V
              := Self.S_HREFS_V
                & (URIs (R_Index) & '#'
                   & Utils.Image (Line - Ranges (R_Index).First + 1));
         end if;
      end Add_Ref;

   begin
      if Size (Self.S_HREFS_V) = 0 then
         --  Not yet initialized

         Self.S_INDEXES_V := +"<>";
         Add_Ref (Self.Lines (1));

         Self.S_INDEXES_V := Self.S_INDEXES_V & "0..9";
         Add_Ref (Self.Lines (2));

         for C in Character range 'A' .. 'Z' loop
            Self.S_INDEXES_V := Self.S_INDEXES_V & C;
            Add_Ref (Self.Lines (Alpha_Value (C)));
         end loop;
      end if;

      Insert (Result, Assoc ("S_HREFS_V", Self.S_HREFS_V));
      Insert (Result, Assoc ("S_INDEXES_V", Self.S_INDEXES_V));
      return Result;
   end Get_Translations;

   -------------
   -- Set_Key --
   -------------

   procedure Set_Key (This : in out Splitter; Key : in String) is
   begin
      This.Key := To_Unbounded_String (Key);
   end Set_Key;

end AWS.Services.Split_Pages.Uniform.Alpha;
