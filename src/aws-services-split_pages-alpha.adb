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

package body AWS.Services.Split_Pages.Alpha is

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

      procedure Set_Entry (C : in Character; Index : in Natural);
      --  Set index for character C entry

      Self    : Splitter renames Splitter (This.Self.all);
      Result  : Ranges_Table (1 .. 26 + 2);
      Res_Inx : Natural;
      Key_Vec : Tag := Shared.Associated_Vector (Table, To_String (Self.Key));
      Initial : Character;

      ---------------
      -- Set_Entry --
      ---------------

      procedure Set_Entry (C : in Character; Index : in Natural) is
      begin
         if C < Initial then
            --  Not sorted
            raise Splitter_Error;
         end if;

         case C is
            when ' ' =>
               Self.Index (1) := Index;
            when '0' .. '9' =>
               Self.Index (2) := Index;
            when 'A' .. 'Z' =>
               Self.Index (Alpha_Value (C)) := Index;
            when others =>
               --  Not an empty field, numeric or letter
               raise Splitter_Error;
         end case;
      end Set_Entry;

   begin
      --  Clear context from (possible) previous runs

      Clear (Self.HREFS_V);
      Clear (Self.INDEXES_V);
      Self.Index := (others => 0);

      --  Build table

      --  Initialize iteration with the first item

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

         Set_Entry (Initial, 1);
         Result (1).First := 1;
         Res_Inx          := 1;
      end;

      --  For all other items

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

            if New_Initial /= Initial
              and then (Initial not in '0' .. '9'
                          or else New_Initial not in '0' .. '9')
            then
               --  This is a new entry, record the last item for previous entry
               Result (Res_Inx).Last := I - 1;

               --  Initialize new entry
               Res_Inx := Res_Inx + 1;
               Result (Res_Inx).First := I;
               Set_Entry (New_Initial, Res_Inx);
               Initial := New_Initial;
            end if;
         end;
      end loop;

      --  Last item for the last entry

      Result (Res_Inx).Last := Size (Key_Vec);

      return Result (1 .. Res_Inx);
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
      pragma Unreferenced (Ranges);

      procedure Add_Entry (Index : in Natural);
      --  Append entry for the given index into the hrefs vector

      Self     : Splitter renames Splitter (This.Self.all);
      Previous : Natural;
      Next     : Natural;
      Page_Inx : Alpha_Index;

      ---------------
      -- Add_Entry --
      ---------------

      procedure Add_Entry (Index : in Natural) is
      begin
         if Index = 0 then
            --  This entry has no element
            Self.HREFS_V := Self.HREFS_V & Self.Default_Href;
         else
            Self.HREFS_V := Self.HREFS_V & URIs (Index);
         end if;
      end Add_Entry;

   begin
      if Size (Self.HREFS_V) = 0 then
         --  Not yet built
         Self.INDEXES_V := +"<>";
         Add_Entry (Self.Index (1));

         Self.INDEXES_V := Self.INDEXES_V & "0..9";
         Add_Entry (Self.Index (2));

         for C in Character range 'A' .. 'Z' loop
            Self.INDEXES_V := Self.INDEXES_V & C;
            Add_Entry (Self.Index (Alpha_Value (C)));
         end loop;
      end if;

      --  Compute page index

      for I in reverse Alpha_Index loop
         if Self.Index (I) /= 0 and then Self.Index (I) <= Page then
            Page_Inx := I;
            exit;
         end if;
      end loop;

      --  Compute Previous and Next, we need to find the previous/next non
      --  empty entry.

      Previous := 0;

      for P in reverse 1 .. Page_Inx - 1 loop
         if Self.Index (P) /= 0 then
            Previous := Self.Index (P);
            exit;
         end if;
      end loop;

      Next := 0;

      for P in Page_Inx + 1 .. Alpha_Index'Last loop
         if Self.Index (P) /= 0 then
            Next := Self.Index (P);
            exit;
         end if;
      end loop;

      return To_Set
        ((Assoc ("PREVIOUS",   Shared.Safe_URI (URIs, Previous)),
          Assoc ("NEXT",       Shared.Safe_URI (URIs, Next)),
          Assoc ("FIRST",      URIs (URIs'First)),
          Assoc ("LAST",       URIs (URIs'Last)),
          Assoc ("PAGE_INDEX", Positive (Page_Inx)),
          Assoc ("HREFS_V",    Self.HREFS_V),
          Assoc ("INDEXES_V",  Self.INDEXES_V)));
   end Get_Translations;

   ----------------------
   -- Set_Default_Href --
   ----------------------

   procedure Set_Default_Href
     (This : in out Splitter; Href : in String) is
   begin
      This.Default_Href := To_Unbounded_String (Href);
   end Set_Default_Href;

   -------------
   -- Set_Key --
   -------------

   procedure Set_Key (This : in out Splitter; Key : in String) is
   begin
      This.Key := To_Unbounded_String (Key);
   end Set_Key;

end AWS.Services.Split_Pages.Alpha;
