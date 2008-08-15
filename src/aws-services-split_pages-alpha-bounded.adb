------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2004-2008, AdaCore                     --
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

package body AWS.Services.Split_Pages.Alpha.Bounded is

   ---------------------
   -- Get_Page_Ranges --
   ---------------------

   overriding function Get_Page_Ranges
     (This  : in Splitter;
      Table : in Templates.Translate_Set) return Ranges_Table
   is
      function Nb_Pages (The_Range : in Page_Range) return Natural;
      --  Returns the number of pages in the range

      Self        : Splitter renames Splitter (This.Self.all);
      Alpha_Table : constant Ranges_Table
        := Get_Page_Ranges (Alpha.Splitter (This), Table);
      New_Size    : Natural := 0;

      --------------
      -- Nb_Pages --
      --------------

      function Nb_Pages (The_Range : in Page_Range) return Natural is
      begin
         if The_Range.Last < The_Range.First then
            return 1;
         else
            return
              (The_Range.Last - The_Range.First + Self.Max_Per_Page)
              / Self.Max_Per_Page;
         end if;
      end Nb_Pages;

   begin
      for I in Alpha_Table'Range loop
         New_Size := New_Size + Nb_Pages (Alpha_Table (I));
      end loop;

      declare
         Result              : Ranges_Table (1 .. New_Size);
         Out_Inx             : Natural := 0;
         Line                : Positive;
         Corresponding_Alpha : Alpha_Index := 1;
      begin
         for In_Inx in Alpha_Table'Range loop

            while Self.Index (Corresponding_Alpha) = 0 loop
               Self.Index_Last (Corresponding_Alpha) := 0;
               Corresponding_Alpha := Corresponding_Alpha + 1;
            end loop;

            --  Here, Corresponding_Alpha is the alpha that corresponds
            --  to the entry in Alpha table that we are processing

            Self.Index (Corresponding_Alpha) := Out_Inx + 1;
            Line  := Alpha_Table (In_Inx).First;

            for I in 1 .. Nb_Pages (Alpha_Table (In_Inx)) - 1 loop
               Out_Inx := Out_Inx + 1;
               Result (Out_Inx)
                 := (First => Line, Last  => Line + Self.Max_Per_Page - 1);
               Line := Line + Self.Max_Per_Page;
            end loop;

            Out_Inx := Out_Inx + 1;
            Result (Out_Inx)
              := (First => Line, Last  => Alpha_Table (In_Inx).Last);

            Self.Index_Last (Corresponding_Alpha) := Out_Inx;

            if Corresponding_Alpha /= Alpha_Index'Last then
               Corresponding_Alpha := Corresponding_Alpha + 1;
            end if;
         end loop;

         return Result;
      end;
   end Get_Page_Ranges;

   ----------------------
   -- Get_Translations --
   ----------------------

   overriding function Get_Translations
     (This   : in Splitter;
      Page   : in Positive;
      URIs   : in URI_Table;
      Ranges : in Ranges_Table) return Templates.Translate_Set
   is
      use Templates;

      Self        : Splitter renames Splitter (This.Self.all);
      Result      : Templates.Translate_Set :=
                      Get_Translations
                        (Alpha.Splitter (This), Page, URIs, Ranges);
      S_INDEXES_V : Tag;
      S_HREFS_V   : Tag;
      A           : Alpha_Index := Alpha_Index'Last;
      Sub_Page    : Positive := Positive'Last;
   begin
      --  Find page index

      for I in Alpha_Index loop
         if Page <= Self.Index_Last (I) then
            A := I;
            Sub_Page := Page - Self.Index (I) + 1;
            exit;
         end if;
      end loop;

      for I in Self.Index (A) .. Self.Index_Last (A) loop
         S_INDEXES_V := S_INDEXES_V & Integer (I - Self.Index (A) + 1);
         S_HREFS_V   := S_HREFS_V   & URIs (I);
      end loop;

      if Page = Self.Index (A) then
         Insert (Result, Assoc ("S_PREVIOUS", ""));
      else
         Insert (Result, Assoc ("S_PREVIOUS", URIs (Page - 1)));
      end if;

      if Page = Self.Index_Last (A) then
         Insert (Result, Assoc ("S_NEXT", ""));
      else
         Insert (Result, Assoc ("S_NEXT", URIs (Page + 1)));
      end if;

      Insert (Result, Assoc ("S_FIRST", URIs (Positive (Self.Index (A)))));
      Insert
        (Result, Assoc ("S_LAST",  URIs (Positive (Self.Index_Last (A)))));
      Insert (Result, Assoc ("S_PAGE_INDEX", Sub_Page));
      Insert (Result, Assoc ("S_INDEXES_V", S_INDEXES_V));
      Insert (Result, Assoc ("S_HREFS_V",   S_HREFS_V));
      return Result;
   end Get_Translations;

end AWS.Services.Split_Pages.Alpha.Bounded;
