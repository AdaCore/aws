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

with Templates_Parser.Query;

with AWS.Services.Split_Pages.Shared;

package body AWS.Services.Split_Pages.Uniform is

   ---------------------
   -- Get_Page_Ranges --
   ---------------------

   function Get_Page_Ranges
     (This  : in Splitter;
      Table : in Templates.Translate_Set) return Ranges_Table
   is
      use Templates;
      use Templates.Query;

      procedure Max_Size (A : in Association; Quit : in out Boolean);
      --  Set Max with the maximum size between Max and the size of the vector

      Self : Splitter renames Splitter (This.Self.all);
      Max  : Natural := 0;

      --------------
      -- Max_Size --
      --------------

      procedure Max_Size (A : in Association; Quit : in out Boolean) is
         pragma Unreferenced (Quit);
      begin
         Max := Natural'Max (Max, Size (Composite (A)));
      end Max_Size;

      ----------------------
      -- Compute_Max_Size --
      ----------------------

      procedure Compute_Max_Size is new For_Every_Association (Max_Size);

   begin
      --  Clear possible remainings of a previous usage of This object

      Clear (Self.HREFS_V);
      Clear (Self.INDEXES_V);

      --  Get max size of vectors in Table

      Compute_Max_Size (Table);

      if Max <= Self.Max_Per_Page then
         return (1 => (First => 1, Last => Max));
      end if;

      declare
         Result : Ranges_Table
           (1 .. (Max + Self.Max_Per_Page - 1) / Self.Max_Per_Page);
      begin
         for I in 1 .. Result'Last - 1 loop
            Result (I) := (First => Self.Max_Per_Page *  (I - 1) + 1,
                           Last  => Self.Max_Per_Page * I);
         end loop;

         Result (Result'Last)
           := (First => Self.Max_Per_Page *  (Result'Last - 1) + 1,
               Last  => Max);

         return Result;
      end;
   end Get_Page_Ranges;

   ----------------------
   -- Get_Translations --
   ----------------------

   function Get_Translations
     (This   : in Splitter;
      Page   : in Positive;
      URIs   : in URI_Table;
      Ranges : in Ranges_Table) return Templates.Translate_Set
   is
      pragma Unreferenced (Ranges);
      use Templates_Parser;
      Self : Splitter renames Splitter (This.Self.all);
   begin
      if Size (Self.HREFS_V) = 0 then
         --  Not yet built

         for I in URIs'Range loop
            Self.HREFS_V   := Self.HREFS_V & URIs (I);
            Self.INDEXES_V := Self.INDEXES_V & I;
         end loop;
      end if;

      return To_Set
        ((Assoc ("PREVIOUS",   Shared.Safe_URI (URIs, Page - 1)),
          Assoc ("NEXT",       Shared.Safe_URI (URIs, Page + 1)),
          Assoc ("FIRST",      URIs (URIs'First)),
          Assoc ("LAST",       URIs (URIs'Last)),
          Assoc ("PAGE_INDEX", Page),
          Assoc ("HREFS_V",    Self.HREFS_V),
          Assoc ("INDEXES_V",  Self.INDEXES_V)));
   end Get_Translations;

end AWS.Services.Split_Pages.Uniform;
