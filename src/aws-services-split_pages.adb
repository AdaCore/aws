------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                            Copyright (C) 2003                            --
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

with Templates_Parser.Query;

with AWS.Config;
with AWS.MIME;
with AWS.Resources.Streams.Memory;
with AWS.Services.Transient_Pages;
with AWS.Translator;

package body AWS.Services.Split_Pages is

   use Ada.Strings.Unbounded;

   -----------
   -- Parse --
   -----------

   function Parse
     (Template     : in String;
      Translations : in Templates.Translate_Table;
      Table        : in Templates.Translate_Table;
      Max_Per_Page : in Positive := 25;
      Max_In_Index : in Positive := 20;
      Cached       : in Boolean  := True)
      return Response.Data
   is
      package T_Query renames Templates_Parser.Query;

      function Previous (P : in Positive) return String;
      --  Returns URI for previous page (Page index P - 1)

      function Current  (P : in Positive) return String;
      --  Returns URI for current page (Page index P)

      function Next     (P : in Positive) return String;
      --  Returns URI for next page (Page index P + 1)

      function Indexes  (P : in Positive) return Templates.Translate_Table;
      --  Returns the set of indexes for all the pages as a Translate_Table.
      --  This tables contains 2 Vector_Tag, one with the page numbers
      --  named INDEXES_V and one with the href named HREFS_V.

      function Compute_Max_Items return Natural;
      --  Returns the maximum number of items on the table to split

      -----------------------
      -- Compute_Max_Items --
      -----------------------

      function Compute_Max_Items return Natural is
         Max : Natural := 0;
      begin
         for A in Table'Range loop
            Max := Natural'Max
              (Max, Templates.Size (T_Query.Composite (Table (A))));
         end loop;

         return Max;
      end Compute_Max_Items;

      Max         : constant Natural := Compute_Max_Items;
      Nb_Pages    : constant Natural := (Max - 1) / Max_Per_Page + 1;

      Split_Table : Templates.Translate_Table (Table'Range);
      Last        : Positive := 1;
      Result      : Unbounded_String;

      URIs        : array (1 .. Nb_Pages) of Unbounded_String;

      -------------
      -- Current --
      -------------

      function Current  (P : in Positive) return String is
      begin
         return To_String (URIs (P));
      end Current;

      -------------
      -- Indexes --
      -------------

      function Indexes (P : in Positive) return Templates.Translate_Table is
         use type Templates.Vector_Tag;

         V  : Templates.Vector_Tag;  --  Set of URIs
         VI : Templates.Vector_Tag;  --  Set of Indexes

      begin
         if Nb_Pages <= Max_In_Index then
            for K in URIs'Range loop
               VI := VI & K;
               V  := V  & To_String (URIs (K));
            end loop;

         else
            --  Create a sliced index set around the current page

            declare
               Cont    : constant := 0;
               --  Value for a continuation "..."

               Middle  : constant Natural := (Max_In_Index - 4) / 2;

               Indexes : array (1 .. Max_In_Index) of Natural;
               I       : Integer;
               First   : Natural;
               Last    : Natural;
            begin
               --  The first item is always present

               Indexes (Indexes'First) := URIs'First;

               --  Should we have a cont symbol on the left

               I := P - Middle;

               if I <= 2 then
                  --  There is not enough items on the left
                  First := Indexes'First + 1;
                  I := 2;

               else
                  --  Add a continuation item
                  Indexes (Indexes'First + 1) := Cont;
                  First := Indexes'First + 2;
               end if;

               --  Fill the table

               for K in First .. Indexes'Last loop
                  Indexes (K) := I;
                  I := I + 1;
               end loop;

               Last := Indexes'Last;

               if Indexes (Last) > Nb_Pages then
                  --  Too many items, skip them

                  while Indexes (Last) > Nb_Pages loop
                     Last := Last - 1;
                  end loop;

               else
                  --  Not enough space, add continuation item
                  Indexes (Last)     := Nb_Pages;
                  Indexes (Last - 1) := 0;
               end if;

               --  Create vector of indexes

               for K in Indexes'First .. Last loop
                  if Indexes (K) = 0 then
                     VI := VI & "...";
                     V  := V  & "...";
                  else
                     VI := VI & Indexes (K);
                     V  := V  & To_String (URIs (Indexes (K)));
                  end if;
               end loop;
            end;
         end if;

         return (Templates.Assoc ("HREFS_V", V),
                 Templates.Assoc ("INDEXES_V", VI));
      end Indexes;

      ----------
      -- Next --
      ----------

      function Next     (P : in Positive) return String is
      begin
         if P < URIs'Last then
            return To_String (URIs (P + 1));
         else
            return "";
         end if;
      end Next;

      --------------
      -- Previous --
      --------------

      function Previous (P : in Positive) return String is
      begin
         if P > 1 then
            return To_String (URIs (P - 1));
         else
            return "";
         end if;
      end Previous;

   begin
      --  Create the set of temporary URIs needed for the pages

      for K in URIs'Range loop
         URIs (K) := To_Unbounded_String (Services.Transient_Pages.Get_URI);
      end loop;

      --  Create each page

      for I in 1 .. Nb_Pages loop

         --  Create the Split_Table containing part of the items

         for A in Table'Range loop

            case T_Query.Kind (Table (A)) is

               when Templates.Std =>
                  --  Nothing to be done, copy this association as-is

                  Split_Table (A) := Table (A);

               when Templates.Composite =>
                  --  Copy Max_Per_Page items starting from Last

                  declare
                     use type Templates.Tag;

                     T      : Templates.Tag
                                renames T_Query.Composite (Table (A));
                     Size   : constant Natural := Templates.Size (T);
                     Nested : constant Natural := T_Query.Nested_Level (T);
                     V      : Templates.Tag;
                  begin
                     for K in
                       Last .. Natural'Min (Last + Max_Per_Page - 1, Size)
                     loop
                        if Nested > 1 then
                           V := V & Templates.Tag'(Templates.Item (T, K));
                        else
                           V := V & String'(Templates.Item (T, K));
                        end if;
                     end loop;

                     Split_Table (A) := Templates.Assoc
                       (T_Query.Variable (Table (A)), V);
                  end;
            end case;
         end loop;

         --  Generate all the pages, add them to the transient pages handler

         declare
            use Templates;

            Table  : constant Templates.Translate_Table
              := Translations & Split_Table
                   & Templates.Assoc ("PREVIOUS", Previous (I))
                   & Templates.Assoc ("NEXT", Next (I))
                   & Templates.Assoc ("PAGE_INDEX", I)
                   & Templates.Assoc ("NUMBER_PAGES", Nb_Pages)
                   & Templates.Assoc ("OFFSET", Last - 1)
                   & Indexes (I);

            Stream : AWS.Resources.Streams.Stream_Access;
         begin
            Stream := new AWS.Resources.Streams.Memory.Stream_Type;

            declare
               Page : constant Unbounded_String
                 := Templates.Parse (Template, Table, Cached);
            begin
               AWS.Resources.Streams.Memory.Append
                 (AWS.Resources.Streams.Memory.Stream_Type (Stream.all),
                  Translator.To_Stream_Element_Array (To_String (Page)));

               if Result = Null_Unbounded_String then
                  Result := Page;
               end if;
            end;

            Services.Transient_Pages.Register
              (Current (I), Stream, Config.Transient_Lifetime);
         end;

         Last := Last + Max_Per_Page;
      end loop;

      return Response.Build (MIME.Text_HTML, Result);
   end Parse;

end AWS.Services.Split_Pages;
