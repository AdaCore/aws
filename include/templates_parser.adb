------------------------------------------------------------------------------
--                             Templates Parser                             --
--                                                                          --
--                            Copyright (C) 1999                            --
--                               Pascal Obry                                --
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

with Ada.Exceptions;
with Ada.Characters.Handling;
with Ada.Text_IO;
with Ada.Strings.Fixed;
with Ada.Strings.Maps.Constants;
with Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;
with Strings_Cutter;

package body Templates_Parser is

   use Ada;
   use Ada.Strings;

   Max_Section     : constant := 10;

   subtype Buffer_Type is String (1 .. 1_024);

   Table_Token              : constant String := "@@TABLE@@";
   Terminate_Sections_Token : constant String := "@@TERMINATE_SECTIONS@@";
   Section_Token            : constant String := "@@SECTION@@";
   End_Table_Token          : constant String := "@@END_TABLE@@";
   If_Token                 : constant String := "@@IF@@";
   Else_Token               : constant String := "@@ELSE@@";
   End_If_Token             : constant String := "@@END_IF@@";
   Include_Token            : constant String := "@@INCLUDE@@";

   Filter_Identity_Token    : aliased constant String := "@@IDENTITY";
   Filter_Reverse_Token     : aliased constant String := "@@REVERSE";
   Filter_Lower_Token       : aliased constant String := "@@LOWER";
   Filter_Upper_Token       : aliased constant String := "@@UPPER";
   Filter_Clean_Text_Token  : aliased constant String := "@@CLEAN_TEXT";
   Filter_Capitalize_Token  : aliased constant String := "@@CAPITALIZE";

   subtype Table_Range     is Positive range Table_Token'Range;
   subtype Section_Range   is Positive range Section_Token'Range;
   subtype End_Table_Range is Positive range End_Table_Token'Range;
   subtype If_Range        is Positive range If_Token'Range;
   subtype Else_Range      is Positive range Else_Token'Range;
   subtype End_If_Range    is Positive range End_If_Token'Range;
   subtype Include_Range   is Positive range Include_Token'Range;

   --  Filters setting
   --
   --  a filter appear just before a tag variable (e.g. @@LOWER@@_SOME_VAT_@@
   --  and means that the filter LOWER should be applied to SOME_VAR before
   --  replacing it in the template file.

   type Filters_Mode is
     (Identity,   -- same string as the input
      Invert,     -- reverse string
      Lower,      -- lower case
      Upper,      -- upper case
      Capitalize, -- lower case except char before spaces and underscores
      Clean_Text  -- only letter/digits all other chars are changed to spaces.
      );

   type Filter_Function is access function (S : in String) return String;

   type String_Access is access constant String;

   type Filter_Record is record
      Name   : String_Access;
      Handle : Filter_Function;
   end record;

   --  filter functions, see above.

   function Lower_Filter (S : in String) return String;
   function Identity_Filter (S : in String) return String;
   function Reverse_Filter (S : in String) return String;
   function Upper_Filter (S : in String) return String;
   function Capitalize_Filter (S : in String) return String;
   function Clean_Text_Filter (S : in String) return String;

   function Check_Filter (Str : in Unbounded_String;
                          P   : in Positive)
                         return Filters_Mode;
   --  returns the prefix filter for the tag variable starting a position P in
   --  Str or Identity if no filter has been found.

   function Through_Filter (S      : in String;
                            Filter : in Filters_Mode) return String;
   --  apply Filter to string S and return the result string.

   function Filter_Length (Filter : in Filters_Mode) return Natural;
   --  returns the number of characters for the filter token Filter.

   ---------------------
   -- Identity_Filter --
   ---------------------

   function Identity_Filter (S : in String) return String is
   begin
      return S;
   end Identity_Filter;

   ------------------
   -- Lower_Filter --
   ------------------

   function Lower_Filter (S : in String) return String is
   begin
      return Characters.Handling.To_Lower (S);
   end Lower_Filter;

   --------------------
   -- Reverse_Filter --
   --------------------

   function Reverse_Filter (S : in String) return String is
      Result : String (S'Range);
   begin
      for K in S'Range loop
         Result (Result'Last - K + Result'First) := S (K);
      end loop;
      return Result;
   end Reverse_Filter;

   ------------------
   -- Upper_Filter --
   ------------------

   function Upper_Filter (S : in String) return String is
   begin
      return Characters.Handling.To_Upper (S);
   end Upper_Filter;

   -----------------------
   -- Capitalize_Filter --
   -----------------------

   function Capitalize_Filter (S : in String) return String is
      Result : String (S'Range);
      Upper  : Boolean := True;
   begin
      for K in Result'Range loop
         if Upper then
            Result (K) := Characters.Handling.To_Upper (S (K));
            Upper := False;
         else
            Result (K) := Characters.Handling.To_Lower (S (K));
            if Result (K) = ' ' or else Result (K) = '_' then
               Upper := True;
            end if;
         end if;
      end loop;
      return Result;
   end Capitalize_Filter;

   -----------------------
   -- Clean_Text_Filter --
   -----------------------

   function Clean_Text_Filter (S : in String) return String is

      use type Strings.Maps.Character_Set;

      Result : String (S'Range);

      Clean_Set : Strings.Maps.Character_Set
        := Strings.Maps.Constants.Letter_Set
        or Strings.Maps.Constants.Decimal_Digit_Set
        or Strings.Maps.To_Set (" йикопафз");

   begin
      for K in S'Range loop
         if Strings.Maps.Is_In (S (K), Clean_Set) then
            Result (K) := S (K);
         else
            Result (K) := ' ';
         end if;
      end loop;
      return Result;
   end Clean_Text_Filter;

   Filter_Table : array (Filters_Mode) of Filter_Record
     := (Identity   =>
           (Filter_Identity_Token'Access,   Identity_Filter'Access),
         Lower      =>
           (Filter_Lower_Token'Access,      Lower_Filter'Access),
         Upper      =>
           (Filter_Upper_Token'Access,      Upper_Filter'Access),
         Capitalize =>
           (Filter_Capitalize_Token'Access, Capitalize_Filter'Access),
         Clean_Text =>
           (Filter_Clean_Text_Token'Access, Clean_Text_Filter'Access),
         Invert     =>
           (Filter_Reverse_Token'Access,    Reverse_Filter'Access));

   ------------------
   -- Check_Filter --
   ------------------

   function Check_Filter (Str : in Unbounded_String;
                          P : in Positive)
                         return Filters_Mode is
   begin
      for F in Filter_Table'Range loop
         if P - Filter_Table (F).Name'Length >= 1
           and then Slice (Str,
                           P - Filter_Table (F).Name'Length,
                           P - 1) = Filter_Table (F).Name.all
         then
            return F;
         end if;
      end loop;
      return Identity;
   end Check_Filter;

   --------------------
   -- Through_Filter --
   --------------------

   function Through_Filter (S      : in String;
                            Filter : in Filters_Mode) return String is
   begin
      return Filter_Table (Filter).Handle (S);
   end Through_Filter;

   -------------------
   -- Filter_Length --
   -------------------

   function Filter_Length (Filter : in Filters_Mode) return Natural is
   begin
      if Filter = Identity then
         return 0;
      else
         return Filter_Table (Filter).Name'Length;
      end if;
   end Filter_Length;

   -----------
   -- Assoc --
   -----------

   function Assoc (Variable  : in String;
                   Value     : in String;
                   Is_Vector : in Boolean   := False;
                   Begin_Tag : in String    := Default_Begin_Tag;
                   End_Tag   : in String    := Default_End_Tag;
                   Separator : in Character := Default_Separator)
                  return Association is
   begin
      return Association'
        (To_Unbounded_String (Begin_Tag & Variable & End_Tag),
         To_Unbounded_String (Value),
         Separator,
         Is_Vector);
   end Assoc;

   -----------
   -- Assoc --
   -----------

   function Assoc (Variable  : in String;
                   Value     : in Boolean;
                   Begin_Tag : in String    := Default_Begin_Tag;
                   End_Tag   : in String    := Default_End_Tag)
                   return Association is
   begin
      if Value then
         return Assoc (Variable, "TRUE", False, Begin_Tag, End_Tag);
      else
         return Assoc (Variable, "FALSE", False, Begin_Tag, End_Tag);
      end if;
   end Assoc;

   ---------------
   -- Translate --
   ---------------

   procedure Translate (Str : in out Unbounded_String;
                        Tag : in     String;
                        To  : in     String);
   --  Translate all tags named Tag in Str by To

   procedure Translate (Str : in out Unbounded_String;
                        Tag : in     String;
                        To  : in     String) is
      Pos    : Natural;
      Filter : Filters_Mode;
   begin
      loop
         Pos := Index (Str, Tag);

         exit when Pos = 0;

         --  check if there is a prefix filter to apply

         Filter := Check_Filter (Str, Pos);

         if Pos /= 0 then
            Replace_Slice (Str,
                           Pos - Filter_Length (Filter),
                           Pos + Tag'Length - 1,
                           Through_Filter (To, Filter));
         end if;
      end loop;
   end Translate;


   -----------
   -- Parse --
   -----------

   function Parse (Template     : in Template_File;
                   Translations : in Translate_Table := No_Translation)
                  return String is

      Template_Filename : constant String := To_String (Template.Filename);

      File         : Text_IO.File_Type;
      Current_Line : Natural := 0;
      Table_Level  : Natural := 0;
      If_Level     : Natural := 0;

      Max_Lines    : Natural := 0;
      --  maximum number if lines in the table. This variable is always set to
      --  0 outside of a table.

      procedure Fatal_Error (Message : in String);
      --  raise Template_Error exception with message.

      function Exist (Translations : in Translate_Table;
                      Tag          : in String;
                      Value        : in String)
                     return Boolean;
      --  check that Tag exist in the translation table and that its value is
      --  set to Value.

      procedure Translate (Str : in out Unbounded_String);
      --  Translate all tags in the translations table and the specials tags
      --  in Str by their corresponding value.

      procedure Translate (Str  : in out Unbounded_String;
                           N    : in     Positive;
                           Stop :    out Boolean);
      --  Translate all tags in Str with the Nth Tag's value. This procedure
      --  is used to build the tables. Tags used in a table are a set of
      --  values separated by a special character.

      -----------------
      -- Fatal_Error --
      -----------------

      procedure Fatal_Error (Message : in String) is
      begin
         Exceptions.Raise_Exception
           (Template_Error'Identity,
            "In " & Template_Filename
            & " at line" & Natural'Image (Current_Line) & ' ' & Message);
      end Fatal_Error;

      -----------
      -- Exist --
      -----------

      function Exist (Translations : in Translate_Table;
                      Tag          : in String;
                      Value        : in String)
        return Boolean is
      begin
         for K in Translations'Range loop
            if Translations (K).Variable = Tag
              and then Translations (K).Value = Value then
               return True;
            end if;
         end loop;
         return False;
      end Exist;

      ---------------
      -- Translate --
      ---------------

      procedure Translate (Str : in out Unbounded_String) is
      begin
         for K in Translations'Range loop

            Translate (Str,
                       To_String (Translations (K).Variable),
                       To_String (Translations (K).Value));
         end loop;

         Translate (Str,
                    Tag => "@@_NUMBER_LINE_@@",
                    To  => Fixed.Trim (Positive'Image (Max_Lines),
                                       Strings.Both));

         Translate (Str,
                    Tag => "@@_TABLE_LEVEL_@@",
                    To  => Fixed.Trim (Positive'Image (Table_Level),
                                       Strings.Both));
      end Translate;

      procedure Translate (Str  : in out Unbounded_String;
                           N    : in     Positive;
                           Stop :    out Boolean)
      is
         use Strings_Cutter;

         Pos     : Natural;
         CS      : Cutted_String;
         Nb_Item : Natural;

         Filter  : Filters_Mode;

      begin
         Stop := False;

         --  for every entry in the translation table

         for K in Translations'Range loop

            loop
               Pos := Index
                 (Str,
                  To_String (Translations (K).Variable));

               exit when Pos = 0;

               --  check if there is a prefix filter to apply

               Filter := Check_Filter (Str, Pos);

               Create (CS,
                       To_String (Translations (K).Value),
                       String'(1 => Translations (K).Separator));

               Nb_Item := Field_Count (CS);

               --  we stop when we reach (or are over) the maximum number of
               --  fields

               Stop := Stop
                 or else ((Nb_Item = 0 or else N >= Nb_Item)
                          and then Translations (K).Vector);

               --  if there is no value for the tag or we ask for a value
               --  that does not exist, just replace it with an
               --  emptry string (i.e. removing it from the template).

               if Nb_Item = 0
                 or else (Nb_Item < N and then Translations (K).Vector)
               then
                  Replace_Slice
                    (Str,
                     Pos,
                     Pos + Length (Translations (K).Variable) - 1,
                     "");
               else
                  if Translations (K).Vector then
                     Replace_Slice
                       (Str,
                        Pos - Filter_Length (Filter),
                        Pos + Length (Translations (K).Variable) - 1,
                        Through_Filter (Field (CS, N), Filter));
                  else
                     Replace_Slice
                       (Str,
                        Pos - Filter_Length (Filter),
                        Pos + Length (Translations (K).Variable) - 1,
                        Through_Filter (Field (CS, 1), Filter));
                  end if;
               end if;

               Destroy (CS);
            end loop;

         end loop;

         --  translate special tags

         Translate (Str,
                    Tag => "@@_TABLE_LINE_@@",
                    To  => Fixed.Trim (Positive'Image (N), Strings.Both));

         Translate (Str,
                    Tag => "@@_NUMBER_LINE_@@",
                    To  => Fixed.Trim (Positive'Image (Max_Lines),
                                       Strings.Both));

         Translate (Str,
                    Tag => "@@_TABLE_LEVEL_@@",
                    To  => Fixed.Trim (Positive'Image (Table_Level),
                                       Strings.Both));

      end Translate;

      --  variables used by Parse

      Buffer        : Buffer_Type;
      Last          : Natural;
      Result        : Unbounded_String;
      Trimed_Buffer : Buffer_Type;

      -------------------
      -- Get_Next_Line --
      -------------------

      procedure Get_Next_Line (Buffer, Trimed_Buffer : out String;
                               Last                  : out Natural);
      pragma Inline (Get_Next_Line);

      procedure Get_Next_Line (Buffer, Trimed_Buffer : out String;
                               Last                  : out Natural) is
      begin
         Current_Line := Current_Line + 1;

         declare
            Line : constant String
              := To_String (Template.Lines (Current_Line));
         begin
            Buffer (Line'Range) := Line;
            Last := Line'Last;
         end;

         Fixed.Move (Fixed.Trim (Buffer (1 .. Last), Strings.Left),
                     Trimed_Buffer);
      end Get_Next_Line;

      -----------
      -- Parse --
      -----------

      function Parse (Line : in String) return String is

         Str : Unbounded_String;


         function Parse_If (Condition       : in String  := "";
                            Check_Condition : in Boolean := True)
           return String;
         --  Parse an if statement (from If_Token to End_If_Token). Condition
         --  is the if conditional part. It will be checked only if
         --  Check_Condition is True.  At the time of this call If_Token as
         --  been read. When returning the file index is positioned on the
         --  line just after the End_If_Token is ready.

         function Parse_Table (Terminate_Sections : Boolean) return String;
         --  Parse a table statement (from Table_Token to End_Table_Token). At
         --  the time of this call Table_Token as been read. When returning
         --  the file index is positioned on the line just after the
         --  End_Table_Token line.

         function Parse_Include (Filename : in String) return String;
         --  Parse an include statement (Include_Token).  At the time of this
         --  call Include_Token as been read. When returning the file index is
         --  positioned on the line just after the Include_Token line.

         -----------------
         -- Parse_Table --
         -----------------

         function Parse_Table (Terminate_Sections : Boolean) return String is

            type Sections is array (Positive range <>) of Unbounded_String;

            Buffer        : Buffer_Type;
            Trimed_Buffer : Buffer_Type;
            Last          : Natural;
            Lines         : Sections (1 .. Max_Section);
            Str           : Unbounded_String;
            Result        : Unbounded_String;
            Section       : Positive := 1;

            function Count_Lines (Lines : Sections) return Natural is
               use Strings_Cutter;
               Max_Values : Natural := 0;
               CS         : Cutted_String;
            begin
               for S in Lines'Range loop
                  for T in Translations'Range loop
                     if Translations (T).Vector
                       and then
                       Index (Lines (S),
                              To_String (Translations (T).Variable)) /= 0
                     then
                        Create (CS,
                                To_String (Translations (T).Value),
                                String'(1 => Translations (T).Separator));
                        Max_Values := Natural'Max (Max_Values,
                                                   Field_Count (CS));
                        Destroy (CS);
                     end if;
                  end loop;
               end loop;
               return Max_Values;
            end Count_Lines;

         begin

            Table_Level := Table_Level + 1;

            loop
               Get_Next_Line (Buffer, Trimed_Buffer, Last);

               if Last = 0 then
                  --  this is an empty line
                  Lines (Section) := Lines (Section) & ASCII.LF;
               else
                  if Trimed_Buffer (If_Range) = If_Token then
                     Lines (Section) := Lines (Section) & Parse_If
                       (Fixed.Trim
                        (Trimed_Buffer (If_Range'Last + 2 .. Last), Both));

                  elsif Trimed_Buffer (Table_Range) = Table_Token then
                     Lines (Section) := Lines (Section)
                       & Parse_Table
                       (Fixed.Index
                        (Trimed_Buffer, Terminate_Sections_Token) /= 0);

                  elsif Trimed_Buffer (Include_Range) = Include_Token then
                     Lines (Section) := Lines (Section) & Parse_Include
                       (Fixed.Trim
                        (Trimed_Buffer (Include_Range'Last + 2 .. Last),
                         Both));

                  elsif Trimed_Buffer (End_Table_Range) = End_Table_Token then
                     declare
                        K    : Positive := 1;
                        Stop : Boolean;
                        SN   : Positive range 1 .. Section; --  section number
                     begin

                        Max_Lines := Count_Lines (Lines (1 .. Section));

                        if Max_Lines > 0 then

                           loop
                              SN := (K - 1) mod Section + 1;
                              Str := Lines (SN);
                              Translate (Str, K, Stop);

                              Result := Result & To_String (Str);

                              exit when Stop
                                and then (not Terminate_Sections
                                          or else SN = Section);

                              K := K + 1;
                           end loop;

                        end if;

                        Max_Lines := 0;

                        exit;
                     end;

                  elsif Trimed_Buffer (Section_Range) = Section_Token then

                     Section := Section + 1;

                  elsif Trimed_Buffer (End_If_Range) = End_If_Token then
                     Fatal_Error (End_If_Token & " found, "
                                  & End_Table_Token & " expected.");

                  else
                     Lines (Section) := Lines (Section)
                       & Buffer (1 .. Last) & ASCII.LF;
                  end if;
               end if;
            end loop;

            Table_Level := Table_Level - 1;
            return To_String (Result);

         exception
            when Text_IO.End_Error =>
               Fatal_Error ("found end of file, expected " & End_Table_Token);
               return "";
         end Parse_Table;

         --------------
         -- Parse_If --
         --------------

         function Parse_If (Condition       : in String  := "";
                            Check_Condition : in Boolean := True)
           return String
         is
            Buffer        : Buffer_Type;
            Trimed_Buffer : Buffer_Type;
            Last          : Natural := 0;
            Lines         : Unbounded_String;
         begin
            --  if no need to check condition or condition is true

            if Check_Condition = False
              or else Exist (Translations, Condition, "TRUE")
            then

               loop

                  Get_Next_Line (Buffer, Trimed_Buffer, Last);

                  if Last = 0 then
                     Lines := Lines & ASCII.LF;
                  else
                     if Trimed_Buffer (Table_Range) = Table_Token then
                        Lines := Lines
                          & Parse_Table
                          (Fixed.Index
                           (Trimed_Buffer, Terminate_Sections_Token) /= 0);

                     elsif Trimed_Buffer (If_Range) = If_Token then
                        Lines := Lines & Parse_If
                          (Fixed.Trim
                           (Trimed_Buffer (If_Range'Last + 2 .. Last),
                            Both));

                     elsif Trimed_Buffer (Include_Range) = Include_Token then
                        Lines := Lines & Parse_Include
                          (Fixed.Trim
                           (Trimed_Buffer (Include_Range'Last + 2 .. Last),
                            Both));

                     elsif Trimed_Buffer (End_If_Range) = End_If_Token then
                        exit;

                     elsif Trimed_Buffer (Else_Range) = Else_Token then

                        --  skip to End_If_Token at the same level
                        If_Level := 0;

                        loop
                           Get_Next_Line (Buffer, Trimed_Buffer, Last);

                           if Trimed_Buffer (If_Range) = If_Token then
                              If_Level := If_Level + 1;
                           end if;

                           if Trimed_Buffer (End_If_Range) = End_If_Token then

                              if If_Level = 0 then
                                 exit;
                              else
                                 If_Level := If_Level - 1;
                              end if;
                           end if;
                        end loop;

                        exit;

                     elsif
                       Trimed_Buffer (End_Table_Range) = End_Table_Token
                     then
                        Fatal_Error (End_Table_Token & " found, "
                                     & End_If_Token & " expected.");

                     else
                        Lines := Lines & Buffer (1 .. Last) & ASCII.LF;

                     end if;
                  end if;
               end loop;

               --  We want to translate if's body only if it is not inside a
               --  table. In this case we return the if's body untranslated.
               --  The translation will take place in the table using the
               --  table translation mechanism (i.e. using tag's values set).

               if Table_Level = 0 then
                  Translate (Lines);
               end if;

               return To_String (Lines);

            else

               --  condition is false, we skip lines until End_If_Token (at
               --  the same level) or if we found a Else_Token part we parse
               --  it.

               If_Level := 0;

               loop
                  Get_Next_Line (Buffer, Trimed_Buffer, Last);

                  if Trimed_Buffer (If_Range) = If_Token then
                     If_Level := If_Level + 1;
                  end if;

                  if If_Level = 0 then

                     if Trimed_Buffer (Else_Range) = Else_Token then

                        --  else part found, parse else-block as a main-if
                        --  block without checking the condition.

                        return Parse_If (Check_Condition => False);

                     elsif Trimed_Buffer (End_Table_Range)
                       = End_Table_Token
                     then
                        Fatal_Error (End_Table_Token & " found, "
                                     & End_If_Token & " expected.");
                     end if;

                  end if;

                  if Trimed_Buffer (End_If_Range) = End_If_Token then

                     if If_Level = 0 then
                        exit;
                     else
                        If_Level := If_Level - 1;
                     end if;
                  end if;
               end loop;

               return "";

            end if;
         exception
            when Text_IO.End_Error =>
               Fatal_Error ("found end of file, expected " & End_If_Token);
               return "";
         end Parse_If;

         -------------------
         -- Parse_Include --
         -------------------

         function Parse_Include (Filename : in String) return String is

            --  returns last directory separator in Filename.
            --  0 means that there is not directory separator in Filename.

            function Dir_Sep_Index (Filename : in String) return Natural is
               use Ada.Strings;
            begin
               return Fixed.Index (Filename,
                                   Set   => Maps.To_Set ("/\"),
                                   Going => Strings.Backward);
            end Dir_Sep_Index;

            K      : Natural;

         begin
            --  if the file to be included is specified with a path or if
            --  there is no path specified with the current template then
            --  we will open the included template as specified.

            K := Dir_Sep_Index (Template_Filename);

            if Dir_Sep_Index (Filename) /= 0 or else K = 0 then
               return Parse (Filename, Translations);
            else

               --  concat current template file pathname with the included
               --  filename. it means that we want to open the new template
               --  filename in the directory were the current template is.

               return Parse (Template_Filename (1 .. K) & Filename,
                             Translations);
            end if;

         exception
            when others =>
               Fatal_Error ("include file " & Filename & " not found.");
               return "";
         end Parse_Include;

      begin
         if Line'Length /= 0 then

            if Trimed_Buffer (Table_Range) = Table_Token then

               return Parse_Table
                 (Fixed.Index (Trimed_Buffer, Terminate_Sections_Token) /= 0);

            elsif Trimed_Buffer (If_Range) = If_Token then

               return Parse_If
                 (Fixed.Trim
                  (Trimed_Buffer (If_Range'Last + 2 .. Last), Both));

            elsif Trimed_Buffer (Include_Range) = Include_Token then

               return Parse_Include
                 (Fixed.Trim
                  (Trimed_Buffer (Include_Range'Last + 2 .. Last), Both));

            else
               Str := To_Unbounded_String (Buffer (1 .. Last));
               Translate (Str);
               return To_String (Str) & ASCII.LF;
            end if;

         else
            --  empty line

            return String'(1 => ASCII.LF);

         end if;
      end Parse;

   begin
      while Current_Line < Template.Lines'Last loop

         Get_Next_Line (Buffer, Trimed_Buffer, Last);

         Result := Result & Parse (Line => Buffer (1 .. Last));

      end loop;

      return To_String (Result);
   exception
      when Template_Error =>
         raise;

      when others =>
         raise Template_Error;
   end Parse;

   -----------
   -- Parse --
   -----------

   function Parse (Template_Filename : in String;
                   Translations      : in Translate_Table := No_Translation)
                   return String is
   begin
      return Parse (Open (Template_Filename), Translations);
   end Parse;

   ---------------
   -- Translate --
   ---------------

   function Translate (Template : in String;
                       Translations : in Translate_Table := No_Translation)
                      return String
   is
      New_Template : Unbounded_String := To_Unbounded_String (Template);
   begin
      for T in Translations'Range loop
         Translate (New_Template,
                    To_String (Translations (T).Variable),
                    To_String (Translations (T).Value));
      end loop;
      return To_String (New_Template);
   end Translate;

   ----------
   -- Open --
   ----------

   function Open (Template_Filename : in String)
                 return Template_File
   is
      File     : Text_IO.File_Type;
      Lines    : Template_Content (1 .. Max_Template_Lines);
      Buffer   : Buffer_Type;
      Last     : Natural;
      K        : Natural := 0;
      Template : Template_File;
   begin
      begin
         Text_IO.Open (File => File,
                       Name => Template_Filename,
                       Mode => Text_IO.In_File);
      exception
         when Text_IO.Name_Error =>
            Exceptions.Raise_Exception
              (Template_Error'Identity,
               "template file " & Template_Filename & " not found");
      end;

      while not Text_IO.End_Of_File (File) loop

         Text_IO.Get_Line (File, Buffer, Last);

         K := K + 1;
         Lines (K) := To_Unbounded_String (Buffer (1 .. Last));

      end loop;

      Text_IO.Close (File);

      Template.Filename := To_Unbounded_String (Template_Filename);
      Template.Lines    := new Template_Content'(Lines (1 .. K));

      return Template;

   exception
      when Template_Error =>
         raise;

      when others =>
         raise Template_Error;
   end Open;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Template : in out Template_File) is
   begin
      Template.Count := new Natural'(1);
   end Initialize;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Template : in out Template_File) is

      procedure Free is
         new Unchecked_Deallocation (Template_Content, Template_Lines);

      procedure Free is
         new Unchecked_Deallocation (Natural, Counter);

   begin
      Template.Count.all := Template.Count.all - 1;

      if Template.Count.all = 0 then
         Free (Template.Lines);
         Free (Template.Count);
      end if;
   end Finalize;

   ------------
   -- Assign --
   ------------

   procedure Adjust (Template : in out Template_File) is
   begin
      Template.Count.all := Template.Count.all + 1;
   end Adjust;

end Templates_Parser;
