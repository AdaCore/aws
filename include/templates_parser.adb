------------------------------------------------------------------------------
--                             Templates Parser                             --
--                                                                          --
--                        Copyright (C) 1999 - 2001                         --
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

package body Templates_Parser is

   use Ada;
   use Ada.Exceptions;
   use Ada.Strings;

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
   Filter_Yes_No_Token      : aliased constant String := "@@YES_NO";
   Filter_Oui_Non_Token     : aliased constant String := "@@OUI_NON";

   subtype Table_Range     is Positive range Table_Token'Range;
   subtype Section_Range   is Positive range Section_Token'Range;
   subtype End_Table_Range is Positive range End_Table_Token'Range;
   subtype If_Range        is Positive range If_Token'Range;
   subtype Else_Range      is Positive range Else_Token'Range;
   subtype End_If_Range    is Positive range End_If_Token'Range;
   subtype Include_Range   is Positive range Include_Token'Range;

   --------------------------------
   --  Template Tree definitions --
   --------------------------------

   type Nkind is (Data, If_Stmt, Table_Stmt, Section_Stmt, Include_Stmt);

   type Node;
   type Tree is access Node;

   type Node (Kind : Nkind) is record
      Next : Tree;

      case Kind is
         when Data =>
            Value : Unbounded_String;

         when If_Stmt =>
            Cond    : Unbounded_String;
            N_True  : Tree;
            N_False : Tree;

         when Table_Stmt =>
            Terminate_Sections : Boolean;
            Sections           : Tree;

         when Section_Stmt =>
            N_Section : Tree;

         when Include_Stmt =>
            File     : Tree;
            Filename : Unbounded_String;
      end case;
   end record;

   -------------------
   --  Cached Files --
   -------------------

   --  Cached_Files keep the Tree for a given file.

   package Cached_Files is

      procedure Add
        (Filename : in String;
         T        : in Tree);
      --  Add Filename/T to the list of cached files.

      function Get (Filename : in String) return Tree;
      --  Returns the Tree for Filename or null if Filename has not been
      --  cached.

   end Cached_Files;

   ----------------------
   --  Filters setting --
   ----------------------

   --  a filter appear just before a tag variable (e.g. @@LOWER@@_SOME_VAT_@@
   --  and means that the filter LOWER should be applied to SOME_VAR before
   --  replacing it in the template file.

   type Filters_Mode is
     (Identity,   -- same string as the input
      Invert,     -- reverse string
      Lower,      -- lower case
      Upper,      -- upper case
      Capitalize, -- lower case except char before spaces and underscores
      Clean_Text, -- only letter/digits all other chars are changed to spaces.
      Yes_No,     -- if True return Yes, If False returns No, else do nothing.
      Oui_Non     -- if True return Oui, If False returns Non, else do nothing.
      );

   type Filter_Function is access function (S : in String) return String;

   type String_Access is access constant String;

   type Filter_Record is record
      Name   : String_Access;
      Handle : Filter_Function;
   end record;

   --  filter functions, see above.

   function Lower_Filter      (S : in String) return String;
   function Reverse_Filter    (S : in String) return String;
   function Upper_Filter      (S : in String) return String;
   function Capitalize_Filter (S : in String) return String;
   function Clean_Text_Filter (S : in String) return String;
   function Yes_No_Filter     (S : in String) return String;
   function Oui_Non_Filter    (S : in String) return String;

   function Identity_Filter   (S : in String) return String;
   pragma Inline (Identity_Filter);

   function Check_Filter
     (Str : in Unbounded_String;
      P   : in Positive)
     return Filters_Mode;
   --  returns the prefix filter for the tag variable starting a position P in
   --  Str or Identity if no filter has been found.

   function Through_Filter
     (S      : in String;
      Filter : in Filters_Mode) return String;
   --  apply Filter to string S and return the result string.
   pragma Inline (Through_Filter);

   function Filter_Length (Filter : in Filters_Mode) return Natural;
   --  returns the number of characters for the filter token Filter.

   function Field
     (Vect_Value : in Vector_Tag;
      N          : in Positive) return String;
   --  returns the Nth value in the vector tag.

   function List (A : in Association) return String;
   --  returns the Vector_Tag for the Association as a String, each value is
   --  separated by the given separator.

   procedure Translate
     (Str : in out Unbounded_String;
      Tag : in     String;
      To  : in     String);
   --  Translate all tags named Tag in Str by To
   pragma Inline (Translate);

   ---------
   -- "+" --
   ---------

   function "+" (Value : in String) return Vector_Tag is

      Item : constant Vector_Tag_Node_Access
        := new Vector_Tag_Node'(To_Unbounded_String (Value), null);

   begin
      return Vector_Tag'
        (Ada.Finalization.Controlled with
         Ref_Count => new Integer'(1),
         Count     => 1,
         Head      => Item,
         Last      => Item);
   end "+";

   function "+" (Value : in Character) return Vector_Tag is
   begin
      return +String'(1 => Value);
   end "+";

   function "+" (Value : in Boolean) return Vector_Tag is
   begin
      return +Boolean'Image (Value);
   end "+";

   function "+" (Value : in Strings.Unbounded.Unbounded_String)
     return Vector_Tag is
   begin
      return +To_String (Value);
   end "+";

   function "+" (Value : in Integer) return Vector_Tag is
      S_Value : constant String := Integer'Image (Value);
   begin
      if Value in Natural then
         return +S_Value (S_Value'First + 1 .. S_Value'Last);
      else
         return +S_Value;
      end if;
   end "+";

   ---------
   -- "&" --
   ---------

   function "&"
     (Vect  : in Vector_Tag;
      Value : in String)
     return Vector_Tag
   is
      Item : constant Vector_Tag_Node_Access
        := new Vector_Tag_Node'(To_Unbounded_String (Value), null);
   begin
      Vect.Ref_Count.all := Vect.Ref_Count.all + 1;

      if Vect.Count = 0 then
         return Vector_Tag'
           (Ada.Finalization.Controlled with
            Ref_Count => Vect.Ref_Count,
            Count     => 1,
            Head      => Item,
            Last      => Item);
      else
         Vect.Last.Next := Item;
         return Vector_Tag'
           (Ada.Finalization.Controlled with
            Ref_Count => Vect.Ref_Count,
            Count     => Vect.Count + 1,
            Head      => Vect.Head,
            Last      => Item);
      end if;
   end "&";

   function "&"
     (Vect  : in Vector_Tag;
      Value : in Character)
     return Vector_Tag is
   begin
      return Vect & String'(1 => Value);
   end "&";

   function "&"
     (Vect  : in Vector_Tag;
      Value : in Boolean)
     return Vector_Tag is
   begin
      return Vect & Boolean'Image (Value);
   end "&";

   function "&"
     (Vect  : in Vector_Tag;
      Value : in Strings.Unbounded.Unbounded_String)
     return Vector_Tag is
   begin
      return Vect & To_String (Value);
   end "&";

   function "&"
     (Vect  : in Vector_Tag;
      Value : in Integer)
     return Vector_Tag
   is
      S_Value : constant String := Integer'Image (Value);
   begin
      if Value in Natural then
         return Vect & S_Value (S_Value'First + 1 .. S_Value'Last);
      else
         return Vect & S_Value;
      end if;
   end "&";

   ------------
   -- Adjust --
   ------------

   procedure Adjust (V : in out Vector_Tag) is
   begin
      V.Ref_Count.all := V.Ref_Count.all + 1;
   end Adjust;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (V : in out Vector_Tag) is
   begin
      V.Ref_Count := new Integer'(1);
      V.Count     := 0;
   end Initialize;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (V : in out Vector_Tag) is
   begin
      V.Ref_Count.all := V.Ref_Count.all - 1;

      if V.Ref_Count.all = 0 then
         declare
            procedure Free is new Ada.Unchecked_Deallocation
              (Vector_Tag_Node, Vector_Tag_Node_Access);

            procedure Free is new Ada.Unchecked_Deallocation
              (Integer, Integer_Access);

            P, N : Vector_Tag_Node_Access;
         begin
            P := V.Head;

            while P /= null loop
               N := P.Next;
               Free (P);
               P := N;
            end loop;

            V.Head := null;
            Free (V.Ref_Count);
         end;
      end if;
   end Finalize;

   ----------
   -- Size --
   ----------

   function Size (Vect : in Vector_Tag) return Natural is
   begin
      return Vect.Count;
   end Size;

   ------------------
   -- Cached_Files --
   ------------------

   package body Cached_Files is

      Initial_Size : constant := 20; -- cache initial size
      Growing_Size : constant := 50; -- cache growing size

      type File_Data is record
         Filename : Unbounded_String;
         T        : Tree;
      end record;

      type File_Array is array (Positive range <>) of File_Data;
      type File_Array_Access is access File_Array;

      Files : File_Array_Access;
      Index : Natural := 0;

      procedure Growth;
      --  Growth the size (by Growing_Size places) of Files array.

      ---------
      -- Add --
      ---------

      procedure Add
        (Filename : in String;
         T        : in Tree)
      is
         L_Filename : Unbounded_String := To_Unbounded_String (Filename);
         Place      : Positive;
      begin
         if Files = null or else Index = Files'Last then
            Growth;
         end if;

         Place := Index + 1;

         for K in 1 .. Index loop
            if Files (Index).Filename > L_Filename then
               Place := K;
            end if;
         end loop;

         Files (Place + 1 .. Index + 1) := Files (Place .. Index);

         Index := Index + 1;

         Files (Place) := (L_Filename, T);
      end Add;

      ---------
      -- Get --
      ---------

      function Get (Filename : in String) return Tree is

         L_Filename : constant Unbounded_String
           := To_Unbounded_String (Filename);

         S : Natural := 1;
         E : Natural := Index;
         N : Natural;

      begin
         loop
            exit when S > E;

            N := (S + E) / 2;

            if Files (N).Filename = L_Filename then
               return Files (N).T;

            elsif Files (N).Filename < L_Filename then
               S := N;

            else
               E := N;
            end if;
         end loop;

         return null;
      end Get;

      ------------
      -- Growth --
      ------------

      procedure Growth is

         procedure Free is
            new Ada.Unchecked_Deallocation (File_Array, File_Array_Access);


      begin
         if Files = null then
            Files := new File_Array (1 .. Initial_Size);
         else

            declare
               New_Array : File_Array_Access;
            begin
               New_Array := new File_Array (1 .. Files'Length + Growing_Size);
               New_Array (1 .. Files'Length) := Files.all;
               Free (Files);
               Files := New_Array;
            end;
         end if;
      end Growth;

   end Cached_Files;

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

   -------------------
   -- Yes_No_Filter --
   -------------------

   function Yes_No_Filter (S : in String) return String is
   begin
      if S = "TRUE" then
         return "YES";

      elsif S = "true" then
         return "yes";

      elsif S = "True" then
         return "Yes";

      elsif S = "FALSE" then
         return "NO";

      elsif S = "false" then
         return "no";

      elsif S = "False" then
         return "No";

      else
         return S;
      end if;
   end Yes_No_Filter;

   --------------------
   -- Oui_Non_Filter --
   --------------------

   function Oui_Non_Filter (S : in String) return String is
   begin
      if S = "TRUE" then
         return "OUI";

      elsif S = "true" then
         return "oui";

      elsif S = "True" then
         return "Oui";

      elsif S = "FALSE" then
         return "NON";

      elsif S = "false" then
         return "non";

      elsif S = "False" then
         return "Non";

      else
         return S;
      end if;
   end Oui_Non_Filter;

   --  Filter Table

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
           (Filter_Reverse_Token'Access,    Reverse_Filter'Access),
         Yes_No     =>
           (Filter_Yes_No_Token'Access,     Yes_No_Filter'Access),
         Oui_Non    =>
           (Filter_Oui_Non_Token'Access,    Oui_Non_Filter'Access));

   ------------------
   -- Check_Filter --
   ------------------

   function Check_Filter
     (Str : in Unbounded_String;
      P   : in Positive)
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

   function Through_Filter
     (S      : in String;
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
   -- Field --
   -----------

   function Field
     (Vect_Value : in Vector_Tag;
      N          : in Positive) return String
   is
      P : Vector_Tag_Node_Access := Vect_Value.Head;
   begin
      if N = Vect_Value.Count then
         return To_String (Vect_Value.Last.Value);

      elsif N > Vect_Value.Count then
         return "";

      else
         for K in 1 .. N - 1 loop
            P := P.Next;
         end loop;
         return To_String (P.Value);
      end if;
   end Field;

   ----------
   -- List --
   ----------

   function List (A : in Association) return String is
      Result : Unbounded_String;
      P      : Vector_Tag_Node_Access := A.Vect_Value.Head;
   begin
      if P = null then
         return "";
      else
         Result := P.Value;
         for K in 2 .. A.Vect_Value.Count loop
            P := P.Next;
            Result := Result & A.Separator & P.Value;
         end loop;

         return To_String (Result);
      end if;
   end List;

   -----------
   -- Assoc --
   -----------

   function Assoc
     (Variable  : in String;
      Value     : in String;
      Begin_Tag : in String    := Default_Begin_Tag;
      End_Tag   : in String    := Default_End_Tag)
     return Association is
   begin
      return Association'
        (Std,
         To_Unbounded_String (Begin_Tag & Variable & End_Tag),
         To_Unbounded_String (Value));
   end Assoc;

   function Assoc
     (Variable  : in String;
      Value     : in Ada.Strings.Unbounded.Unbounded_String;
      Begin_Tag : in String    := Default_Begin_Tag;
      End_Tag   : in String    := Default_End_Tag)
     return Association is
   begin
      return Assoc (Variable, To_String (Value), Begin_Tag, End_Tag);
   end Assoc;

   function Assoc
     (Variable  : in String;
      Value     : in Integer;
      Begin_Tag : in String    := Default_Begin_Tag;
      End_Tag   : in String    := Default_End_Tag)
     return Association
   is
      S_Value : constant String := Integer'Image (Value);
   begin
      if Value in Natural then
         return Assoc (Variable,
                       S_Value (S_Value'First + 1 .. S_Value'Last),
                       Begin_Tag,
                       End_Tag);
      else
         return Assoc (Variable, S_Value, Begin_Tag, End_Tag);
      end if;
   end Assoc;

   function Assoc
     (Variable  : in String;
      Value     : in Boolean;
      Begin_Tag : in String    := Default_Begin_Tag;
      End_Tag   : in String    := Default_End_Tag)
     return Association is
   begin
      if Value then
         return Assoc (Variable, "TRUE", Begin_Tag, End_Tag);
      else
         return Assoc (Variable, "FALSE", Begin_Tag, End_Tag);
      end if;
   end Assoc;

   function Assoc
     (Variable  : in String;
      Value     : in Vector_Tag;
      Separator : in String     := Default_Separator;
      Begin_Tag : in String     := Default_Begin_Tag;
      End_Tag   : in String     := Default_End_Tag)
     return Association is
   begin
      return Association'
        (Vect,
         To_Unbounded_String (Begin_Tag & Variable & End_Tag),
         Value,
         To_Unbounded_String (Separator));
   end Assoc;

   ---------------
   -- Translate --
   ---------------

   procedure Translate
     (Str : in out Unbounded_String;
      Tag : in     String;
      To  : in     String)
   is
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

   ----------
   -- Load --
   ----------

   function Load
     (Filename : in String;
      Cached   : in Boolean := False)
     return Tree
   is

      File : Text_IO.File_Type;
      --  File beeing parsed.

      Buffer : String (1 .. 2048); --  current line content
      Last   : Natural;            --  index of last characters read in buffer
      First  : Natural;            --  first non blank characters in buffer

      Line   : Natural := 0;

      --  Line handling

      procedure Fatal_Error (Message : in String);
      --  raise Template_Error exception with message.

      function Get_Next_Line return Boolean;
      --  Get new line in File and set Buffer, Last and First.

      function Get_First_Parameter return Unbounded_String;
      --  Get first parameter in current line (second word), words beeing
      --  separated by a set of blank characters (space or horizontal
      --  tabulation).

      function Is_Stmt (Stmt : in String) return Boolean;
      --  Returns True is Stmt is found at the begining of the current line
      --  ignoring leading blank characters.

      type Parse_Mode is
        (Parse_Std,              --  in standard line
         Parse_If,               --  in a if statement
         Parse_Else,             --  in else part of a if statement
         Parse_Table,            --  in a table statement
         Parse_Section,          --  in new section
         Parse_Section_Content   --  in section content
        );

      function Parse (Mode : in Parse_Mode) return Tree;
      --  Get a line in File and returns the Tree.

      -----------------
      -- Fatal_Error --
      -----------------

      procedure Fatal_Error (Message : in String) is
      begin
         Exceptions.Raise_Exception
           (Template_Error'Identity,
            "In " & Filename
            & " at line" & Natural'Image (Line) & ' ' & Message & '.');
      end Fatal_Error;

      -------------------------
      -- Get_First_Parameter --
      -------------------------

      function Get_First_Parameter return Unbounded_String is
         Blank : Maps.Character_Set := Maps.To_Set (' ' & ASCII.HT);
         Start, Stop : Natural;
      begin
         Start := Strings.Fixed.Index (Buffer (First .. Last), Blank);

         if Start = 0 then
            return Null_Unbounded_String;
         end if;

         Start := Strings.Fixed.Index_Non_Blank (Buffer (Start .. Last));
         Stop  := Strings.Fixed.Index (Buffer (Start .. Last), Blank);

         if Stop = 0 then
            Stop := Last;
         end if;

         return To_Unbounded_String (Buffer (Start .. Stop));
      end Get_First_Parameter;

      -------------------
      -- Get_Next_Line --
      -------------------

      function Get_Next_Line return Boolean is
      begin
         if Text_IO.End_Of_File (File) then
            return True;
         else
            Line := Line + 1;
            Text_IO.Get_Line (File, Buffer, Last);
            First := Strings.Fixed.Index_Non_Blank (Buffer (1 .. Last));
            return False;
         end if;
      end Get_Next_Line;

      -------------
      -- Is_Stmt --
      -------------

      function Is_Stmt (Stmt : in String) return Boolean is
      begin
         return Last /= 0
           and then Buffer (First .. First + Stmt'Length - 1) = Stmt;
      end Is_Stmt;

      -----------
      -- Parse --
      -----------

      function Parse (Mode : in Parse_Mode) return Tree is
         T : Tree;
      begin
         if Mode /= Parse_Section then
            if Get_Next_Line then
               return null;
            end if;
         end if;

         case Mode is
            when Parse_Std =>
               null;

            when Parse_If =>
               if Is_Stmt (Else_Token) or else Is_Stmt (End_If_Token) then
                  return null;

               end if;

               if Is_Stmt (End_Table_Token) then
                  Fatal_Error ("@@END_TABLE@@ found, @@END_IF@@ expected");
               end if;

            when Parse_Else  =>
               if Is_Stmt (End_If_Token) then
                  return null;
               end if;

               if Is_Stmt (End_Table_Token) then
                  Fatal_Error ("@@END_TABLE@@ found, @@END_IF@@ expected");
               end if;

            when Parse_Section =>

               if Is_Stmt (End_If_Token) then
                  Fatal_Error ("@@END_IF@@ found, @@END_TABLE@@ expected");
               end if;

               T := new Node (Section_Stmt);
               T.Next := Parse (Parse_Section_Content);

               if Is_Stmt (End_Table_Token) then
                  T.N_Section := null;
               else
                  T.N_Section := Parse (Parse_Section);
               end if;

               return T;

            when Parse_Section_Content =>
               if Is_Stmt (Section_Token)
                 or else Is_Stmt (End_Table_Token)
               then
                  return null;
               end if;

               if Is_Stmt (End_If_Token) then
                  Fatal_Error ("@@END_IF@@ found, @@END_TABLE@@ expected");
               end if;

            when Parse_Table =>
               if Is_Stmt (End_Table_Token) then
                  return null;
               end if;

               if Is_Stmt (End_If_Token) then
                  Fatal_Error ("@@END_IF@@ found, @@END_TABLE@@ expected");
               end if;

         end case;

         if Is_Stmt (If_Token) then
            T := new Node (If_Stmt);

            T.Cond    := Get_First_Parameter;
            T.N_True  := Parse (Parse_If);

            if Is_Stmt (End_If_Token) then
               T.N_False := null;
            else
               T.N_False := Parse (Parse_Else);
            end if;

            T.Next    := Parse (Mode);

            return T;

         elsif Is_Stmt (Table_Token) then
            T := new Node (Table_Stmt);

            T.Terminate_Sections :=
              Get_First_Parameter = Terminate_Sections_Token;

            T.Sections := Parse (Parse_Section);
            T.Next     := Parse (Mode);

            return T;

         elsif Is_Stmt (Include_Token) then
            T := new Node (Include_Stmt);

            T.Filename := Get_First_Parameter;
            T.File     := Load (To_String (T.Filename), Cached);

            T.Next     := Parse (Mode);

            return T;

         else
            return new Node'(Data,
                             Value => To_Unbounded_String (Buffer (1 .. Last)),
                             Next  => Parse (Mode));
         end if;
      end Parse;

      T : Tree;

   begin
      if Cached then
         T := Cached_Files.Get (Filename);

         if T /= null then
            return T;
         end if;
      end if;

      Text_IO.Open (File, Text_IO.In_File, Filename);

      T := Parse (Parse_Std);

      Text_IO.Close (File);

      if Cached then
         Cached_Files.Add (Filename, T);
      end if;

      return T;
   end Load;

   ----------------
   -- Print_Tree --
   ----------------

   procedure Print_Tree (T : in Tree; Level : in Natural := 0) is

      procedure Print_Indent (L : in Natural) is
         use Ada.Strings.Fixed;
      begin
         Text_IO.Put ((L * 2) * ' ');
      end Print_Indent;

   begin
      if T = null then
         return;
      end if;

      Print_Indent (Level);

      case T.Kind is
         when Data =>
            Text_IO.Put_Line ("[DATA] " & To_String (T.Value));
            Print_Tree (T.Next, Level);

         when If_Stmt  =>
            Text_IO.Put_Line ("[IF_STMT] " & To_String (T.Cond));
            Print_Tree (T.N_True, Level + 1);
            Text_IO.Put_Line ("[ELSE]");
            Print_Tree (T.N_False, Level + 1);
            Print_Indent (Level);
            Text_IO.Put_Line ("[END_IF_STMT]");
            Print_Tree (T.Next, Level);

         when Table_Stmt =>
            Text_IO.Put_Line ("[TABLE_STMT] "
                              & Boolean'Image (T.Terminate_Sections));
            Print_Tree (T.Sections, Level + 1);
            Print_Indent (Level);
            Text_IO.Put_Line ("[END_TABLE_STMT]");
            Print_Tree (T.Next, Level);

         when Section_Stmt =>
            Text_IO.Put_Line ("[SECION_STMT]");
            Print_Tree (T.Next, Level + 1);
            Print_Tree (T.N_Section, Level);

         when Include_Stmt =>
            Text_IO.Put_Line ("[INCLUDE_STMT] " & To_String (T.Filename));
            Print_Tree (T.File, Level + 1);
            Print_Tree (T.Next, Level);
      end case;

   end Print_Tree;

   procedure Print_Tree (Filename : in String) is
      T : Tree;
   begin
      T := Load (Filename);
      Print_Tree (T);
   end Print_Tree;

   -----------
   -- Parse --
   -----------

   function Parse
     (Filename     : in String;
      Translations : in Translate_Table := No_Translation;
      Cached       : in Boolean         := False)
     return String
   is
      Results : Unbounded_String;

      procedure Analyze
        (T              : in Tree;
         N              : in Natural;
         Max_Lines      : in Natural;
         Max_Expand     : in Natural;
         Table_Level    : in Natural;
         Section_Number : in Natural);
      --  Parse T and build results file. Table_Level is the number of table
      --  imbication and Section_Number the section number parsed.

      -------------
      -- Analyze --
      -------------

      procedure Analyze
        (T              : in Tree;
         N              : in Natural;
         Max_Lines      : in Natural;
         Max_Expand     : in Natural;
         Table_Level    : in Natural;
         Section_Number : in Natural)
      is

         procedure Get_Max
           (T : in Tree;
            Max_Lines  : out Natural;
            Max_Expand : out Natural);
         --  Returns the maximum number of lines (Max_Lines) into the
         --  table. This correspond to the length of the shortest vector tag
         --  into the table. Returns also the number of time the table will be
         --  expanded (Max_Expand), this is equal to Max_Lines + offset to
         --  terminate the sections.

         procedure Translate (Str : in out Unbounded_String);
         --  Translate all tags in the translations table and the specials tags
         --  in Str by their corresponding value.

         function Is_True (Str : in String) return Boolean;
         --  Return True if Str is one of "TRUE", "OUI", the case beeing not
         --  case sensitive.

         -------------
         -- Is_True --
         -------------

         function Is_True (Str : in String) return Boolean is
            L_Str : constant String := Characters.Handling.To_Upper (Str);
         begin
            return L_Str = "TRUE" or else L_Str = "OUI";
         end Is_True;

         ---------------
         -- Translate --
         ---------------

         procedure Translate (Str : in out Unbounded_String) is
         begin
            for K in Translations'Range loop
               if Translations (K).Kind = Std then
                  Translate (Str,
                             To_String (Translations (K).Variable),
                             To_String (Translations (K).Value));
               else
                  if Table_Level = 0 then
                     --  this is a vector tag (outside of a table tag
                     --  statement), we display it as a list separated by the
                     --  specified separator.
                     Translate (Str,
                                To_String (Translations (K).Variable),
                                List (Translations (K)));
                  else
                     Translate (Str,
                                To_String (Translations (K).Variable),
                                Field (Translations (K).Vect_Value, N));
                  end if;
               end if;
            end loop;

            Translate (Str,
                       Tag => "@@_TABLE_LINE_@@",
                       To  => Fixed.Trim (Positive'Image (N), Strings.Left));

            Translate (Str,
                       Tag => "@@_NUMBER_LINE_@@",
                       To  => Fixed.Trim (Positive'Image (Max_Lines),
                                          Strings.Left));

            Translate (Str,
                       Tag => "@@_TABLE_LEVEL_@@",
                       To  => Fixed.Trim (Positive'Image (Table_Level),
                                          Strings.Left));
         end Translate;

         -------------
         -- Get_Max --
         -------------

         procedure Get_Max
           (T : in Tree;
            Max_Lines  : out Natural;
            Max_Expand : out Natural)
         is

            function Get_Max_Lines (T : in Tree) return Natural;
            --  Recursivelly descend the tree and compute the max lines that
            --  will be displayed into the table.

            function Check (Str : in String) return Natural;
            --  Returns the length of the smallest vector tag found on Str.

            function Count_Section return Natural;
            --  Returns the number of section into table T;

            -----------
            -- Check --
            -----------

            function Check (Str : in String) return Natural is
               Smallest : Natural := Natural'Last;
            begin
               for K in Translations'Range loop
                  if Translations (K).Kind = Vect
                    and then
                     Fixed.Index (Str,
                                  To_String (Translations (K).Variable)) /= 0
                  then
                     Smallest :=
                       Natural'Min (Smallest,
                                    Size (Translations (K).Vect_Value));
                  end if;
               end loop;

               return Smallest;
            end Check;

            -------------------
            -- Count_Section --
            -------------------

            function Count_Section return Natural is
               C : Natural := 0;
               S : Tree    := T.Sections;
            begin
               while S /= null loop
                  C := C + 1;
                  S := S.N_Section;
               end loop;
               return C;
            end Count_Section;

            -------------------
            -- Get_Max_Lines --
            -------------------

            function Get_Max_Lines (T : in Tree) return Natural is
            begin
               if T = null then
                  return Natural'Last;
               end if;

               case T.Kind is
                  when Data =>
                     return Natural'Min (Check (To_String (T.Value)),
                                         Get_Max_Lines (T.Next));
                  when If_Stmt =>
                     return Natural'Min
                       (Natural'Min (Get_Max_Lines (T.N_True),
                                     Get_Max_Lines (T.N_False)),
                        Get_Max_Lines (T.Next));

                  when Table_Stmt =>
                     return Natural'Last;

                  when Section_Stmt =>
                     return Natural'Min (Get_Max_Lines (T.Next),
                                         Get_Max_Lines (T.N_Section));

                  when Include_Stmt =>
                     return Natural'Min (Get_Max_Lines (T.File),
                                         Get_Max_Lines (T.Next));
               end case;
            end Get_Max_Lines;

            Result : Natural := Get_Max_Lines (T.Sections);

         begin
            pragma Assert (T.Kind = Table_Stmt);

            if Result = Natural'Last then
               Result := 0;
            end if;

            Max_Lines := Result;

            if T.Terminate_Sections then

               declare
                  N_Section : Positive := Count_Section;
               begin
                  if Result mod N_Section /= 0 then
                     Result := Result + N_Section - (Result mod N_Section);
                  end if;
               end;
            end if;

            Max_Expand := Result;
         end Get_Max;

      begin
         if T = null then
            return;
         end if;

         case T.Kind is
            when Data =>
               declare
                  Value : Unbounded_String := T.Value;
               begin
                  Translate (Value);

                  Results := Results & Value & ASCII.LF;
               end;

               Analyze (T.Next, N,
                        Max_Lines, Max_Expand, Table_Level, Section_Number);

            when If_Stmt  =>
               declare
                  Cond : Unbounded_String := T.Cond;
               begin
                  Translate (Cond);

                  if Is_True (To_String (Cond)) then
                     Analyze (T.N_True, N, Max_Lines, Max_Expand,
                              Table_Level, Section_Number);
                  else
                     Analyze (T.N_False, N, Max_Lines, Max_Expand,
                              Table_Level, Section_Number);
                  end if;
               end;

               Analyze (T.Next, N, Max_Lines, Max_Expand,
                        Table_Level, Section_Number);

            when Table_Stmt =>
               declare
                  Max_Lines, Max_Expand : Natural;
               begin
                  Get_Max (T, Max_Lines, Max_Expand);

                  Analyze (T.Sections, 1, Max_Lines, Max_Expand,
                           Table_Level + 1, Section_Number + 1);
               end;

               Analyze (T.Next, N, Max_Lines, Max_Expand,
                        Table_Level, Section_Number);

            when Section_Stmt =>
               declare
                  First_Section : Tree := T;
                  Current       : Tree := T;
                  Section       : Positive := 1;
               begin
                  for K in 1 .. Max_Expand loop
                     Analyze (Current.Next, K,
                              Max_Lines, Max_Expand,
                              Table_Level, Section);

                     Current := Current.N_Section;
                     Section := Section + 1;

                     if Current = null then
                        Current := First_Section;
                        Section := 1;
                     end if;
                  end loop;
               end;

            when Include_Stmt =>
               Analyze (T.File, N,
                        Max_Lines, Max_Expand, Table_Level, Section_Number);

               Analyze (T.Next, N,
                        Max_Lines, Max_Expand, Table_Level, Section_Number);

         end case;
      end Analyze;

      T : Tree;

   begin
      T := Load (Filename, Cached);

      Analyze (T, 0, 0, 0, 0, 0);

      return To_String (Results);
   end Parse;

   ---------------
   -- Translate --
   ---------------

   function Translate
     (Template     : in String;
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

end Templates_Parser;
