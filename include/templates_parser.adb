------------------------------------------------------------------------------
--                             Templates Parser                             --
--                                                                          --
--                        Copyright (C) 1999 - 2003                         --
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
with Ada.Calendar;
with Ada.IO_Exceptions;
with Ada.Strings.Fixed;
with Ada.Strings.Maps.Constants;
with Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;

with GNAT.Calendar.Time_IO;
with GNAT.OS_Lib;
with GNAT.Regexp;

with Templates_Parser.Input;

package body Templates_Parser is

   use Ada;
   use Ada.Exceptions;
   use Ada.Strings;

   Internal_Error : exception;

   Blank : constant Maps.Character_Set := Maps.To_Set (' ' & ASCII.HT);

   function Image (N : in Integer) return String;
   pragma Inline (Image);
   --  Returns N image without leading blank

   function No_Quote (Str : in String) return String;
   --  Removes quotes around Str. If Str (Str'First) and Str (Str'Last)
   --  are quotes return Str (Str'First + 1 ..  Str'Last - 1) otherwise
   --  return Str as-is.

   -----------
   -- Image --
   -----------

   function Image (N : in Integer) return String is
      N_Img : constant String := Integer'Image (N);
   begin
      if N_Img (N_Img'First) = '-' then
         return N_Img;
      else
         return N_Img (N_Img'First + 1 .. N_Img'Last);
      end if;
   end Image;

   --------------
   -- No_Quote --
   --------------

   function No_Quote (Str : in String) return String is
   begin
      if Str (Str'First) = '"' and then Str (Str'Last) = '"' then
         return Str (Str'First + 1 .. Str'Last - 1);
      else
         return Str;
      end if;
   end No_Quote;

   --------------
   -- Tag Info --
   --------------

   Begin_Tag : Unbounded_String := To_Unbounded_String (Default_Begin_Tag);
   End_Tag   : Unbounded_String := To_Unbounded_String (Default_End_Tag);

   Table_Token                : constant String := "@@TABLE@@";
   Terminate_Sections_Token   : constant String := "@@TERMINATE_SECTIONS@@";
   Section_Token              : constant String := "@@SECTION@@";
   End_Table_Token            : constant String := "@@END_TABLE@@";
   If_Token                   : constant String := "@@IF@@";
   Elsif_Token                : constant String := "@@ELSIF@@";
   Else_Token                 : constant String := "@@ELSE@@";
   End_If_Token               : constant String := "@@END_IF@@";
   Include_Token              : constant String := "@@INCLUDE@@";

   ------------
   -- Filter --
   ------------

   package Filter is

      ----------------------
      --  Filters setting --
      ----------------------

      --  A filter appear just before a tag variable (e.g. @_LOWER:SOME_VAR_@
      --  and means that the filter LOWER should be applied to SOME_VAR before
      --  replacing it in the template file.

      type Mode is
        (Multiply,
         --  Multiply the given parameter to the string (operator "*")

         Plus,
         --  Add the given parameter to the string (operator "+")

         Minus,
         --  Substract the given parameter to the string (operator "-")

         Divide,
         --  Divide the given parameter to the string (operator "/")

         Add,
         --  Add the given parameter to the string

         BR_2_LF,
         --  Replaces all <BR> HTML tag by a LF character.

         Capitalize,
         --  Lower case except char before spaces and underscores.

         Clean_Text,
         --  Only letter/digits all other chars are changed to spaces.

         Coma_2_Point,
         --  Replaces comas by points.

         Contract,
         --  Replaces a suite of spaces by a single space character.

         Div,
         --  Divide the given parameter to the string

         Exist,
         --  Returns "TRUE" if var is not empty and "FALSE" otherwise.

         Format_Number,
         --  Returns the number with a space added between each 3 digits
         --  blocks. The decimal part is not transformed. If the data is not a
         --  number nothing is done. The data is trimmed before processing it.

         Is_Empty,
         --  Returns "TRUE" if var is empty and "FALSE" otherwise.

         LF_2_BR,
         --  Replaces all LF character to <BR> HTML tag.

         Lower,
         --  Lower case.

         Match,
         --  Returns "TRUE" if var match the pattern passed as argument.

         Modulo,
         --  Returns current value modulo N (N is the filter parameter)

         Mult,
         --  Multiply the given parameter to the string

         No_Digit,
         --  Replace all digits by spaces.

         No_Letter,
         --  Removes all letters by spaces.

         No_Space,
         --  Removes all spaces found in the value.

         Oui_Non,
         --  If True return Oui, If False returns Non, else do nothing.

         Point_2_Coma,
         --  Replaces points by comas.

         Repeat,
         --  Returns N copy of the original string. The number of copy is
         --  passed as parameter.

         Invert,
         --  Reverse string.

         Size,
         --  Returns the number of characters in the string value.

         Slice,
         --  Returns a slice of the string.

         Sub,
         --  Substract the given parameter to the string

         Trim,
         --  Trim leading and trailing space.

         Upper,
         --  Upper case.

         Web_Escape,
         --  Convert characters "<>&" to HTML equivalents: &lt;, &gt; and &amp;

         Web_NBSP,
         --  Convert spaces to HTML &nbsp; - non breaking spaces.

         Yes_No
         --  If True return Yes, If False returns No, else do nothing.
        );

      type Parameter_Mode is (Void, Str, Regexp, Slice);

      function Parameter (Mode : in Filter.Mode) return Parameter_Mode;
      --  Returns the parameter mode for the given filter.

      type Parameter_Data (Mode : Parameter_Mode := Void) is record
         case Mode is
            when Void =>
               null;

            when Str =>
               S : Unbounded_String;

            when Regexp =>
               R_Str  : Unbounded_String;
               Regexp : GNAT.Regexp.Regexp;

            when Slice =>
               First : Natural;
               Last  : Natural;
         end case;
      end record;

      No_Parameter : constant Parameter_Data := Parameter_Data'(Mode => Void);

      function Image (P : in Parameter_Data) return String;
      --  Returns parameter string representation.

      type Callback is
        access function (S : in String; P : in Parameter_Data := No_Parameter)
        return String;
      --  P is the filter parameter, no parameter by default. Parameter are
      --  untyped and will be parsed by the filter function if needed.

      type Routine is record
         Handle     : Callback;
         Parameters : Parameter_Data;
      end record;

      type Set is array (Positive range <>) of Routine;
      type Set_Access is access Set;

      type String_Access is access constant String;

      type Filter_Record is record
         Name   : String_Access;
         Handle : Callback;
      end record;

      --  filter functions, see above.

      procedure Check_Null_Parameter (P : in Parameter_Data);
      --  Raises Template_Error if P is not equal to Null_Parameter.

      function BR_2_LF
        (S : in String; P : in Parameter_Data := No_Parameter) return String;

      function Capitalize
        (S : in String; P : in Parameter_Data := No_Parameter) return String;

      function Clean_Text
        (S : in String; P : in Parameter_Data := No_Parameter) return String;

      function Coma_2_Point
        (S : in String; P : in Parameter_Data := No_Parameter) return String;

      function Contract
        (S : in String; P : in Parameter_Data := No_Parameter) return String;

      function Exist
        (S : in String; P : in Parameter_Data := No_Parameter) return String;

      function Format_Number
        (S : in String; P : in Parameter_Data := No_Parameter) return String;

      function Is_Empty
        (S : in String; P : in Parameter_Data := No_Parameter) return String;

      function LF_2_BR
        (S : in String; P : in Parameter_Data := No_Parameter) return String;

      function Lower
        (S : in String; P : in Parameter_Data := No_Parameter) return String;

      function Match
        (S : in String; P : in Parameter_Data := No_Parameter) return String;

      function No_Digit
        (S : in String; P : in Parameter_Data := No_Parameter) return String;

      function No_Letter
        (S : in String; P : in Parameter_Data := No_Parameter) return String;

      function No_Space
        (S : in String; P : in Parameter_Data := No_Parameter) return String;

      function Oui_Non
        (S : in String; P : in Parameter_Data := No_Parameter) return String;

      function Point_2_Coma
        (S : in String; P : in Parameter_Data := No_Parameter) return String;

      function Repeat
        (S : in String; P : in Parameter_Data := No_Parameter) return String;

      function Reverse_Data
        (S : in String; P : in Parameter_Data := No_Parameter) return String;

      function Size
        (S : in String; P : in Parameter_Data := No_Parameter) return String;

      function Slice
        (S : in String; P : in Parameter_Data := No_Parameter) return String;

      function Trim
        (S : in String; P : in Parameter_Data := No_Parameter) return String;

      function Upper
        (S : in String; P : in Parameter_Data := No_Parameter) return String;

      function Web_Escape
        (S : in String; P : in Parameter_Data := No_Parameter) return String;

      function Web_NBSP
        (S : in String; P : in Parameter_Data := No_Parameter) return String;

      function Yes_No
        (S : in String; P : in Parameter_Data := No_Parameter) return String;

      function Plus
        (S : in String; P : in Parameter_Data := No_Parameter) return String;

      function Minus
        (S : in String; P : in Parameter_Data := No_Parameter) return String;

      function Divide
        (S : in String; P : in Parameter_Data := No_Parameter) return String;

      function Multiply
        (S : in String; P : in Parameter_Data := No_Parameter) return String;

      function Modulo
        (S : in String; P : in Parameter_Data := No_Parameter) return String;

      function Handle (Name : in String) return Callback;
      --  Returns the filter function for the given filter name.

      function Handle (Mode : in Filter.Mode) return Callback;
      --  Returns the filter function for the given filter mode.

      function Mode_Value (Name : in String) return Mode;
      --  Returns the Mode for filter named Name. This is the internal
      --  representation for this filter name.

      function Name (Handle : in Callback) return String;
      --  Returns the filter name for the given filter function.

   end Filter;

   --------------------
   --  Tags variable --
   --------------------

   type Attribute is (Nil, Length, Line, Min_Column, Max_Column);

   type Tag is record
      Name    : Unbounded_String;
      Filters : Filter.Set_Access;
      Attr    : Attribute := Nil;
   end record;

   function Build (Str : in String) return Tag;
   --  Create a Tag from Str. A tag is composed of a name and a set of
   --  filters.

   function Image (T : in Tag) return String;
   --  Returns string representation for the Tag variable.

   function Translate (T : in Tag; Value : in String) return String;
   --  Returns the result of Value after applying all filters for tag T.

   procedure Release (T : in out Tag);
   --  Release all memory associated with Tag.

   -----------
   -- Image --
   -----------

   function Image (T : in Tag) return String is
      use type Filter.Set_Access;
      R : Unbounded_String;
   begin
      R := Begin_Tag;

      --  Filters

      if T.Filters /= null then
         for K in reverse T.Filters'Range loop
            Append (R, Filter.Name (T.Filters (K).Handle));
            Append (R, Filter.Image (T.Filters (K).Parameters));
            Append (R, ":");
         end loop;
      end if;

      --  Tag name

      Append (R, T.Name);

      --  Attributes

      case T.Attr is
         when Nil        => null;
         when Length     => Append (R, "'Length");
         when Line       => Append (R, "'Line");
         when Min_Column => Append (R, "'Min_Column");
         when Max_Column => Append (R, "'Max_Column");
      end case;

      Append (R, End_Tag);

      return To_String (R);
   end Image;

   -----------
   -- Build --
   -----------

   function Build (Str : in String) return Tag is

      function Get_Var_Name (Tag : in String) return Unbounded_String;
      --  Given a Tag name, it returns the variable name only. It removes
      --  the tag separator and the filters.

      function Get_Filter_Set (Tag : in String) return Filter.Set_Access;
      --  Given a tag name, it retruns a set of filter to apply to this
      --  variable when translated.

      function Get_Attribute (Tag : in String) return Attribute;
      --  Returns attribute for the given tag.

      F_Sep : constant Natural
        := Strings.Fixed.Index (Str, ":", Strings.Backward);
      --  Last filter separator

      A_Sep : constant Natural
        := Strings.Fixed.Index (Str, "'", Strings.Backward);
      --  Attribute separator

      -------------------
      -- Get_Attribute --
      -------------------

      function Get_Attribute (Tag : in String) return Attribute is
         Start, Stop : Natural;
      begin
         if A_Sep = 0 then
            return Nil;
         else
            Start := A_Sep + 1;
            Stop  := Tag'Last - Length (End_Tag);
         end if;

         declare
            A_Name : constant String
              := Characters.Handling.To_Lower (Tag (Start .. Stop));
         begin
            if A_Name = "length" then
               return Length;

            elsif A_Name = "line" then
               return Line;

            elsif A_Name = "min_column" then
               return Min_Column;

            elsif A_Name = "max_column" then
               return Max_Column;

            else
               Exceptions.Raise_Exception
                 (Template_Error'Identity,
                  "Unknown attribute name """ & A_Name & '"');
            end if;
         end;
      end Get_Attribute;

      --------------------
      -- Get_Filter_Set --
      --------------------

      function Get_Filter_Set (Tag : in String) return Filter.Set_Access is

         Start : Natural;
         Stop  : Natural := Tag'Last;
         FS    : Filter.Set (1 .. Strings.Fixed.Count (Tag, ":"));
         K     : Positive := FS'First;

         function Name_Parameter (Filter : in String) return Filter.Routine;
         --  Given a Filter description, returns the filter handle and
         --  parameter.

         procedure Get_Slice (Slice : in String; First, Last : out Natural);
         --  Returns the First and Last slice index as parsed into the Slice
         --  string. Retruns First and Last set to 0 if there is not valid
         --  slice definition in Slice.

         ---------------
         -- Get_Slice --
         ---------------

         procedure Get_Slice (Slice : in String; First, Last : out Natural) is
            P1 : constant Natural := Fixed.Index (Slice, "..");
         begin
            First := 0;
            Last  := 0;

            if P1 = 0 then
               Exceptions.Raise_Exception
                 (Template_Error'Identity, "slice expected """ & Slice & '"');

            else
               First := Natural'Value (Slice (Slice'First .. P1 - 1));
               Last  := Natural'Value (Slice (P1 + 2 .. Slice'Last));
            end if;
         end Get_Slice;

         --------------------
         -- Name_Parameter --
         --------------------

         function Name_Parameter (Filter : in String) return Filter.Routine is
            use Strings;

            package F renames Templates_Parser.Filter;

            P1 : constant Natural := Fixed.Index (Filter, "(");
            P2 : constant Natural := Fixed.Index (Filter, ")", Backward);
         begin
            if (P1 = 0 and then P2 /= 0) or else (P1 /= 0 and then P2 = 0) then
               Exceptions.Raise_Exception
                 (Template_Error'Identity,
                  "unbalanced parenthesis """ & Filter & '"');

            elsif P2 /= 0
              and then P2 < Filter'Last
              and then Filter (P2 + 1) /= ':'
            then
               Exceptions.Raise_Exception
                 (Template_Error'Identity,
                  "unexpected character after parenthesis """ & Filter & '"');
            end if;

            if P1 = 0 then
               --  No parenthesis, so there is no parameter to parse
               return (F.Handle (Filter),
                       F.Parameter_Data'(Mode => F.Void));

            else
               declare
                  Name : constant String
                    := Filter (Filter'First .. P1 - 1);

                  Mode : constant F.Mode := F.Mode_Value (Name);

                  Parameter : constant String
                    := No_Quote (Filter (P1 + 1 .. P2 - 1));
               begin
                  case F.Parameter (Mode) is
                     when F.Regexp =>
                        return (F.Handle (Mode),
                                F.Parameter_Data'
                                  (F.Regexp,
                                   To_Unbounded_String (Parameter),
                                   GNAT.Regexp.Compile (Parameter)));

                     when F.Slice =>
                        declare
                           First, Last : Natural;
                        begin
                           Get_Slice (Parameter, First, Last);

                           return (F.Handle (Mode),
                                   F.Parameter_Data'(F.Slice, First, Last));
                        end;

                     when F.Str =>
                        return (F.Handle (Mode),
                                F.Parameter_Data'
                                  (F.Str,
                                   To_Unbounded_String (Parameter)));

                     when F.Void =>
                        pragma Warnings (Off);
                        null;
                  end case;
               end;
            end if;
         end Name_Parameter;

      begin
         if FS'Length = 0 then
            return null;
         end if;

         loop
            Start := Tag'First;

            Stop := Strings.Fixed.Index
              (Tag (Start .. Stop), ":", Strings.Backward);

            exit when Stop = 0;

            Start := Strings.Fixed.Index
              (Tag (Start .. Stop - 1), ":", Strings.Backward);

            if Start = 0 then
               --  Last filter found
               FS (K) := Name_Parameter
                 (Tag (Tag'First + Length (Begin_Tag) .. Stop - 1));
            else
               FS (K) := Name_Parameter (Tag (Start + 1 .. Stop - 1));
            end if;

            K := K + 1;

            Stop := Stop - 1;
         end loop;

         return new Filter.Set'(FS);
      end Get_Filter_Set;

      ------------------
      -- Get_Var_Name --
      ------------------

      function Get_Var_Name (Tag : in String) return Unbounded_String is
         Start, Stop : Natural;
      begin
         if A_Sep = 0 then
            --  No attribute
            Stop := Tag'Last - Length (End_Tag);
         else
            Stop := A_Sep - 1;
         end if;

         if F_Sep = 0 then
            --  No filter
            Start := Tag'First + Length (Begin_Tag);
         else
            Start := F_Sep + 1;
         end if;

         return To_Unbounded_String (Tag (Start .. Stop));
      end Get_Var_Name;

   begin
      return (Get_Var_Name (Str), Get_Filter_Set (Str), Get_Attribute (Str));
   end Build;

   -------------
   -- Release --
   -------------

   procedure Release (T : in out Tag) is
      procedure Free is
         new Ada.Unchecked_Deallocation (Filter.Set, Filter.Set_Access);
   begin
      Free (T.Filters);
   end Release;

   ---------------
   -- Translate --
   ---------------

   function Translate (T : in Tag; Value : in String) return String is
      use type Filter.Set_Access;
   begin
      if T.Filters /= null then
         declare
            R : Unbounded_String := To_Unbounded_String (Value);
         begin
            for K in T.Filters'Range loop
               R := To_Unbounded_String
                 (T.Filters (K).Handle (To_String (R),
                                        T.Filters (K).Parameters));
            end loop;

            return To_String (R);
         end;
      end if;

      return Value;
   end Translate;

   ----------
   -- Data --
   ----------

   package Data is

      type Node;
      type Tree is access Node;

      type NKind is (Text, Var);

      type Node (Kind : NKind) is record
         Next : Tree;
         case Kind is
            when Text =>
               Value : Unbounded_String;
            when Var =>
               Var   : Tag;
         end case;
      end record;

      function Parse (Line : in String) return Tree;
      --  Parse text line and returns the corresponding tree representation.

      procedure Print_Tree (D : in Tree);
      --  Decend the text tree and print it to the standard output.

      procedure Release (D : in out Tree);
      --  Release all memory used by the tree.

   end Data;

   ------------------
   --  Expressions --
   ------------------

   package Expr is

      type Ops is (O_And, O_Or, O_Xor,
                   O_Sup, O_Inf, O_Esup, O_Einf, O_Equal, O_Diff);

      function Image (O : in Ops) return String;
      --  Returns Ops string representation.

      function Value (O : in String) return Ops;
      --  Returns Ops from its string representation. Raises Templates_Error if
      --  the token is not a known operation.

      type U_Ops is (O_Not);

      function Image (O : in U_Ops) return String;
      --  Returns U_Ops string representation.

      function Value (O : in String) return U_Ops;
      --  Returns U_Ops from its string representation. Raises Templates_Error
      --  if the token is not a known operation.

      type Node;
      type Tree is access Node;

      type NKind is (Value, Var, Op, U_Op);
      --  The node is a value, a variable a binary operator or an unary
      --  operator

      type Node (Kind : NKind) is record
         case Kind is
            when Value =>
               V   : Unbounded_String;

            when Var =>
               Var : Tag;

            when Op =>
               O           : Ops;
               Left, Right : Tree;

            when U_Op =>
               U_O         : U_Ops;
               Next        : Tree;
         end case;
      end record;

      function Parse (Expression : in String) return Tree;
      --  Parse Expression and returns the corresponding tree representation.

      procedure Print_Tree (E : in Tree);
      --  Decend the expression's tree and print the expression. It outputs the
      --  expression with all parenthesis to show without ambiguity the way the
      --  expression has been parsed.

      procedure Release (E : in out Tree);
      --  Release all associated memory with the tree.

   end Expr;

   --------------------------------
   --  Template Tree definitions --
   --------------------------------

   type Nkind is (Info,          --  first node is tree infos
                  C_Info,        --  second node is cache tree info
                  Text,          --  this is a text line
                  If_Stmt,       --  an IF tag statement
                  Table_Stmt,    --  a TABLE tag statement
                  Section_Stmt,  --  a TABLE section
                  Include_Stmt); --  an INCLUDE tag statement

   --  A template line is coded as a suite of Data and Var element.

   --  The first node in the tree is of type Info and should never be release
   --  and changed. This ensure that included tree will always be valid
   --  otherwise will would have to parse all the current trees in the cache
   --  to update the reference.

   type Node;
   type Tree is access Node;

   --  Static_Tree represent a Tree immune to cache changes. Info point to the
   --  first node and C_Info to the second one. C_Info could be different to
   --  Info.Next in case of cache changes. This way we keep a pointer to the
   --  old tree to be able to release it when not used anymore. This way it is
   --  possible to use the cache in multitasking program without trouble. The
   --  changes in the cache are either because more than one task is parsing
   --  the same template at the same time, they will update the cache with the
   --  same tree at some point, or because a newer template was found in the
   --  file system.

   type Static_Tree is record
      Info   : Tree;
      C_Info : Tree;
   end record;

   type Node (Kind : Nkind) is record
      Next : Tree;
      Line : Natural;

      case Kind is
         when Info =>
            Filename  : Unbounded_String;    --  Name of the file
            Timestamp : GNAT.OS_Lib.OS_Time; --  Date and Time of last change
            I_File    : Tree;                --  Included file references

            --  Used for the cache system

            Ref       : Natural := 0;        --  Number of ref in the cache

         when C_Info =>
            Obsolete  : Boolean := False;    --  True if newer version in cache
            Used      : Natural := 0;        --  >0 if currently used

         when Text =>
            Text      : Data.Tree;

         when If_Stmt =>
            Cond      : Expr.Tree;
            N_True    : Tree;
            N_False   : Tree;

         when Table_Stmt =>
            Terminate_Sections : Boolean;
            Sections           : Tree;

         when Section_Stmt =>
            N_Section : Tree;

         when Include_Stmt =>
            File : Static_Tree;
      end case;
   end record;

   procedure Release (T : in out Tree);
   --  Release all memory associated with the tree.

   procedure Free is new Ada.Unchecked_Deallocation (Node, Tree);

   -------------------
   --  Cached Files --
   -------------------

   --  Cached_Files keep the parsed Tree for a given file in memory. This
   --  implementation is thread safe so it is possible to use the cache in a
   --  multitasking program.

   package Cached_Files is

      protected Prot is

         procedure Add
           (Filename : in     String;
            T        : in     Tree;
            Old      :    out Tree);
         --  Add Filename/T to the list of cached files. If Filename is
         --  already in the list, replace the current tree with T. Furthemore
         --  if Filename tree is already in use, Old will be set with the
         --  previous C_Info node otherwise Old will be T.Next (C_Info node
         --  for current tree).

         procedure Get
           (Filename : in     String;
            Load     : in     Boolean;
            Result   :    out Static_Tree);
         --  Returns the Tree for Filename or null if Filename has not been
         --  cached. Load must be set to True at load stage and False at Parse
         --  stage.

         procedure Release (T : in out Static_Tree);
         --  After loading a tree and using it, it is required that it be
         --  released. This will ensure that a tree marked as obsolete (a new
         --  version being now in the cache) will be released from the memory.

      end Prot;

   end Cached_Files;

   ----------------
   -- Vector_Tag --
   ----------------

   procedure Field
     (Vect   : in     Vector_Tag;
      N      : in     Positive;
      Result :    out Unbounded_String;
      Found  :    out Boolean);
   --  Returns the Nth value in the vector tag. Found is set to False if
   --  N > Vect_Value'Last.

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
           Last      => Item,
           Current   => new Vector_Tag_Node_Access'(Item),
           Pos       => new Integer'(1));
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
   begin
      return +Image (Value);
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
            Last      => Item,
            Current   => new Vector_Tag_Node_Access'(Item),
            Pos       => Vect.Pos);
      else
         Vect.Last.Next := Item;
         return Vector_Tag'
           (Ada.Finalization.Controlled with
            Ref_Count => Vect.Ref_Count,
            Count     => Vect.Count + 1,
            Head      => Vect.Head,
            Last      => Item,
            Current   => Vect.Current,
            Pos       => Vect.Pos);
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
      return Vector_Tag is
   begin
      return Vect & Image (Value);
   end "&";

   -----------
   -- Clear --
   -----------

   procedure Clear (Vect : in out Vector_Tag) is
   begin
      --  Here we just separate current vector from the new one. The memory
      --  used by the current one will be collected by the Finalize
      --  routine. We just want a new independant Vector_Tag here.

      Finalize (Vect);

      Vect.Ref_Count := new Integer'(1);
      Vect.Pos       := new Integer'(1);
      Vect.Count     := 0;
      Vect.Head      := null;
      Vect.Last      := null;
      Vect.Current   := null;
   end Clear;

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
      V.Pos       := new Integer'(1);
      V.Count     := 0;
   end Initialize;

   ----------
   -- Item --
   ----------

   function Item (Vect : in Vector_Tag; N : in Positive) return String is
      Result : Unbounded_String;
      Found  : Boolean;
   begin
      Field (Vect, N, Result, Found);

      if not Found then
         raise Constraint_Error;
      else
         return To_String (Result);
      end if;
   end Item;

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
              (Vector_Tag_Node_Access, Access_Vector_Tag_Node_Access);

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
            V.Last := null;

            Free (V.Ref_Count);
            Free (V.Pos);
            Free (V.Current);
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

   ----------------
   -- Matrix_Tag --
   ----------------

   procedure Field
     (Matrix : in     Matrix_Tag;
      I, J   : in     Natural;
      Result :    out Unbounded_String;
      Found  :    out Boolean);
   --  Returns Value in Mat_Value (I, J). Found is set to False if there is
   --  no such value in Mat_Value.

   procedure Vector
     (Matrix : in     Matrix_Tag;
      N      : in     Positive;
      Vect   :    out Vector_Tag;
      Found  :    out Boolean);
   --  Returns Vect in Matrix (N). Found is set to False if there is no such
   --  vector in Matrix.

   ---------
   -- "+" --
   ---------

   function "+" (Vect : in Vector_Tag) return Matrix_Tag is
      Item : constant Matrix_Tag_Node_Access
        := new Matrix_Tag_Node'(Vect, null);
      V_Size : constant Natural := Size (Vect);
   begin
      return Matrix_Tag'
        (M => (Ada.Finalization.Controlled with
                 Ref_Count => New Integer'(1),
                 Count     => 1,
                 Min       => V_Size,
                 Max       => V_Size,
                 Head      => Item,
                 Last      => Item,
                 Current   => new Matrix_Tag_Node_Access'(Item),
                 Pos       => new Integer'(1)));
   end "+";

   ---------
   -- "&" --
   ---------

   function "&"
     (Matrix : in Matrix_Tag;
      Vect   : in Vector_Tag)
      return Matrix_Tag
   is
      Item : constant Matrix_Tag_Node_Access
        := new Matrix_Tag_Node'(Vect, null);
      V_Size : constant Natural := Size (Vect);
   begin
      Matrix.M.Ref_Count.all := Matrix.M.Ref_Count.all + 1;

      if Matrix.M.Head = null then
         return (M => (Ada.Finalization.Controlled with
                       Matrix.M.Ref_Count,
                       Matrix.M.Count + 1,
                       Min     => Natural'Min (Matrix.M.Min, V_Size),
                       Max     => Natural'Max (Matrix.M.Max, V_Size),
                       Head    => Item,
                       Last    => Item,
                       Current => new Matrix_Tag_Node_Access'(Item),
                       Pos     => Matrix.M.Pos));
      else
         Matrix.M.Last.Next := Item;
         return (M => (Ada.Finalization.Controlled with
                       Matrix.M.Ref_Count,
                       Matrix.M.Count + 1,
                       Min     => Natural'Min (Matrix.M.Min, V_Size),
                       Max     => Natural'Max (Matrix.M.Max, V_Size),
                       Head    => Matrix.M.Head,
                       Last    => Item,
                       Current => Matrix.M.Current,
                       Pos     => Matrix.M.Pos));
      end if;
   end "&";

   ----------
   -- Size --
   ----------

   function Size (Matrix : in Matrix_Tag) return Natural is
   begin
      return Matrix.M.Count;
   end Size;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (M : in out Matrix_Tag_Int) is
   begin
      M.Ref_Count := new Integer'(1);
      M.Pos       := new Integer'(1);
      M.Count     := 0;
      M.Min       := Natural'Last;
      M.Max       := 0;
   end Initialize;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (M : in out Matrix_Tag_Int) is
   begin
      M.Ref_Count.all := M.Ref_Count.all - 1;

      if M.Ref_Count.all = 0 then
         declare
            procedure Free is new Ada.Unchecked_Deallocation
              (Matrix_Tag_Node, Matrix_Tag_Node_Access);

            procedure Free is new Ada.Unchecked_Deallocation
              (Matrix_Tag_Node_Access, Access_Matrix_Tag_Node_Access);

            procedure Free is new Ada.Unchecked_Deallocation
              (Integer, Integer_Access);

            P, N : Matrix_Tag_Node_Access;
         begin
            P := M.Head;

            while P /= null loop
               N := P.Next;
               Free (P);
               P := N;
            end loop;

            M.Head := null;
            M.Last := null;

            Free (M.Ref_Count);
            Free (M.Pos);
            Free (M.Current);
         end;
      end if;
   end Finalize;

   ------------
   -- Adjust --
   ------------

   procedure Adjust (M : in out Matrix_Tag_Int) is
   begin
      M.Ref_Count.all := M.Ref_Count.all + 1;
   end Adjust;

   ------------
   -- Vector --
   ------------

   procedure Vector
     (Matrix : in     Matrix_Tag;
      N      : in     Positive;
      Vect   :    out Vector_Tag;
      Found  :    out Boolean) is
   begin
      Found := True;

      if N = Matrix.M.Count then
         Vect := Matrix.M.Last.Vect;

      elsif N >= Matrix.M.Pos.all then

         for K in 1 .. N - Matrix.M.Pos.all loop
            Matrix.M.Pos.all     := Matrix.M.Pos.all + 1;
            Matrix.M.Current.all := Matrix.M.Current.all.Next;
         end loop;

         Vect := Matrix.M.Current.all.Vect;

      elsif N > Matrix.M.Count then
         Found  := False;

      else
         declare
            P : Matrix_Tag_Node_Access := Matrix.M.Head;
         begin
            for K in 1 .. N - 1 loop
               P := P.Next;
            end loop;

            Matrix.M.Pos.all     := N;
            Matrix.M.Current.all := P;

            Vect := P.Vect;
         end;
      end if;
   end Vector;

   function Vector
     (Matrix : in Matrix_Tag;
      N      : in Positive)
      return Vector_Tag
   is
      Result : Vector_Tag;
      Found  : Boolean;
   begin
      Vector (Matrix, N, Result, Found);

      if Found then
         return Result;
      else
         Exceptions.Raise_Exception
           (Constraint_Error'Identity, "Index out of range");
      end if;
   end Vector;

   ------------------
   -- Cached_Files --
   ------------------

   package body Cached_Files is separate;

   ----------
   -- Data --
   ----------

   package body Data is separate;

   ----------
   -- Expr --
   ----------

   package body Expr is separate;

   -----------
   -- Field --
   -----------

   procedure Field
     (Vect   : in     Vector_Tag;
      N      : in     Positive;
      Result :    out Unbounded_String;
      Found  :    out Boolean) is
   begin
      Found := True;

      if N = Vect.Count then
         Result := Vect.Last.Value;

      elsif N > Vect.Count then
         Result := Null_Unbounded_String;
         Found  := False;

      elsif N >= Vect.Pos.all then

         for K in 1 .. N - Vect.Pos.all loop
            Vect.Pos.all     := Vect.Pos.all + 1;
            Vect.Current.all := Vect.Current.all.Next;
         end loop;

         Result := Vect.Current.all.Value;

      else
         declare
            P : Vector_Tag_Node_Access := Vect.Head;
         begin
            for K in 1 .. N - 1 loop
               P := P.Next;
            end loop;

            Vect.Pos.all     := N;
            Vect.Current.all := P;

            Result := P.Value;
         end;
      end if;
   end Field;

   procedure Field
     (Matrix : in     Matrix_Tag;
      I, J   : in     Natural;
      Result :    out Unbounded_String;
      Found  :    out Boolean) is
   begin
      Found := True;

      if I = Matrix.M.Count then
         Field (Matrix.M.Last.Vect, J, Result, Found);

      elsif I > Matrix.M.Count then
         Result := Null_Unbounded_String;
         Found  := False;

      elsif I >= Matrix.M.Pos.all then

         for K in 1 .. I - Matrix.M.Pos.all loop
            Matrix.M.Pos.all     := Matrix.M.Pos.all + 1;
            Matrix.M.Current.all := Matrix.M.Current.all.Next;
         end loop;

         Field (Matrix.M.Current.all.Vect, J, Result, Found);

      else
         declare
            P : Matrix_Tag_Node_Access := Matrix.M.Head;
         begin
            for K in 1 .. I - 1 loop
               P := P.Next;
            end loop;

            Matrix.M.Pos.all     := I;
            Matrix.M.Current.all := P;

            Field (P.Vect, J, Result, Found);
         end;
      end if;
   end Field;

   ------------
   -- Filter --
   ------------

   package body Filter is separate;

   -----------
   -- Assoc --
   -----------

   function Assoc
     (Variable  : in String;
      Value     : in String)
      return Association is
   begin
      return Association'
        (Std,
         To_Unbounded_String (Variable),
         To_Unbounded_String (Value));
   end Assoc;

   function Assoc
     (Variable  : in String;
      Value     : in Ada.Strings.Unbounded.Unbounded_String)
      return Association is
   begin
      return Assoc (Variable, To_String (Value));
   end Assoc;

   function Assoc
     (Variable  : in String;
      Value     : in Integer)
      return Association
   is
      S_Value : constant String := Integer'Image (Value);
   begin
      return Assoc (Variable, Image (Value));
   end Assoc;

   function Assoc
     (Variable  : in String;
      Value     : in Boolean)
      return Association is
   begin
      if Value then
         return Assoc (Variable, "TRUE");
      else
         return Assoc (Variable, "FALSE");
      end if;
   end Assoc;

   function Assoc
     (Variable  : in String;
      Value     : in Vector_Tag;
      Separator : in String     := Default_Separator)
      return Association is
   begin
      return Association'
        (Vect,
         To_Unbounded_String (Variable),
         Value,
         To_Unbounded_String (Separator));
   end Assoc;

   function Assoc
     (Variable  : in String;
      Value     : in Matrix_Tag;
      Separator : in String     := Default_Separator)
      return Association is
   begin
      return Association'
        (Matrix,
         To_Unbounded_String (Variable),
         Value,
         To_Unbounded_String (Separator));
   end Assoc;

   ----------
   -- Load --
   ----------

   function Load
     (Filename     : in String;
      Cached       : in Boolean := False;
      Include_File : in Boolean := False)
      return Static_Tree
   is

      File   : Input.File_Type;    --  file beeing parsed.

      Buffer : String (1 .. 2048); --  current line content
      Last   : Natural;            --  index of last characters read in buffer
      First  : Natural;            --  first non blank characters in buffer

      Line   : Natural := 0;

      I_File : Tree;               --  list of includes

      Error_Include_Filename : Unbounded_String;
      --  This variable will be set with the name of the include file that was
      --  not possible to load.

      --  Line handling

      procedure Fatal_Error (Message : in String);
      pragma No_Return (Fatal_Error);
      --  raise Template_Error exception with message.

      function Get_Next_Line return Boolean;
      --  Get new line in File and set Buffer, Last and First. Returns True if
      --  end of file reached.

      function Get_First_Parameter return Unbounded_String;
      --  Get first parameter in current line (second word), words beeing
      --  separated by a set of blank characters (space or horizontal
      --  tabulation).

      function Get_All_Parameters return String;
      --  Get all parameters on the current line.

      function Is_Stmt (Stmt : in String) return Boolean;
      pragma Inline (Is_Stmt);
      --  Returns True is Stmt is found at the begining of the current line
      --  ignoring leading blank characters.

      function EOF return Boolean;
      pragma Inline (EOF);
      --  Returns True if the end of file has been reach.

      function Build_Include_Pathname
        (Include_Filename : in Unbounded_String)
         return String;
      --  Returns the full pathname to the include file (Include_Filename). It
      --  returns Include_Filename if there is a pathname specified, or the
      --  pathname of the main template file as a prefix of the include
      --  filename.

      procedure Replace_Include_Variables
        (File      : in out Static_Tree;
         Variables : in     String);
      --  Parse the include tree and replace all include variables (numeric
      --  name) with the corresponding value in Variables (a set of space
      --  separated words). The first word in Variables is the include file
      --  name (variable 0), other words are the parameters (variable 1 .. N).

      type Parse_Mode is
        (Parse_Std,              --  in standard line
         Parse_If,               --  in a if statement
         Parse_Elsif,            --  in elsif part of a if statement
         Parse_Else,             --  in else part of a if statement
         Parse_Table,            --  in a table statement
         Parse_Section,          --  in new section
         Parse_Section_Content   --  in section content
        );

      function Parse
        (Mode    : in Parse_Mode;
         No_Read : in Boolean := False)
         return Tree;
      --  Get a line in File and returns the Tree.

      ----------------------------
      -- Build_Include_Pathname --
      ----------------------------

      function Build_Include_Pathname
        (Include_Filename : in Unbounded_String)
         return String
      is
         K : constant Natural
           := Index (Include_Filename, Maps.To_Set ("/\"),
                     Going => Strings.Backward);
      begin
         if K = 0 then
            declare
               K : constant Natural
                 := Fixed.Index (Filename, Maps.To_Set ("/\"),
                                 Going => Strings.Backward);
            begin
               if K = 0 then
                  return To_String (Include_Filename);
               else
                  return Filename (Filename'First .. K)
                    & To_String (Include_Filename);
               end if;
            end;
         else
            return To_String (Include_Filename);
         end if;
      end Build_Include_Pathname;

      ---------
      -- EOF --
      ---------

      function EOF return Boolean is
      begin
         return Last = 0;
      end EOF;

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

      ------------------------
      -- Get_All_Parameters --
      ------------------------

      function Get_All_Parameters return String is
         Start : Natural;
      begin
         Start := Strings.Fixed.Index (Buffer (First .. Last), Blank);

         if Start = 0 then
            Fatal_Error ("missing parameter");
         end if;

         if Buffer (Last) = ASCII.CR then
            --  Last character is a DOS CR (certainly because the template
            --  file is in DOS format), ignore it as this is not part of the
            --  parameter.
            Last := Last - 1;
         end if;

         return Strings.Fixed.Trim (Buffer (Start .. Last), Strings.Both);
      end Get_All_Parameters;

      -------------------------
      -- Get_First_Parameter --
      -------------------------

      function Get_First_Parameter return Unbounded_String is
         Start, Stop : Natural;
      begin
         Start := Strings.Fixed.Index (Buffer (First .. Last), Blank);

         if Start = 0 then
            return Null_Unbounded_String;
         end if;

         Start := Strings.Fixed.Index (Buffer (Start .. Last), Blank, Outside);

         if Start = 0 then
            --  We have only spaces after the first word, there is no
            --  parameter in this case.
            return Null_Unbounded_String;
         end if;

         Stop  := Strings.Fixed.Index (Buffer (Start .. Last), Blank);

         if Stop = 0 then
            Stop := Last;
         else
            Stop := Stop - 1;
         end if;

         return To_Unbounded_String (Buffer (Start .. Stop));
      end Get_First_Parameter;

      -------------------
      -- Get_Next_Line --
      -------------------

      function Get_Next_Line return Boolean is
         use type Maps.Character_Set;

         Skip_End : constant Maps.Character_Set
           := Blank or Maps.To_Set (ASCII.CR);
      begin
         if Input.End_Of_File (File) then
            Last := 0;
            return True;

         else
            Line := Line + 1;

            loop
               Input.Get_Line (File, Buffer, Last);
               exit when Buffer (Buffer'First .. Buffer'First + 3) /= "@@--";

               if Input.End_Of_File (File) then
                  --  We have reached the end of file, exit now.
                  Last := 0;
                  return True;
               end if;
            end loop;

            First := Strings.Fixed.Index (Buffer (1 .. Last), Blank, Outside);

            if First = 0 then
               --  There is only spaces on this line, this is an empty line
               --  we just have to skip it.
               Last := 0;
               return False;
            end if;

            Last := Strings.Fixed.Index
              (Buffer (1 .. Last), Skip_End, Outside, Strings.Backward);

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

      function Parse
        (Mode    : in Parse_Mode;
         No_Read : in Boolean := False)
         return Tree
      is
         T : Tree;
      begin
         if not No_Read
           and then (Mode /= Parse_Section and then Mode /= Parse_Elsif)
         then
            if Get_Next_Line then
               return null;
            end if;
         end if;

         case Mode is
            when Parse_Std =>
               if Is_Stmt (End_If_Token) then
                  Fatal_Error
                    ("@@END_IF@@ found outside an @@IF@@ statement");
               end if;

               if Is_Stmt (End_Table_Token) then
                  Fatal_Error
                    ("@@END_TABLE@@ found outside a @@TABLE@@ statement");
               end if;

            when Parse_If =>
               if Is_Stmt (Else_Token)
                 or else Is_Stmt (Elsif_Token)
                 or else Is_Stmt (End_If_Token)
               then
                  return null;
               end if;

               if Is_Stmt (End_Table_Token) then
                  Fatal_Error ("@@END_TABLE@@ found, @@END_IF@@ expected");
               end if;

            when Parse_Elsif =>
               if Is_Stmt (Else_Token)
                 or else Is_Stmt (End_If_Token)
               then
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

               if Is_Stmt (Elsif_Token) then
                  Fatal_Error ("@@ELSIF@@ found after @@ELSE@@");
               end if;

            when Parse_Section =>
               if Is_Stmt (End_If_Token) then
                  Fatal_Error ("@@END_IF@@ found, @@END_TABLE@@ expected");
               end if;

               T := new Node (Section_Stmt);

               T.Line := Line;

               T.Next := Parse (Parse_Section_Content);

               if Is_Stmt (End_Table_Token) then
                  T.N_Section := null;

               elsif EOF then
                  Fatal_Error ("EOF found, @@END_TABLE@@ expected");

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

         if Is_Stmt (If_Token) or else Is_Stmt (Elsif_Token) then
            T := new Node (If_Stmt);

            T.Line := Line;

            T.Cond   := Expr.Parse (Get_All_Parameters);
            T.N_True := Parse (Parse_If);

            if Is_Stmt (End_If_Token) then
               T.N_False := null;

            elsif Is_Stmt (Elsif_Token) then
               T.N_False := Parse (Parse_Elsif);

            elsif EOF then
               Fatal_Error ("EOF found, @@END_IF@@ expected");

            else

               T.N_False := Parse (Parse_Else);
            end if;

            T.Next := Parse (Mode);

            return T;

         elsif Is_Stmt (Table_Token) then
            T := new Node (Table_Stmt);

            T.Line := Line;

            T.Terminate_Sections
              := Get_First_Parameter = Terminate_Sections_Token;

            T.Sections := Parse (Parse_Section);
            T.Next     := Parse (Mode);

            return T;

         elsif Is_Stmt (Include_Token) then
            T := new Node (Include_Stmt);

            T.Line := Line;

            begin
               T.File
                 := Load (Build_Include_Pathname (Get_First_Parameter),
                          Cached, True);
            exception
               when others =>
                  --  Error while parsing the include file, record this
                  --  error. Let the parser exit properly from the recursion
                  --  to be able to release properly the memory before
                  --  raising an exception.

                  Error_Include_Filename := Get_First_Parameter;
                  Free (T);
                  return null;
            end;

            --  Now we must replace the include parameters (if present) into
            --  the included file tree.

            Replace_Include_Variables (T.File, Get_All_Parameters);

            I_File := new Node'(Include_Stmt, I_File, Line, T.File);

            T.Next := Parse (Mode);

            return T;

         else
            declare
               Root, N : Tree;
            begin
               loop
                  N := new Node (Text);

                  if Root = null then
                     Root := N;
                  else
                     T.Next := N;
                  end if;

                  T      := N;
                  T.Line := Line;

                  if Input.LF_Terminated (File)
                    and then (not Input.End_Of_File (File)
                                or else Include_File)
                  then
                     --  Add a LF is the read line with terminated by a LF. Do
                     --  not add this LF if we reach the end of file except for
                     --  included files.

                     T.Text := Data.Parse (Buffer (1 .. Last) & ASCII.LF);
                  else
                     T.Text := Data.Parse (Buffer (1 .. Last));
                  end if;

                  if Get_Next_Line then
                     --  Nothing more, returns the result now.
                     return Root;
                  end if;

                  --  If this is a statement just call the parsing routine

                  if Is_Stmt (If_Token)
                    or else Is_Stmt (ElsIf_Token)
                    or else Is_Stmt (Else_Token)
                    or else Is_Stmt (End_If_Token)
                    or else Is_Stmt (Include_Token)
                    or else Is_Stmt (Table_Token)
                    or else Is_Stmt (Section_Token)
                    or else Is_Stmt (End_Table_Token)
                  then
                     T.Next := Parse (Mode, No_Read => True);
                     return Root;
                  end if;
               end loop;

               return Root;
            end;
         end if;
      end Parse;

      -------------------------------
      -- Replace_Include_Variables --
      -------------------------------

      procedure Replace_Include_Variables
        (File      : in out Static_Tree;
         Variables : in     String)
      is
         procedure Replace (T : in out Tree);
         --  Recursive routine to parse the tree for all Data.Tree node

         procedure Replace (T : in out Data.Tree);
         --  Recursive routine that replace all numeric variables by the
         --  corresponding parameter in Variables.

         function Get_Variable (Tag : in String) return String;
         --  Returns the variable name for the include tag Tag. Tag is a
         --  numeric value and represent the Nth include parameter.

         function Is_Number (Name : in String) return Boolean;
         --  Returns True if Name is an include tag variable ($<n>)

         ------------------
         -- Get_Variable --
         ------------------

         function Get_Variable (Tag : in String) return String is
            T : constant Natural
              := Natural'Value (Tag (Tag'First + 1 .. Tag'Last));
            S : Natural := Variables'First;
            E : Natural;
            K : Natural := 0;
         begin
            loop
               if Variables (S) = '"' then
                  --  Search for the ending quote

                  E := Strings.Fixed.Index
                    (Variables (S + 1 .. Variables'Last), """");

                  if E = 0 then
                     Fatal_Error ("Missing quote");
                  else
                     E := E + 1;
                  end if;

               else
                  --  Search for the next separator

                  E := Strings.Fixed.Index
                    (Variables (S .. Variables'Last), Blank);
               end if;

               if E = 0 and then K /= T then
                  --  Not found, return the original tag name
                  return To_String (Begin_Tag) & Tag & To_String (End_Tag);

               elsif K = T then
                  --  We have found the right variable

                  if E = 0 then
                     E := Variables'Last;
                  else
                     E := E - 1;
                  end if;

                  --  Always return the variable or value unquoted

                  if Variables (S) = '"' then
                     return Variables (S + 1  .. E - 1);
                  else
                     return Variables (S .. E);
                  end if;

               else
                  --  Set the new start

                  S := E;

                  S := Strings.Fixed.Index
                    (Variables (S .. Variables'Last), Blank, Strings.Outside);

                  if S = 0 then
                     --  No more values, return the original tag name
                     return To_String (Begin_Tag) & Tag & To_String (End_Tag);
                  end if;
               end if;

               K := K + 1;
            end loop;
         end Get_Variable;

         ---------------
         -- Is_Number --
         ---------------

         function Is_Number (Name : in String) return Boolean is
         begin
            return Name'Length > 1
              and then Name (Name'First) = '$'
              and then Strings.Fixed.Count
                         (Name, Strings.Maps.Constants.Decimal_Digit_Set)
                       = Name'Length - 1;
         end Is_Number;

         -------------
         -- Replace --
         -------------

         procedure Replace (T : in out Data.Tree) is

            use type Data.NKind;
            use type Data.Tree;
            use type Filter.Set_Access;

            procedure Free is
               new Ada.Unchecked_Deallocation (Data.Node, Data.Tree);

            Old : Data.Tree := T;

         begin
            if T /= null then
               if T.Kind = Data.Var then
                  if Is_Number (To_String (T.Var.Name)) then
                     --  Here we have an include variable name, replace it

                     T := Data.Parse (Get_Variable (To_String (T.Var.Name)));
                     T.Next := Old.Next;

                     case T.Kind is
                        when Data.Var =>
                           --  The new node is also a variable, inherit all the
                           --  filters and attribute
                           T.Var.Filters := Old.Var.Filters;
                           T.Var.Attr    := Old.Var.Attr;

                        when Data.Text =>
                           --  The new node is a value, apply filters if the
                           --  previous node had some.

                           if Old.Var.Filters /= null then
                              T.Value := To_Unbounded_String
                                (Translate (Old.Var, To_String (T.Value)));
                           end if;

                           --  Free filters
                           Release (Old.Var);
                     end case;

                     --  Free only node
                     Free (Old);
                  end if;
               end if;

               Replace (T.Next);
            end if;
         end Replace;

         -------------
         -- Replace --
         -------------

         procedure Replace (T : in out Expr.Tree) is

            use type Expr.NKind;
            use type Expr.Tree;
            use type Filter.Set_Access;

            procedure Free is
               new Ada.Unchecked_Deallocation (Expr.Node, Expr.Tree);

            Old : Expr.Tree := T;

         begin
            if T /= null then

               case T.Kind is
                  when Expr.Var =>
                     if Is_Number (To_String (T.Var.Name)) then
                        --  Here we have an include variable name, replace it

                        declare
                           New_Value : constant String
                             := Get_Variable (To_String (T.Var.Name));
                        begin
                           if Strings.Fixed.Index (New_Value, " ") = 0 then
                              T := Expr.Parse (New_Value);

                           else
                              --  There is some spaces in the new value,
                              --  this can't be a variable so it is a value
                              --  with multiple word, quote it to ensure a
                              --  correct parsing.

                              T := Expr.Parse ('"' & New_Value & '"');
                           end if;
                        end;

                        case T.Kind is
                           when Expr.Var =>
                              --  The new node is also a variable, inherit all
                              --  the filters.
                              T.Var.Filters := Old.Var.Filters;
                              T.Var.Attr    := Old.Var.Attr;

                           when Expr.Value =>
                              --  The new node is a value, apply filters if the
                              --  previous node had some.

                              if Old.Var.Filters /= null then
                                 T.V := To_Unbounded_String
                                   (Translate (Old.Var, To_String (T.V)));
                              end if;

                              --  Free filters
                              Release (Old.Var);

                           when Expr.Op | Expr.U_Op =>
                              --  Should never happen
                              Fatal_Error
                                ("Var or Value node kind expected,"
                                   & " Op or U_Op found ");
                        end case;

                        Free (Old);
                     end if;

                  when Expr.Op =>
                    Replace (T.Left);
                    Replace (T.Right);

                  when Expr.U_Op =>
                     Replace (T.Next);

                  when Expr.Value =>
                     null;

               end case;
            end if;
         end Replace;

         -------------
         -- Replace --
         -------------

         procedure Replace (T : in out Tree) is
            use type Tree;
         begin
            if T /= null then
               case T.Kind is
                  when Text =>
                     Replace (T.Text);

                  when If_Stmt =>
                     Replace (T.Cond);
                     Replace (T.N_True);
                     Replace (T.N_False);

                  when Table_Stmt =>
                     Replace (T.Sections);

                  when Include_Stmt =>
                     Replace (T.File.C_Info);

                  when Section_Stmt =>
                     Replace (T.N_Section);

                  when Info | C_Info =>
                     null;
               end case;

               Replace (T.Next);
            end if;
         end Replace;

      begin
         Replace (File.C_Info);
      end Replace_Include_Variables;

      T     : Static_Tree;
      New_T : Tree;
      Old   : Tree;

   begin
      if Cached then
         Cached_Files.Prot.Get (Filename, Load => True, Result => T);

         if T.Info /= null then
            pragma Assert (T.C_Info /= null);
            return T;
         end if;
      end if;

      Input.Open (File, Filename, Form => "shared=no");

      New_T := Parse (Parse_Std);

      Input.Close (File);

      --  T is the tree file, add two nodes (Info and C_Info) in front of the
      --  tree.

      --  Add second node (cache info)

      Old := new Node'(C_Info, New_T, 0, False, 1);

      --  Add first node (info about tree)

      New_T := new Node'(Info,
                         Old,
                         0,
                         To_Unbounded_String (Filename),
                         GNAT.OS_Lib.File_Time_Stamp (Filename),
                         I_File,
                         1);

      if Error_Include_Filename /= Null_Unbounded_String then
         --  An include filename was not found, release the memory now and
         --  raise a fatal error.

         Release (New_T);
         Fatal_Error
           (To_String (Error_Include_Filename) & " include file missing");
      end if;

      if Cached then
         Cached_Files.Prot.Add (Filename, New_T, Old);
         pragma Assert (Old /= null);
      end if;

      return Static_Tree'(New_T, Old);

   exception
      when E : Internal_Error =>
         Fatal_Error (Exceptions.Exception_Message (E));
   end Load;

   ----------------
   -- Print_Tree --
   ----------------

   procedure Print_Tree (T : in Tree; Level : in Natural := 0) is separate;

   ----------------
   -- Print_Tree --
   ----------------

   procedure Print_Tree (Filename : in String) is
      T : Static_Tree;
   begin
      T := Load (Filename);
      Print_Tree (T.Info);
      Release (T.Info);
   end Print_Tree;

   -----------
   -- Parse --
   -----------

   function Parse
     (Filename          : in String;
      Translations      : in Translate_Table := No_Translation;
      Cached            : in Boolean         := False;
      Keep_Unknown_Tags : in Boolean         := False)
      return String is
   begin
      return To_String
        (Parse (Filename, Translations, Cached, Keep_Unknown_Tags));
   end Parse;

   -----------
   -- Parse --
   -----------

   function Parse
     (Filename          : in String;
      Translations      : in Translate_Table := No_Translation;
      Cached            : in Boolean         := False;
      Keep_Unknown_Tags : in Boolean         := False)
      return Unbounded_String
   is
      type Table_State is record
         I, J           : Natural;
         Max_Lines      : Natural;
         Max_Expand     : Natural;
         Table_Level    : Natural;
         Section_Number : Natural;
      end record;

      Empty_State : constant Table_State := (0, 0, 0, 0, 0, 0);

      Results : Unbounded_String := Null_Unbounded_String;

      Now     : Calendar.Time;

      procedure Analyze
        (T     : in Tree;
         State : in Table_State);
      --  Parse T and build results file. State is needed for Vector_Tag and
      --  Matrix_Tag expansion.

      -------------
      -- Analyze --
      -------------

      procedure Analyze
        (T     : in Tree;
         State : in Table_State)
      is

         function Analyze (E : in Expr.Tree) return String;
         --  Analyse the expression tree and returns the result as a boolean
         --  The conditional expression must be equal to either TRUE or
         --  FALSE. Note that a string is True if it is equal to string "TRUE"
         --  and False otherwise.

         procedure Analyze (D : in Data.Tree);
         --  Analyse the data tree and replace all variables by the
         --  correspinding value specified in Translations. This procedure
         --  catenate the result into Results variable.

         procedure Get_Max
           (T          : in     Tree;
            Max_Lines  :    out Natural;
            Max_Expand :    out Natural);
         --  Returns the maximum number of lines (Max_Lines) into the
         --  table. This correspond to the length of the shortest vector tag
         --  into the table or the shortest number of lines in sub-table
         --  matrix tag.
         --  Returns also the number of time the table will be expanded
         --  (Max_Expand), this is equal to Max_Lines + offset to terminate
         --  the sections.

         function Is_True (Str : in String) return Boolean;
         --  Return True if Str is one of "TRUE", "OUI", the case beeing not
         --  case sensitive.

         function Translate (Var : in Tag) return String;
         --  Translate Tag variable using Translation table and apply all
         --  Filters and Atribute recorded for this variable.

         ---------------
         -- Translate --
         ---------------

         function Translate (Var : in Tag) return String is

            function Vect_List (A : in Association) return String;
            --  Returns the Vector_Tag for the Association as a String, each
            --  value is separated by the given separator.

            function Vect_Size (A : in Association) return String;
            pragma Inline (Vect_Size);
            --  Returns the number of items into the Vector_Tag

            function Mat_List (A : in Association) return String;
            --  Returns the Matrix_Tag as a string. If Matrix_Tag is not into
            --  a table, each Vector_Tag is convected using Vect_List and a LF
            --  is inserted between each rows. If the Matrix_Tag is into a
            --  table of level 1, it returns only the Vector_Tag (converted
            --  using Vect_List) for the current table line.

            function Mat_Line (A : in Association) return String;
            pragma Inline (Mat_Line);
            --  Returns the number of line (vector) into the matrix

            function Mat_Min_Column (A : in Association) return String;
            pragma Inline (Mat_Line);
            --  Returns the size of the smallest vector

            function Mat_Max_Column (A : in Association) return String;
            pragma Inline (Mat_Line);
            --  Returns the size of the largest vector

            ---------------
            -- Vect_List --
            ---------------

            function Vect_List (A : in Association) return String is
               Result : Unbounded_String;
               P      : Vector_Tag_Node_Access := A.Vect_Value.Head;
            begin
               if P = null then
                  return "";
               else
                  Result := P.Value;
                  for K in 2 .. A.Vect_Value.Count loop
                     P := P.Next;
                     Append (Result, A.Separator & P.Value);
                  end loop;

                  return To_String (Result);
               end if;
            end Vect_List;

            ---------------
            -- Vect_Size --
            ---------------

            function Vect_Size (A : in Association) return String is
            begin
               return Image (A.Vect_Value.Count);
            end Vect_Size;

            --------------
            -- Mat_List --
            --------------

            function Mat_List (A : in Association) return String is
               Result : Unbounded_String;
               P      : Matrix_Tag_Node_Access := A.Mat_Value.M.Head;

               procedure Add_Vector (V : in Vector_Tag);
               --  Add V Vector_Tag representation into Result variable.

               ----------------
               -- Add_Vector --
               ----------------

               procedure Add_Vector (V : in Vector_Tag) is
                  P : Vector_Tag_Node_Access := V.Head;
               begin
                  --  Check that vector is not empty
                  if P /= null then
                     Result := Result & P.Value;

                     for K in 2 .. V.Count loop
                        P := P.Next;
                        Append (Result, A.Column_Separator & P.Value);
                     end loop;
                  end if;
               end Add_Vector;

            begin
               if State.Table_Level = 0 then
                  --  A Matrix outside a table statement.

                  loop
                     Add_Vector (P.Vect);
                     P := P.Next;

                     exit when P = null;

                     Append (Result, ASCII.LF);
                  end loop;

               else
                  if not (State.J > A.Mat_Value.M.Count) then
                     Add_Vector (Vector (A.Mat_Value, State.J));
                  end if;
               end if;

               return To_String (Result);
            end Mat_List;

            --------------
            -- Mat_Line --
            --------------

            function Mat_Line (A : in Association) return String is
            begin
               return Image (A.Mat_Value.M.Count);
            end Mat_Line;

            --------------------
            -- Mat_Min_Column --
            --------------------

            function Mat_Min_Column (A : in Association) return String is
            begin
               return Image (A.Mat_Value.M.Min);
            end Mat_Min_Column;

            --------------------
            -- Mat_Max_Column --
            --------------------

            function Mat_Max_Column (A : in Association) return String is
            begin
               return Image (A.Mat_Value.M.Max);
            end Mat_Max_Column;

         begin
            for K in Translations'Range loop
               if Var.Name = Translations (K).Variable then

                  declare
                     Tk : constant Association := Translations (K);
                  begin
                     case Tk.Kind is

                        when Std =>
                           if Var.Attr = Nil then
                              return Translate (Var, To_String (Tk.Value));
                           else
                              Exceptions.Raise_Exception
                                (Template_Error'Identity,
                                 "Attribute not valid on a discrete tag");
                           end if;

                        when Vect =>
                           if Var.Attr = Length then
                              --  'Length on a vector
                              return Translate (Var, Vect_Size (Tk));

                           elsif Var.Attr /= Nil then
                              Exceptions.Raise_Exception
                                (Template_Error'Identity,
                                 "This attribute is not valid for a "
                                   & "vector tag");

                           elsif State.Table_Level = 0 then
                              --  This is a vector tag (outside of a
                              --  table tag statement), we display it as
                              --  a list separated by the specified
                              --  separator.
                              return Translate (Var, Vect_List (Tk));

                           else
                              declare
                                 Result : Unbounded_String;
                                 Found  : Boolean;
                              begin
                                 Field (Tk.Vect_Value, State.J, Result, Found);
                                 return Translate (Var, To_String (Result));
                              end;
                           end if;

                        when Matrix =>
                           if Var.Attr = Line then
                              --  'Line on a matrix
                              return Translate (Var, Mat_Line (Tk));

                           elsif Var.Attr = Min_Column then
                              --  'Min_Column on a matrix
                              return Translate (Var, Mat_Min_Column (Tk));

                           elsif Var.Attr = Max_Column then
                              --  'Max_Column on a matrix
                              return Translate (Var, Mat_Max_Column (Tk));

                           elsif Var.Attr /= Nil then
                              Exceptions.Raise_Exception
                                (Template_Error'Identity,
                                 "This attribute is not valid for a "
                                   & "matrix tag");

                           elsif State.Table_Level in 0 .. 1 then
                              --  This is a matrix tag (outside of a
                              --  level 2 table tag statement), convert
                              --  it using Mat_List.
                              return Translate (Var, Mat_List (Tk));

                           else
                              declare
                                 Result : Unbounded_String;
                                 Found  : Boolean;
                              begin
                                 Field (Tk.Mat_Value, State.I, State.J,
                                        Result, Found);
                                 return Translate (Var, To_String (Result));
                              end;
                           end if;
                     end case;
                  end;
               end if;
            end loop;

            --  Check now for an internal tag

            declare
               T_Name : constant String := To_String (Var.Name);
            begin

               if T_Name = "UP_TABLE_LINE" then
                  return Translate
                    (Var,
                     Fixed.Trim (Positive'Image (State.I), Strings.Left));

               elsif T_Name = "TABLE_LINE" then
                  return Translate
                    (Var,
                     Fixed.Trim (Positive'Image (State.J), Strings.Left));

               elsif T_Name = "NUMBER_LINE" then
                  return Translate
                    (Var,
                     Fixed.Trim (Positive'Image (State.Max_Lines),
                                 Strings.Left));

               elsif T_Name = "TABLE_LEVEL" then
                  return Translate
                    (Var,
                     Fixed.Trim (Positive'Image (State.Table_Level),
                                 Strings.Left));

               elsif T_Name = "YEAR" then
                  return Translate
                    (Var, GNAT.Calendar.Time_IO.Image (Now, "%Y"));

               elsif T_Name = "MONTH" then
                  return Translate
                    (Var, GNAT.Calendar.Time_IO.Image (Now, "%m"));

               elsif T_Name = "DAY" then
                  return Translate
                    (Var, GNAT.Calendar.Time_IO.Image (Now, "%d"));

               elsif T_Name = "HOUR" then
                  return Translate
                    (Var, GNAT.Calendar.Time_IO.Image (Now, "%H"));

               elsif T_Name = "MINUTE" then
                  return Translate
                    (Var, GNAT.Calendar.Time_IO.Image (Now, "%M"));

               elsif T_Name = "SECOND" then
                  return Translate
                    (Var, GNAT.Calendar.Time_IO.Image (Now, "%S"));

               elsif T_Name = "MONTH_NAME" then
                  return Translate
                    (Var, GNAT.Calendar.Time_IO.Image (Now, "%B"));

               elsif T_Name = "DAY_NAME" then
                  return Translate
                    (Var, GNAT.Calendar.Time_IO.Image (Now, "%A"));
               end if;
            end;

            --  The tag was not found in the Translation_Table, we either
            --  returns the empty string or we keep the tag as is.

            if Keep_Unknown_Tags then
               return To_String (Begin_Tag & Var.Name & End_Tag);
            else
               return Translate (Var, "");
            end if;
         end Translate;

         -------------
         -- Analyze --
         -------------

         procedure Analyze (D : in Data.Tree) is
            use type Data.Tree;

            T : Data.Tree := D;

         begin
            while T /= null loop

               case T.Kind is

                  when Data.Text =>
                     Append (Results, T.Value);

                  when Data.Var =>
                     Append (Results, Translate (T.Var));

               end case;

               T := T.Next;
            end loop;
         end Analyze;

         -------------
         -- Analyze --
         -------------

         function Analyze (E : in Expr.Tree) return String is

            type Ops_Fct is access function (L, R : in String) return String;

            function F_And  (L, R : in String) return String;
            function F_Or   (L, R : in String) return String;
            function F_Xor  (L, R : in String) return String;
            function F_Sup  (L, R : in String) return String;
            function F_Esup (L, R : in String) return String;
            function F_Einf (L, R : in String) return String;
            function F_Inf  (L, R : in String) return String;
            function F_Equ  (L, R : in String) return String;
            function F_Diff (L, R : in String) return String;

            type U_Ops_Fct is access function (N : in String) return String;

            function F_Not (N : in String) return String;

            -----------
            -- F_And --
            -----------

            function F_And (L, R : in String) return String is
            begin
               if Is_True (L) and Is_True (R) then
                  return "TRUE";
               else
                  return "FALSE";
               end if;
            end F_And;

            ------------
            -- F_Diff --
            ------------

            function F_Diff (L, R : in String) return String is
            begin
               if L /= R then
                  return "TRUE";
               else
                  return "FALSE";
               end if;
            end F_Diff;

            ------------
            -- F_Einf --
            ------------

            function F_Einf (L, R : in String) return String is
            begin
               if Integer'Value (L) <= Integer'Value (R) then
                  return "TRUE";
               else
                  return "FALSE";
               end if;
            exception
               when others =>
                  if L <= R then
                     return "TRUE";
                  else
                     return "FALSE";
                  end if;
            end F_Einf;

            -----------
            -- F_Equ --
            -----------

            function F_Equ (L, R : in String) return String is
            begin
               if L = R then
                  return "TRUE";
               else
                  return "FALSE";
               end if;
            end F_Equ;

            ------------
            -- F_Esup --
            ------------

            function F_Esup (L, R : in String) return String is
            begin
               if Integer'Value (L) >= Integer'Value (R) then
                  return "TRUE";
               else
                  return "FALSE";
               end if;
            exception
               when others =>
                  if L >= R then
                     return "TRUE";
                  else
                     return "FALSE";
                  end if;
            end F_Esup;

            -----------
            -- F_Inf --
            -----------

            function F_Inf (L, R : in String) return String is
            begin
               if Integer'Value (L) < Integer'Value (R) then
                  return "TRUE";
               else
                  return "FALSE";
               end if;
            exception
               when others =>
                  if L < R then
                     return "TRUE";
                  else
                     return "FALSE";
                  end if;
            end F_Inf;

            -----------
            -- F_Not --
            -----------

            function F_Not (N : in String) return String is
            begin
               if Is_True (N) then
                  return "FALSE";
               else
                  return "TRUE";
               end if;
            end F_Not;

            ----------
            -- F_Or --
            ----------

            function F_Or (L, R : in String) return String is
            begin
               if Is_True (L) or Is_True (R) then
                  return "TRUE";
               else
                  return "FALSE";
               end if;
            end F_Or;

            -----------
            -- F_Sup --
            -----------

            function F_Sup (L, R : in String) return String is
            begin
               if Integer'Value (L) > Integer'Value (R) then
                  return "TRUE";
               else
                  return "FALSE";
               end if;
            exception
               when others =>
                  if L > R then
                     return "TRUE";
                  else
                     return "FALSE";
                  end if;
            end F_Sup;

            -----------
            -- F_Xor --
            -----------

            function F_Xor (L, R : in String) return String is
            begin
               if Is_True (L) xor Is_True (R) then
                  return "TRUE";
               else
                  return "FALSE";
               end if;
            end F_Xor;

            Op_Table : constant array (Expr.Ops) of Ops_Fct
              := (Expr.O_And   => F_And'Access,
                  Expr.O_Or    => F_Or'Access,
                  Expr.O_Xor   => F_Xor'Access,
                  Expr.O_Sup   => F_Sup'Access,
                  Expr.O_Inf   => F_Inf'Access,
                  Expr.O_Esup  => F_Esup'Access,
                  Expr.O_Einf  => F_Einf'Access,
                  Expr.O_Equal => F_Equ'Access,
                  Expr.O_Diff  => F_Diff'Access);

            U_Op_Table : constant array (Expr.U_Ops) of U_Ops_Fct
              := (Expr.O_Not   => F_Not'Access);

         begin
            case E.Kind is
               when Expr.Value =>
                     return To_String (E.V);

               when Expr.Var =>
                  return Translate (E.Var);

               when Expr.Op =>
                  return Op_Table (E.O) (Analyze (E.Left), Analyze (E.Right));

               when Expr.U_Op =>
                  return U_Op_Table (E.U_O) (Analyze (E.Next));
            end case;
         end Analyze;

         -------------
         -- Get_Max --
         -------------

         procedure Get_Max
           (T          : in     Tree;
            Max_Lines  :    out Natural;
            Max_Expand :    out Natural)
         is

            function Get_Max_Lines
              (T : in Tree;
               N : in Positive)
               return Natural;
            --  Recursivelly descend the tree and compute the max lines that
            --  will be displayed into the table.

            function Count_Section return Natural;
            --  Returns the number of section into table T;

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

            function Get_Max_Lines
              (T : in Tree;
               N : in Positive)
               return Natural
            is

               function Check (T : in Data.Tree) return Natural;
               --  Returns the length of the largest vector tag found on the
               --  subtree.

               -----------
               -- Check --
               -----------

               function Check (T : in Data.Tree) return Natural is
                  use type Data.Tree;
                  use type Data.NKind;
                  Iteration : Natural := Natural'First;
                  D         : Data.Tree := T;
               begin
                  while D /= null loop

                     if D.Kind = Data.Var and then D.Var.Attr = Nil then

                        for K in Translations'Range loop
                           declare
                              Tk : constant Association := Translations (K);
                           begin
                              if D.Var.Name = Tk.Variable then
                                 if N = 1 then
                                    --  First block level analysed.

                                    if Tk.Kind = Vect then
                                       --  This is a Vector tag into a top
                                       --  level table statement. The number
                                       --  of iterations for this table
                                       --  statement correspond to the number
                                       --  of item into the vector.
                                       Iteration :=
                                         Natural'Max (Iteration,
                                                      Size (Tk.Vect_Value));

                                    elsif Tk.Kind = Matrix then

                                       if State.Table_Level = 0 then
                                          --  This is Matrix tag into a top
                                          --  level table statement. The
                                          --  number of iterations for this
                                          --  table statement correspond to
                                          --  the number of vector into the
                                          --  table.
                                          Iteration :=
                                            Natural'Max (Iteration,
                                                         Size (Tk.Mat_Value));
                                       else
                                          --  This is Matrix tag into an
                                          --  embbeded table statement (table
                                          --  statement into a table
                                          --  statement). The number of
                                          --  iterations for this table
                                          --  statement correspond to the
                                          --  largest number of items in the
                                          --  Matrix tag's vectors.
                                          Iteration := Tk.Mat_Value.M.Max;
                                       end if;
                                    end if;

                                 elsif N = 2 then
                                    --  Second block level analysed.

                                    if Tk.Kind = Matrix then
                                       --  This is a Matrix tag into an
                                       --  embedded table statement (table
                                       --  statement into a table statement)
                                       --  analysed at the second block
                                       --  level. This is to report the number
                                       --  of iterations for upper level table
                                       --  statement. This number of
                                       --  iterations correspond to the
                                       --  smallest number of vectors into the
                                       --  table.
                                       Iteration :=
                                         Natural'Max (Iteration,
                                                      Size (Tk.Mat_Value));
                                    end if;
                                 end if;
                              end if;
                           end;
                        end loop;
                     end if;
                     D := D.Next;
                  end loop;

                  return Iteration;
               end Check;

            begin
               if T = null then
                  return Natural'First;
               end if;

               case T.Kind is
                  when Info | C_Info =>
                     return Get_Max_Lines (T.Next, N);

                  when Text =>
                     return Natural'Max (Check (T.Text),
                                         Get_Max_Lines (T.Next, N));
                  when If_Stmt =>
                     return Natural'Max
                       (Natural'Max (Get_Max_Lines (T.N_True, N),
                                     Get_Max_Lines (T.N_False, N)),
                        Get_Max_Lines (T.Next, N));

                  when Table_Stmt =>
                     if N = 1 then
                        return Natural'Max (Get_Max_Lines (T.Sections, N + 1),
                                            Get_Max_Lines (T.Next, N));
                     else
                        return Natural'First;
                     end if;

                  when Section_Stmt =>
                     return Natural'Max (Get_Max_Lines (T.Next, N),
                                         Get_Max_Lines (T.N_Section, N));

                  when Include_Stmt =>
                     return Natural'Max (Get_Max_Lines (T.File.Info, N),
                                         Get_Max_Lines (T.Next, N));
               end case;
            end Get_Max_Lines;

            Result : Natural := Get_Max_Lines (T.Sections, 1);

         begin
            pragma Assert (T.Kind = Table_Stmt);

            Max_Lines := Result;

            if T.Terminate_Sections then

               declare
                  N_Section : constant Positive := Count_Section;
               begin
                  if Result mod N_Section /= 0 then
                     Result := Result + N_Section - (Result mod N_Section);
                  end if;
               end;
            end if;

            Max_Expand := Result;
         end Get_Max;

         -------------
         -- Is_True --
         -------------

         function Is_True (Str : in String) return Boolean is
            L_Str : constant String := Characters.Handling.To_Upper (Str);
         begin
            return L_Str = "TRUE";
         end Is_True;

      begin
         if T = null then
            return;
         end if;

         case T.Kind is

            when Info | C_Info =>
               Analyze (T.Next, State);

            when Text =>
               begin
                  Analyze (T.Text);
               exception
                  when E : others =>
                     Exceptions.Raise_Exception
                       (Template_Error'Identity,
                        "In " & Filename
                        & " at line" & Natural'Image (T.Line) & ", "
                        & Exceptions.Exception_Message (E) & '.');
               end;

               Analyze (T.Next, State);

            when If_Stmt  =>
               if Analyze (T.Cond) = "TRUE" then
                  Analyze (T.N_True, State);
               else
                  Analyze (T.N_False, State);
               end if;

               Analyze (T.Next, State);

            when Table_Stmt =>
               declare
                  Max_Lines, Max_Expand : Natural;
               begin
                  Get_Max (T, Max_Lines, Max_Expand);

                  Analyze (T.Sections,
                           Table_State'(State.I, State.J,
                                        Max_Lines, Max_Expand,
                                        State.Table_Level + 1,
                                        State.Section_Number + 1));
               end;

               Analyze (T.Next, State);

            when Section_Stmt =>
               declare
                  First_Section : Tree := T;
                  Current       : Tree := T;
                  Section       : Positive := 1;
               begin

                  for K in 1 .. State.Max_Expand loop
                     Analyze (Current.Next,
                              Table_State'(State.J,
                                           K,
                                           State.Max_Lines, State.Max_Expand,
                                           State.Table_Level, Section));

                     Current := Current.N_Section;
                     Section := Section + 1;

                     if Current = null then
                        Current := First_Section;
                        Section := 1;
                     end if;
                  end loop;
               end;

            when Include_Stmt =>
               Analyze (T.File.Info, State);

               Analyze (T.Next, State);

         end case;
      end Analyze;

      T : Static_Tree;

   begin
      T := Load (Filename, Cached);

      Now := Ada.Calendar.Clock;
      --  Used for the time related variable

      Analyze (T.C_Info, Empty_State);

      if not Cached then
         Release (T.Info);

      else
         Cached_Files.Prot.Release (T);
      end if;

      return Results;
   end Parse;

   -------------
   -- Release --
   -------------

   procedure Release (T : in out Tree) is
   begin
      if T = null then
         return;
      end if;

      case T.Kind is
         when Info =>
            declare
               I : Tree := T.I_File;
               O : Tree;
            begin
               while I /= null loop
                  O := I;
                  I := I.Next;
                  Free (O);
               end loop;
            end;

            Release (T.Next);
            Free (T);

         when C_Info =>
            Release (T.Next);
            Free (T);

         when Text =>
            Data.Release (T.Text);
            Release (T.Next);
            Free (T);

         when If_Stmt  =>
            Expr.Release (T.Cond);
            Release (T.N_True);
            Release (T.N_False);
            Release (T.Next);
            Free (T);

         when Table_Stmt =>
            Release (T.Sections);
            Release (T.Next);
            Free (T);

         when Section_Stmt =>
            Release (T.Next);
            Release (T.N_Section);
            Free (T);

         when Include_Stmt =>
            T.File.Info.Ref := T.File.Info.Ref - 1;

            if T.File.Info.Ref = 0 then
               --  No more reference to this include file we release it.
               Release (T.File.Info);
            end if;

            Release (T.Next);
            Free (T);
      end case;
   end Release;

   ------------------------
   -- Set_Tag_Separators --
   ------------------------

   procedure Set_Tag_Separators
     (Start_With : in String := Default_Begin_Tag;
      Stop_With  : in String := Default_End_Tag) is
   begin
      Begin_Tag := To_Unbounded_String (Start_With);
      End_Tag   := To_Unbounded_String (Stop_With);
   end Set_Tag_Separators;

   ---------------
   -- Translate --
   ---------------

   function Translate
     (Template     : in String;
      Translations : in Translate_Table := No_Translation)
      return String
   is
      T : Data.Tree := Data.Parse (Template);
      P : Data.Tree := T;

      Results : Unbounded_String;

      function Translate (Var : in Tag) return String;
      --  Returns translation for Var.

      ---------------
      -- Translate --
      ---------------

      function Translate (Var : in Tag) return String is
      begin
         for K in Translations'Range loop
            if Var.Name = Translations (K).Variable then
               declare
                  Tk : constant Association := Translations (K);
               begin
                  case Tk.Kind is

                     when Std =>
                        return Translate (Var, To_String (Tk.Value));

                     when others =>
                        return "";
                  end case;
               end;
            end if;
         end loop;

         return "";
      end Translate;

      use type Data.Tree;

   begin
      while P /= null loop
         case P.Kind is
            when Data.Text =>
               Append (Results, P.Value);

            when Data.Var =>
               Append (Results, Translate (P.Var));
         end case;

         P := P.Next;
      end loop;

      Data.Release (T);

      return To_String (Results);
   end Translate;

end Templates_Parser;
