------------------------------------------------------------------------------
--                             Templates Parser                             --
--                                                                          --
--                        Copyright (C) 1999 - 2004                         --
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
with Ada.Strings.Fixed;
with Ada.Strings.Maps.Constants;
with Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;

with GNAT.Calendar.Time_IO;
with AWS.OS_Lib;
with GNAT.Regpat;

with Templates_Parser.Input;

package body Templates_Parser is

   use Ada;
   use Ada.Exceptions;
   use Ada.Strings;

   Internal_Error : exception;

   Blank : constant Maps.Character_Set := Maps.To_Set (' ' & ASCII.HT);

   Max_Include_Parameters : constant := 20;
   --  Maximum number of include parameters handled by this implementation

   function No_Quote (Str : in String) return String;
   --  Removes quotes around Str. If Str (Str'First) and Str (Str'Last)
   --  are quotes return Str (Str'First + 1 ..  Str'Last - 1) otherwise
   --  return Str as-is.

   procedure Free is new Ada.Unchecked_Deallocation (Integer, Integer_Access);

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

   Table_Token              : constant String := "@@TABLE@@";
   Terminate_Sections_Token : constant String := "@@TERMINATE_SECTIONS@@";
   Begin_Token              : constant String := "@@BEGIN@@";
   End_Token                : constant String := "@@END@@";
   Section_Token            : constant String := "@@SECTION@@";
   End_Table_Token          : constant String := "@@END_TABLE@@";
   If_Token                 : constant String := "@@IF@@";
   Elsif_Token              : constant String := "@@ELSIF@@";
   Else_Token               : constant String := "@@ELSE@@";
   End_If_Token             : constant String := "@@END_IF@@";
   Include_Token            : constant String := "@@INCLUDE@@";

   -------------------
   -- Translate_Set --
   -------------------

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Set : in out Translate_Set) is
   begin
      Set.Ref_Count := new Integer'(1);
      Set.Set       := new Containers.Map;
   end Initialize;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Set : in out Translate_Set) is
      procedure Free is
        new Unchecked_Deallocation (Containers.Map, Map_Access);
   begin
      Set.Ref_Count.all := Set.Ref_Count.all - 1;

      if Set.Ref_Count.all = 0 then
         Free (Set.Ref_Count);
         Free (Set.Set);
      end if;
   end Finalize;

   ------------
   -- Adjust --
   ------------

   procedure Adjust (Set : in out Translate_Set) is
   begin
      Set.Ref_Count.all := Set.Ref_Count.all + 1;
   end Adjust;

   Null_Set : Translate_Set;
   --  This is used as default parameter in some local calls

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

         Absolute,
         --  Returns the abosulte value

         Add,
         --  Add the given parameter to the string

         Add_Param,
         --  Add an HTTP parameter to the string, add the '&' parameter
         --  separator if needed.

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

         Del_Param,
         --  Delete an HTTP parameter from the string, removes the '&'
         --  Parameter separator if needed.

         Div,
         --  Divide the given parameter to the string

         Exist,
         --  Returns "TRUE" if var is not empty and "FALSE" otherwise.

         Format_Date,
         --  Returns the date formatted using the format parameter. This
         --  format is following the GNU/date as implemented in
         --  GNAT.Calendar.Time_IO. The date must be in the ISO format
         --  YYYY-MM-DD eventually followed by a space and the time with the
         --  format HH:MM:SS. So the string must be either 10 or 19 characters
         --  long.

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

         Neg,
         --  Change the size of the value

         No_Context,
         --  This is a command filter, it indicates that the variable even
         --  if not found in the translation table will not be looked up into
         --  the context. See spec for a description about Context feature.
         --  This filter just returns the string as-is.

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

         Replace,
         --  Replaces part of the string using a regultar expression. This is
         --  equivalent to the well known "s/<regexp>/<new value>/" sed
         --  command. It replaces only the first match.

         Replace_All,
         --  Idem as above, but replace all matches. This equivalent to the
         --  well known "s/<regexp>/<new value>/g" sed command.

         Replace_Param,
         --  Idem as @_ADD_PARAM(key=value):DEL_PARAM(key):VAR_@

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

      type Parameter_Mode is (Void, Str, Regexp, Regpat, Slice);

      function Parameter (Mode : in Filter.Mode) return Parameter_Mode;
      --  Returns the parameter mode for the given filter.

      type Pattern_Matcher_Access is access GNAT.Regpat.Pattern_Matcher;

      type Parameter_Data (Mode : Parameter_Mode := Void) is record
         case Mode is
            when Void =>
               null;

            when Str =>
               S : Unbounded_String;

            when Regexp =>
               R_Str  : Unbounded_String;
               Regexp : Pattern_Matcher_Access;

            when Regpat =>
               P_Str  : Unbounded_String;
               Regpat : Pattern_Matcher_Access;
               Param  : Unbounded_String;

            when Slice =>
               First : Natural;
               Last  : Natural;
         end case;
      end record;

      No_Parameter : constant Parameter_Data := Parameter_Data'(Mode => Void);

      function Image (P : in Parameter_Data) return String;
      --  Returns parameter string representation

      procedure Release (P : in out Parameter_Data);
      pragma Inline (Release);
      --  Release all memory allocated P

      type Include_Parameters is
        array (0 .. Max_Include_Parameters) of Unbounded_String;
      --  Flat representation of the include parameters. Only the name or the
      --  value of the corresponding parameter is set here, we do not handle
      --  full data tree into the filter parameters.

      No_Include_Parameters : constant Include_Parameters
        := (others => (Null_Unbounded_String));

      type Callback is access function
        (S : in String;
         P : in Parameter_Data     := No_Parameter;
         T : in Translate_Set      := Null_Set;
         I : in Include_Parameters := No_Include_Parameters)
         return String;
      --  P is the filter parameter, no parameter by default. Parameter are
      --  untyped and will be parsed by the filter function if needed.

      type Routine is record
         Handle     : Callback;
         Parameters : Parameter_Data;
      end record;

      type Set is array (Positive range <>) of Routine;
      type Set_Access is access Set;

      procedure Release (S : in out Set);
      --  Release all memory allocated P

      type String_Access is access constant String;

      type Filter_Record is record
         Name   : String_Access;
         Handle : Callback;
      end record;

      --  filter functions, see above.

      procedure Check_Null_Parameter (P : in Parameter_Data);
      --  Raises Template_Error if P is not equal to Null_Parameter.

      function Absolute
        (S : in String;
         P : in Parameter_Data     := No_Parameter;
         T : in Translate_Set      := Null_Set;
         I : in Include_Parameters := No_Include_Parameters)
         return String;

      function Add_Param
        (S : in String;
         P : in Parameter_Data     := No_Parameter;
         T : in Translate_Set      := Null_Set;
         I : in Include_Parameters := No_Include_Parameters)
         return String;

      function BR_2_LF
        (S : in String;
         P : in Parameter_Data     := No_Parameter;
         T : in Translate_Set      := Null_Set;
         I : in Include_Parameters := No_Include_Parameters)
         return String;

      function Capitalize
        (S : in String;
         P : in Parameter_Data     := No_Parameter;
         T : in Translate_Set      := Null_Set;
         I : in Include_Parameters := No_Include_Parameters)
         return String;

      function Clean_Text
        (S : in String;
         P : in Parameter_Data     := No_Parameter;
         T : in Translate_Set      := Null_Set;
         I : in Include_Parameters := No_Include_Parameters)
         return String;

      function Coma_2_Point
        (S : in String;
         P : in Parameter_Data     := No_Parameter;
         T : in Translate_Set      := Null_Set;
         I : in Include_Parameters := No_Include_Parameters)
         return String;

      function Contract
        (S : in String;
         P : in Parameter_Data     := No_Parameter;
         T : in Translate_Set      := Null_Set;
         I : in Include_Parameters := No_Include_Parameters)
         return String;

      function Del_Param
        (S : in String;
         P : in Parameter_Data     := No_Parameter;
         T : in Translate_Set      := Null_Set;
         I : in Include_Parameters := No_Include_Parameters)
         return String;

      function Exist
        (S : in String;
         P : in Parameter_Data     := No_Parameter;
         T : in Translate_Set      := Null_Set;
         I : in Include_Parameters := No_Include_Parameters)
         return String;

      function Format_Date
        (S : in String;
         P : in Parameter_Data     := No_Parameter;
         T : in Translate_Set      := Null_Set;
         I : in Include_Parameters := No_Include_Parameters)
         return String;

      function Format_Number
        (S : in String;
         P : in Parameter_Data     := No_Parameter;
         T : in Translate_Set      := Null_Set;
         I : in Include_Parameters := No_Include_Parameters)
         return String;

      function Is_Empty
        (S : in String;
         P : in Parameter_Data     := No_Parameter;
         T : in Translate_Set      := Null_Set;
         I : in Include_Parameters := No_Include_Parameters)
         return String;

      function LF_2_BR
        (S : in String;
         P : in Parameter_Data     := No_Parameter;
         T : in Translate_Set      := Null_Set;
         I : in Include_Parameters := No_Include_Parameters)
         return String;

      function Lower
        (S : in String;
         P : in Parameter_Data     := No_Parameter;
         T : in Translate_Set      := Null_Set;
         I : in Include_Parameters := No_Include_Parameters)
         return String;

      function Match
        (S : in String;
         P : in Parameter_Data     := No_Parameter;
         T : in Translate_Set      := Null_Set;
         I : in Include_Parameters := No_Include_Parameters)
         return String;

      function Neg
        (S : in String;
         P : in Parameter_Data     := No_Parameter;
         T : in Translate_Set      := Null_Set;
         I : in Include_Parameters := No_Include_Parameters)
         return String;

      function No_Context
        (S : in String;
         P : in Parameter_Data     := No_Parameter;
         T : in Translate_Set      := Null_Set;
         I : in Include_Parameters := No_Include_Parameters)
         return String;

      function No_Digit
        (S : in String;
         P : in Parameter_Data     := No_Parameter;
         T : in Translate_Set      := Null_Set;
         I : in Include_Parameters := No_Include_Parameters)
         return String;

      function No_Letter
        (S : in String;
         P : in Parameter_Data     := No_Parameter;
         T : in Translate_Set      := Null_Set;
         I : in Include_Parameters := No_Include_Parameters)
         return String;

      function No_Space
        (S : in String;
         P : in Parameter_Data     := No_Parameter;
         T : in Translate_Set      := Null_Set;
         I : in Include_Parameters := No_Include_Parameters)
         return String;

      function Oui_Non
        (S : in String;
         P : in Parameter_Data     := No_Parameter;
         T : in Translate_Set      := Null_Set;
         I : in Include_Parameters := No_Include_Parameters)
         return String;

      function Point_2_Coma
        (S : in String;
         P : in Parameter_Data     := No_Parameter;
         T : in Translate_Set      := Null_Set;
         I : in Include_Parameters := No_Include_Parameters)
         return String;

      function Repeat
        (S : in String;
         P : in Parameter_Data     := No_Parameter;
         T : in Translate_Set      := Null_Set;
         I : in Include_Parameters := No_Include_Parameters)
         return String;

      function Replace
        (S : in String;
         P : in Parameter_Data     := No_Parameter;
         T : in Translate_Set      := Null_Set;
         I : in Include_Parameters := No_Include_Parameters)
         return String;

      function Replace_All
        (S : in String;
         P : in Parameter_Data     := No_Parameter;
         T : in Translate_Set      := Null_Set;
         I : in Include_Parameters := No_Include_Parameters)
         return String;

      function Replace_Param
        (S : in String;
         P : in Parameter_Data     := No_Parameter;
         T : in Translate_Set      := Null_Set;
         I : in Include_Parameters := No_Include_Parameters)
         return String;

      function Reverse_Data
        (S : in String;
         P : in Parameter_Data     := No_Parameter;
         T : in Translate_Set      := Null_Set;
         I : in Include_Parameters := No_Include_Parameters)
         return String;

      function Size
        (S : in String;
         P : in Parameter_Data     := No_Parameter;
         T : in Translate_Set      := Null_Set;
         I : in Include_Parameters := No_Include_Parameters)
         return String;

      function Slice
        (S : in String;
         P : in Parameter_Data     := No_Parameter;
         T : in Translate_Set      := Null_Set;
         I : in Include_Parameters := No_Include_Parameters)
         return String;

      function Trim
        (S : in String;
         P : in Parameter_Data     := No_Parameter;
         T : in Translate_Set      := Null_Set;
         I : in Include_Parameters := No_Include_Parameters)
         return String;

      function Upper
        (S : in String;
         P : in Parameter_Data     := No_Parameter;
         T : in Translate_Set      := Null_Set;
         I : in Include_Parameters := No_Include_Parameters)
         return String;

      function Web_Escape
        (S : in String;
         P : in Parameter_Data     := No_Parameter;
         T : in Translate_Set      := Null_Set;
         I : in Include_Parameters := No_Include_Parameters)
         return String;

      function Web_NBSP
        (S : in String;
         P : in Parameter_Data     := No_Parameter;
         T : in Translate_Set      := Null_Set;
         I : in Include_Parameters := No_Include_Parameters)
         return String;

      function Yes_No
        (S : in String;
         P : in Parameter_Data     := No_Parameter;
         T : in Translate_Set      := Null_Set;
         I : in Include_Parameters := No_Include_Parameters)
         return String;

      function Plus
        (S : in String;
         P : in Parameter_Data     := No_Parameter;
         T : in Translate_Set      := Null_Set;
         I : in Include_Parameters := No_Include_Parameters)
         return String;

      function Minus
        (S : in String;
         P : in Parameter_Data     := No_Parameter;
         T : in Translate_Set      := Null_Set;
         I : in Include_Parameters := No_Include_Parameters)
         return String;

      function Divide
        (S : in String;
         P : in Parameter_Data     := No_Parameter;
         T : in Translate_Set      := Null_Set;
         I : in Include_Parameters := No_Include_Parameters)
         return String;

      function Multiply
        (S : in String;
         P : in Parameter_Data     := No_Parameter;
         T : in Translate_Set      := Null_Set;
         I : in Include_Parameters := No_Include_Parameters)
         return String;

      function Modulo
        (S : in String;
         P : in Parameter_Data     := No_Parameter;
         T : in Translate_Set      := Null_Set;
         I : in Include_Parameters := No_Include_Parameters)
         return String;

      function Handle (Name : in String) return Callback;
      --  Returns the filter function for the given filter name

      function Handle (Mode : in Filter.Mode) return Callback;
      --  Returns the filter function for the given filter mode

      function Mode_Value (Name : in String) return Mode;
      --  Returns the Mode for filter named Name. This is the internal
      --  representation for this filter name.

      function Name (Handle : in Callback) return String;
      --  Returns the filter name for the given filter function

      function Is_No_Context (Filters : in Set_Access) return Boolean;
      --  Returns True if Filters contains NO_CONTEXT

      function Is_Number (S : in String) return Boolean;
      pragma Inline (Is_Number);
      --  Returns True if S is a decimal number

   end Filter;

   --------------------
   --  Tags variable --
   --------------------

   type Attribute is (Nil, Length, Line, Min_Column, Max_Column);

   type Tag_Var is record
      Name    : Unbounded_String;
      Filters : Filter.Set_Access;
      Attr    : Attribute := Nil;
      N       : Integer;         -- include variable index
   end record;

   function Is_Include_Variable (T : in Tag_Var) return Boolean;
   --  Returns True if T is an include variable (Name is $<n>)

   function Build (Str : in String) return Tag_Var;
   --  Create a Tag from Str. A tag is composed of a name and a set of
   --  filters.

   function Image (T : in Tag_Var) return String;
   --  Returns string representation for the Tag variable

   function Translate
     (T            : in Tag_Var;
      Value        : in String;
      Translations : in Translate_Set             := Null_Set;
      I_Params     : in Filter.Include_Parameters
                          := Filter.No_Include_Parameters)
      return String;
   --  Returns the result of Value after applying all filters for tag T

   procedure Release (T : in out Tag_Var);
   --  Release all memory associated with Tag

   -----------
   -- Image --
   -----------

   function Image (T : in Tag_Var) return String is
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

   -------------------------
   -- Is_Include_Variable --
   -------------------------

   function Is_Include_Variable (T : in Tag_Var) return Boolean is
   begin
      return T.N /= -1;
   end Is_Include_Variable;

   -----------
   -- Build --
   -----------

   function Build (Str : in String) return Tag_Var is

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

      A_Sep : Natural
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

         use type Filter.Callback;

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

         function Find_Slash (Str : in String) return Natural;
         --  Returns the first slash index in Str, skip espaced slashes

         ----------------
         -- Find_Slash --
         ----------------

         function Find_Slash (Str : in String) return Natural is
            Escaped : Boolean := False;
         begin
            for K in Str'Range loop
               if Str (K) = '\' then
                  Escaped := not Escaped;

               elsif Str (K) = '/' and not Escaped then
                  return K;

               else
                  Escaped := False;
               end if;
            end loop;

            return 0;
         end Find_Slash;

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
                                   new GNAT.Regpat.Pattern_Matcher'
                                     (GNAT.Regpat.Compile (Parameter))));

                     when F.Regpat =>
                        declare
                           K : constant Natural := Find_Slash (Parameter);
                        begin
                           if K = 0 then
                              --  No replacement, this is equivalent to
                              --  REPLACE(<regexp>/\1)
                              return (F.Handle (Mode),
                                      F.Parameter_Data'
                                        (F.Regpat,
                                         To_Unbounded_String (Parameter),
                                         new GNAT.Regpat.Pattern_Matcher'
                                           (GNAT.Regpat.Compile (Parameter)),
                                         To_Unbounded_String ("\1")));
                           else
                              return (F.Handle (Mode),
                                      F.Parameter_Data'
                                        (F.Regpat,
                                         To_Unbounded_String
                                           (Parameter
                                              (Parameter'First .. K - 1)),
                                         new GNAT.Regpat.Pattern_Matcher'
                                           (GNAT.Regpat.Compile
                                              (Parameter
                                                 (Parameter'First .. K - 1))),
                                         To_Unbounded_String
                                           (Parameter
                                              (K + 1 .. Parameter'Last))));
                           end if;
                        end;

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

            --  Specific check for the NO_CONTEXT filter which must appear
            --  first.

            if FS (K).Handle = Filter.No_Context'Access
              and then K /= FS'First
            then
               Exceptions.Raise_Exception
                 (Template_Error'Identity,
                  "NO_CONTEXT must be the first filter");
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

      Result : Tag_Var;

   begin
      if A_Sep <= F_Sep then
         --  This is not an attribute in fact, but something like:
         --  Filter(that's it):VAR
         A_Sep := 0;
      end if;

      Result.Name    := Get_Var_Name (Str);
      Result.Filters := Get_Filter_Set (Str);
      Result.Attr    := Get_Attribute (Str);

      declare
         Name : constant String := To_String (Result.Name);
      begin
         if Name (Name'First) = '$'
           and then Strings.Fixed.Count
             (Name, Strings.Maps.Constants.Decimal_Digit_Set) = Name'Length - 1
         then
            Result.N := Natural'Value (Name (Name'First + 1 .. Name'Last));
         else
            Result.N := -1;
         end if;
      end;

      return Result;
   end Build;

   -------------
   -- Release --
   -------------

   procedure Release (T : in out Tag_Var) is
      use type Filter.Set_Access;
      procedure Free is
         new Ada.Unchecked_Deallocation (Filter.Set, Filter.Set_Access);
   begin
      if T.Filters /= null then
         Filter.Release (T.Filters.all);
         Free (T.Filters);
      end if;
   end Release;

   ---------------
   -- Translate --
   ---------------

   function Translate
     (T            : in Tag_Var;
      Value        : in String;
      Translations : in Translate_Set := Null_Set;
      I_Params     : in Filter.Include_Parameters
                          := Filter.No_Include_Parameters)
      return String
   is
      use type Filter.Set_Access;
   begin
      if T.Filters /= null then
         declare
            R : Unbounded_String := To_Unbounded_String (Value);
         begin
            for K in T.Filters'Range loop
               R := To_Unbounded_String
                 (T.Filters (K).Handle (To_String (R),
                                        T.Filters (K).Parameters,
                                        Translations,
                                        I_Params));
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
               Var   : Tag_Var;
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
               Var : Tag_Var;

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
                  Section_Block, --  a TABLE block (common, section)
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

   Null_Static_Tree : constant Static_Tree := (null, null);

   type Include_Parameters is array (0 .. Max_Include_Parameters) of Data.Tree;

   No_Parameter : constant Include_Parameters := (others => null);

   type Node (Kind : Nkind) is record
      Next : Tree;
      Line : Natural;

      case Kind is
         when Info =>
            Filename  : Unbounded_String;    -- Name of the file
            Timestamp : Ada.Calendar.Time;   -- Date and Time of last change
            I_File    : Tree;                -- Included file references

         when C_Info =>
            Obsolete  : Boolean := False;    -- True if newer version in cache
            Used      : Natural := 0;        -- >0 if currently used

         when Text =>
            Text      : Data.Tree;

         when If_Stmt =>
            Cond      : Expr.Tree;
            N_True    : Tree;
            N_False   : Tree;

         when Table_Stmt =>
            Terminate_Sections : Boolean;
            Blocks             : Tree;
            Blocks_Count       : Natural;     -- Number if blocks

         when Section_Block =>
            Common         : Tree;
            Sections       : Tree;
            Sections_Count : Natural;         -- Number of sections

         when Section_Stmt =>
            N_Section : Tree;

         when Include_Stmt =>
            File     : Static_Tree;
            I_Params : Include_Parameters;
      end case;
   end record;

   procedure Release (T : in out Tree; Include : in Boolean := True);
   --  Release all memory associated with the tree

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
            Result   :    out Static_Tree);
         --  Returns the Tree for Filename or Null_Static_Tree if Filename has
         --  not been cached or is obsolete.

         procedure Release (T : in out Static_Tree);
         --  After loading a tree and using it, it is required that it be
         --  released. This will ensure that a tree marked as obsolete (a new
         --  version being now in the cache) will be released from the memory.

      end Prot;

   end Cached_Files;

   ---------
   -- Tag --
   ---------

   procedure Field
     (T      : in     Tag;
      N      : in     Positive;
      Result :    out Tag_Node_Access;
      Found  :    out Boolean);
   --  Returns the Nth item in Tag

   procedure Field
     (T      : in     Tag;
      Cursor : in     Indices;
      Result :    out Unbounded_String;
      Found  :    out Boolean);
   --  Returns Value in Tag at position Cursor. Found is set to False if
   --  there is no such value in Tag.

   ----------
   -- Size --
   ----------

   function Size (T : in Tag) return Natural is
   begin
      return T.Data.Count;
   end Size;

   -----------
   -- Clear --
   -----------

   procedure Clear (T : in out Tag) is
   begin
      --  Here we just separate current vector from the new one. The memory
      --  used by the current one will be collected by the Finalize
      --  routine. We just want a new independant Vector_Tag here.

      Finalize (T);

      T.Ref_Count         := new Integer'(1);
      T.Data              := new Tag_Data;
      T.Data.Count        := 0;
      T.Data.Min          := Natural'Last;
      T.Data.Max          := 0;
      T.Data.Head         := null;
      T.Data.Last         := null;
      T.Data.Position     := (null, new Integer'(1));
      T.Data.Nested_Level := 1;
   end Clear;

   ----------
   -- Item --
   ----------

   function Item (T : in Tag; N : in Positive) return String is
      Result : Unbounded_String;
      Found  : Boolean;
   begin
      Field (T, (1 => N), Result, Found);

      if not Found then
         raise Constraint_Error;
      else
         return To_String (Result);
      end if;
   end Item;

   ---------------
   -- Composite --
   ---------------

   function Composite (T : in Tag; N : in Positive) return Tag is
      Result : Tag;
      Found  : Boolean;
   begin
      Field (T, N, Result, Found);

      if Found then
         return Result;
      else
         raise Constraint_Error;
      end if;
   end Composite;

   -------------------
   -- Set_Separator --
   -------------------

   procedure Set_Separator (T : in out Tag; Separator : in String) is
   begin
      T.Data.Separator := To_Unbounded_String (Separator);
   end Set_Separator;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (T : in out Tag) is
   begin
      T.Ref_Count         := new Integer'(1);
      T.Data              := new Tag_Data;
      T.Data.Count        := 0;
      T.Data.Min          := Natural'Last;
      T.Data.Max          := 0;
      T.Data.Position     := (null, new Integer'(1));
      T.Data.Nested_Level := 1;
   end Initialize;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (T : in out Tag) is
   begin
      T.Ref_Count.all := T.Ref_Count.all - 1;

      if T.Ref_Count.all = 0 then
         declare
            procedure Free is new Ada.Unchecked_Deallocation
              (Tag_Node, Tag_Node_Access);

            procedure Free is new Ada.Unchecked_Deallocation
              (Tag, Tag_Access);

            procedure Free is new Ada.Unchecked_Deallocation
              (Tag_Node_Access, Access_Tag_Node_Access);

            procedure Free is new Ada.Unchecked_Deallocation
              (Tag_Data, Tag_Data_Access);

            P, N : Tag_Node_Access;
         begin
            P := T.Data.Head;

            while P /= null loop
               N := P.Next;

               if P.Kind = Value_Set then
                  Free (P.VS);
               end if;

               Free (P);

               P := N;
            end loop;

            T.Data.Head := null;
            T.Data.Last := null;

            Free (T.Ref_Count);
            Free (T.Data.Position.Pos);
            Free (T.Data.Position.Current);
            Free (T.Data);
         end;
      end if;
   end Finalize;

   ------------
   -- Adjust --
   ------------

   procedure Adjust (T : in out Tag) is
   begin
      T.Ref_Count.all := T.Ref_Count.all + 1;
   end Adjust;

   ---------
   -- "+" --
   ---------

   function "+" (Value : in String) return Tag is
      Item : constant Tag_Node_Access
        := new Tag_Node'(Templates_Parser.Value,
                         null, To_Unbounded_String (Value));
   begin
      return (Ada.Finalization.Controlled with
              Ref_Count    => new Integer'(1),
              Data         => new Tag_Data'
                (Count        => 1,
                 Min          => 1,
                 Max          => 1,
                 Nested_Level => 1,
                 Separator    => To_Unbounded_String (Default_Separator),
                 Head         => Item,
                 Last         => Item,
                 Position     =>
                   (new Tag_Node_Access'(Item), new Integer'(1))));
   end "+";

   function "+" (Value : in Character) return Tag is
   begin
      return +String'(1 => Value);
   end "+";

   function "+" (Value : in Boolean) return Tag is
   begin
      return +Boolean'Image (Value);
   end "+";

   function "+" (Value : in Unbounded_String) return Tag is
   begin
      return +To_String (Value);
   end "+";

   function "+" (Value : in Integer) return Tag is
   begin
      return +Image (Value);
   end "+";

   function "+" (Value : in Tag) return Tag is
      Result : Tag;
   begin
      Result := Result & Value;
      --  This is an embedded tag, set separator to LF
      Set_Separator (Result, (1 => ASCII.LF));
      return Result;
   end "+";

   ---------
   -- "&" --
   ---------

   function "&" (T : in Tag; Value : in String) return Tag is
      Item : constant Tag_Node_Access
        := new Tag_Node'
          (Templates_Parser.Value, null, To_Unbounded_String (Value));
   begin
      T.Ref_Count.all := T.Ref_Count.all + 1;

      if T.Data.Head = null then
         T.Data.all :=
           (T.Data.Count + 1,
            Min          => Natural'Min (T.Data.Min, 1),
            Max          => Natural'Max (T.Data.Max, 1),
            Nested_Level => 1,
            Separator    => To_Unbounded_String (Default_Separator),
            Head         => Item,
            Last         => Item,
            Position     => (new Tag_Node_Access'(Item), T.Data.Position.Pos));

         return (Ada.Finalization.Controlled with T.Ref_Count, T.Data);

      else
         T.Data.Last.Next := Item;
         T.Data.all :=
           (T.Data.Count + 1,
            Min          => Natural'Min (T.Data.Min, 1),
            Max          => Natural'Max (T.Data.Max, 1),
            Nested_Level => T.Data.Nested_Level,
            Separator    => T.Data.Separator,
            Head         => T.Data.Head,
            Last         => Item,
            Position     => T.Data.Position);

         return (Ada.Finalization.Controlled with T.Ref_Count, T.Data);
      end if;
   end "&";

   function "&" (Value : in String; T : in Tag) return Tag is
      Item : constant Tag_Node_Access
        := new Tag_Node'
          (Templates_Parser.Value, T.Data.Head, To_Unbounded_String (Value));
   begin
      T.Ref_Count.all := T.Ref_Count.all + 1;

      if T.Data.Head = null then
         T.Data.all :=
           (T.Data.Count + 1,
            Min          => Natural'Min (T.Data.Min, 1),
            Max          => Natural'Max (T.Data.Max, 1),
            Nested_Level => 1,
            Separator    => To_Unbounded_String (Default_Separator),
            Head         => Item,
            Last         => Item,
            Position     => (new Tag_Node_Access'(Item), T.Data.Position.Pos));

         return (Ada.Finalization.Controlled with T.Ref_Count, T.Data);

      else
         T.Data.all :=
           (T.Data.Count + 1,
            Min          => Natural'Min (T.Data.Min, 1),
            Max          => Natural'Max (T.Data.Max, 1),
            Nested_Level => T.Data.Nested_Level,
            Separator    => T.Data.Separator,
            Head         => Item,
            Last         => T.Data.Last,
            Position     => T.Data.Position);

         return (Ada.Finalization.Controlled with T.Ref_Count, T.Data);
      end if;
   end "&";

   function "&" (T : in Tag; Value : in Tag) return Tag is
      Item   : constant Tag_Node_Access
        := new Tag_Node'(Value_Set, null, new Tag'(Value));
      T_Size : constant Natural := Size (Value);
   begin
      T.Ref_Count.all := T.Ref_Count.all + 1;

      if T.Data.Head = null then
         T.Data.all :=
           (T.Data.Count + 1,
            Min          => Natural'Min (T.Data.Min, T_Size),
            Max          => Natural'Max (T.Data.Max, T_Size),
            Nested_Level => Value.Data.Nested_Level + 1,
            Separator    => To_Unbounded_String ((1 => ASCII.LF)),
            Head         => Item,
            Last         => Item,
            Position     => (new Tag_Node_Access'(Item), T.Data.Position.Pos));

         return (Ada.Finalization.Controlled with T.Ref_Count, T.Data);

      else
         T.Data.Last.Next := Item;

         T.Data.all :=
           (T.Data.Count + 1,
            Min          => Natural'Min (T.Data.Min, T_Size),
            Max          => Natural'Max (T.Data.Max, T_Size),
            Nested_Level =>
              Positive'Max
                (T.Data.Nested_Level, Value.Data.Nested_Level + 1),
            Separator    => T.Data.Separator,
            Head         => T.Data.Head,
            Last         => Item,
            Position     => T.Data.Position);

         return (Ada.Finalization.Controlled with T.Ref_Count, T.Data);
      end if;
   end "&";

   function "&" (T : in Tag; Value : in Character) return Tag is
   begin
      return T & String'(1 => Value);
   end "&";

   function "&" (T : in Tag; Value : in Boolean) return Tag is
   begin
      return T & Boolean'Image (Value);
   end "&";

   function "&" (T : in Tag; Value : in Unbounded_String) return Tag is
   begin
      return T & To_String (Value);
   end "&";

   function "&" (T : in Tag; Value : in Integer) return Tag is
   begin
      return T & Image (Value);
   end "&";

   function "&" (Value : in Character; T : in Tag) return Tag is
   begin
      return String'(1 => Value) & T;
   end "&";

   function "&" (Value : in Boolean; T : in Tag) return Tag is
   begin
      return Boolean'Image (Value) & T;
   end "&";

   function "&" (Value : in Unbounded_String; T : in Tag) return Tag is
   begin
      return To_String (Value) & T;
   end "&";

   function "&" (Value : in Integer; T : in Tag) return Tag is
   begin
      return Image (Value) & T;
   end "&";

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
     (T      : in     Tag;
      N      : in     Positive;
      Result :    out Tag_Node_Access;
      Found  :    out Boolean) is
   begin
      Found := True;

      if N = T.Data.Count then
         Result := T.Data.Last;

      elsif N > T.Data.Count then
         --  No such item for this position
         Result := null;
         Found  := False;

      elsif N > T.Data.Position.Pos.all then
         --  Use cursor to move to the right position

         for K in 1 .. N - T.Data.Position.Pos.all loop
            T.Data.Position.Pos.all     := T.Data.Position.Pos.all + 1;
            T.Data.Position.Current.all := T.Data.Position.Current.all.Next;
         end loop;

         Result := T.Data.Position.Current.all;
      else

         declare
            P : Tag_Node_Access := T.Data.Head;
         begin
            for K in 1 .. N - 1 loop
               P := P.Next;
            end loop;

            T.Data.Position.Pos.all     := N;
            T.Data.Position.Current.all := P;

            Result := P;
         end;
      end if;
   end Field;

   procedure Field
     (T      : in     Tag;
      N      : in     Positive;
      Result :    out Tag;
      Found  :    out Boolean)
   is
      R : Tag_Node_Access;
   begin
      Field (T, N, R, Found);

      if Found and then R.Kind = Value_Set then
         --  There is a Tag at this position, return it
         Result := R.VS.all;
      else
         Found := False;
      end if;
   end Field;

   procedure Field
     (T      : in     Tag;
      Cursor : in     Indices;
      Result :    out Unbounded_String;
      Found  :    out Boolean)
   is

      function Image (T : in Tag) return Unbounded_String;
      --  Returns T string representation

      -----------
      -- Image --
      -----------

      function Image (T : in Tag) return Unbounded_String is

         function Image (N : in Tag_Node) return Unbounded_String;
         --  Returns N string representation

         -----------
         -- Image --
         -----------

         function Image (N : in Tag_Node) return Unbounded_String is
         begin
            if N.Kind = Value then
               return N.V;
            else
               return Image (N.VS.all);
            end if;
         end Image;

         Result : Unbounded_String;
         N      : Tag_Node_Access := T.Data.Head;
      begin
         while N /= null loop
            if Result /= Null_Unbounded_String then
               Append (Result, T.Data.Separator);
            end if;
            Append (Result, Image (N.all));
            N := N.Next;
         end loop;
         return Result;
      end Image;

      C : Natural;
      P : Natural;
      R : Tag_Node_Access;
   begin
      Found := True;

      if Cursor'Length > T.Data.Nested_Level then
         C := Cursor'Last - T.Data.Nested_Level + 1;
         P := Cursor (C);

      elsif Cursor'Length /= 0 then
         C := Cursor'First;
         P := Cursor (C);
      end if;

      if Cursor'Length = 0 then
         --  No cursor, we just want the streamed T image
         Result := Image (T);

      else
         Field (T, P, R, Found);
      end if;

      if R /= null then
         --  We have found something at this indice

         if C = Cursor'Last then
            --  This is the last position

            if R.Kind = Value then
               --  Found a leaf, just return the value
               Result := R.V;
            else
               Result := Image (R.VS.all);
            end if;

         else
            --  There is more position to look for in the cursor

            if R.Kind = Value then
               --  This is a leaf, there is nothing more to look for
               Found  := False;
               Result := Null_Unbounded_String;

            else
               --  Look into next dimention
               Field
                 (R.VS.all,
                  Cursor (C + 1 .. Cursor'Last),
                  Result, Found);
            end if;
         end if;

      else
         Found := False;
      end if;
   end Field;

   ------------
   -- Filter --
   ------------

   package body Filter is separate;

   -------------------
   -- Translate_Set --
   -------------------

   ------------
   -- Exists --
   ------------

   function Exists
     (Set      : in Translate_Set;
      Variable : in String)
      return Boolean is
   begin
      return Association_Set.Containers.Is_In (Variable, Set.Set.all);
   end Exists;

   ---------
   -- Get --
   ---------

   function Get
     (Set  : in Translate_Set;
      Name : in String) return Association
   is
      Pos : Containers.Cursor;
   begin
      Pos := Containers.Find (Set.Set.all, Name);

      if Containers.Has_Element (Pos) then
         return Containers.Element (Pos);
      else
         return Null_Association;
      end if;
   end Get;

   ------------
   -- Insert --
   ------------

   procedure Insert (Set : in out Translate_Set; Item : in Association) is
   begin
      Containers.Replace
        (Set.Set.all, To_String (Item.Variable), Item);
   end Insert;

   procedure Insert (Set : in out Translate_Set; Items : in Translate_Set) is
      Pos : Containers.Cursor;
   begin
      Pos := Containers.First (Items.Set.all);

      while Containers.Has_Element (Pos) loop
         Insert (Set, Containers.Element (Pos));
         Pos := Containers.Next (Pos);
      end loop;
   end Insert;

   ------------
   -- Remove --
   ------------

   procedure Remove (Set : in out Translate_Set; Name : in String) is
   begin
      if Containers.Is_In (Name, Set.Set.all) then
         Containers.Delete (Set.Set.all, Name);
      end if;
   end Remove;

   ---------------------------
   -- For_Every_Association --
   ---------------------------

   procedure For_Every_Association (Set : in Translate_Set) is
      Pos  : Containers.Cursor;
      Quit : Boolean := False;
   begin
      Pos := Containers.First (Set.Set.all);

      while Containers.Has_Element (Pos) loop
         Action (Containers.Element (Pos), Quit);
         exit when Quit;
         Pos := Containers.Next (Pos);
      end loop;
   end For_Every_Association;

   ------------
   -- To_Set --
   ------------

   function To_Set (Table : in Translate_Table) return Translate_Set is
      Set : Translate_Set;
   begin
      for K in Table'Range loop
         Insert (Set, Table (K));
      end loop;
      return Set;
   end To_Set;

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
      Value     : in Tag;
      Separator : in String := Default_Separator)
      return Association
   is
      T : Tag := Value;
   begin
      if Separator /= Default_Separator then
         Set_Separator  (T, Separator);
      end if;

      return Association'
        (Composite,
         To_Unbounded_String (Variable),
         T);
   end Assoc;

   ---------
   -- Get --
   ---------

   function Get (Assoc : in Association) return Tag is
   begin
      if Assoc.Kind = Composite then
         return Assoc.Comp_Value;
      else
         raise Constraint_Error;
      end if;
   end Get;

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

      Error_Include_Message  : Unbounded_String;
      --  Message as reported while parsing the include file

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

      function Load_Include_Parameters
        (Parameters : in String)
         return Include_Parameters;
      --  Returns the include parameters found by parsing Parameters

      type Parse_Mode is
        (Parse_Std,              --  in standard line
         Parse_If,               --  in a if statement
         Parse_Elsif,            --  in elsif part of a if statement
         Parse_Else,             --  in else part of a if statement
         Parse_Table,            --  in a table statement
         Parse_Block,            --  in a table block statement
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
         Dir_Seps : constant Maps.Character_Set := Maps.To_Set ("/\");
      begin
         if Length (Include_Filename) > 1
           and then Maps.Is_In (Slice (Include_Filename, 1, 1) (1), Dir_Seps)
         then
            --  Include filename is an absolute path, return it without the
            --  leading directory separator.
            return Slice (Include_Filename, 2, Length (Include_Filename));

         else
            declare
               K : constant Natural
                 := Fixed.Index (Filename, Dir_Seps,
                                 Going => Strings.Backward);
            begin
               if K = 0 then
                  return To_String (Include_Filename);
               else
                  return Filename (Filename'First .. K)
                    & To_String (Include_Filename);
               end if;
            end;
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
         if Message (Message'Last) /= '.' then
            Exceptions.Raise_Exception
              (Template_Error'Identity,
               "In " & Filename
                 & " at line" & Natural'Image (Line) & ' ' & Message & '.');
         else
            Exceptions.Raise_Exception
              (Template_Error'Identity,
               "Included from " & Filename
                 & " at line" & Natural'Image (Line) & ", " & Message);
         end if;
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

      -----------------------------
      -- Load_Include_Parameters --
      -----------------------------

      function Load_Include_Parameters
        (Parameters : in String)
         return Include_Parameters
      is
         First, Last : Natural := 0;
         K           : Natural := 0;
         Result      : Include_Parameters;
      begin
         First := Parameters'First;

         while First <= Parameters'Last loop
            --  Skip blanks

            while First < Parameters'Last
              and then (Parameters (First) = ' '
                        or else Parameters (First) = ASCII.HT)
            loop
               First := First + 1;
            end loop;

            --  Look for end of parameter

            Last := First + 1;

            if Parameters (First) = '"' then
               --  Look for closing quote
               while Last < Parameters'Last
                 and then Parameters (Last) /= '"'
               loop
                  Last := Last + 1;
               end loop;

               if Parameters (Last) /= '"' then
                  Fatal_Error ("Missing closing quote in include parameters");
               end if;

               Result (K) := Data.Parse (Parameters (First + 1 .. Last - 1));
               K := K + 1;

            else
               --  Look for end of word

               while Last < Parameters'Last
                 and then Parameters (Last) /= ' '
                 and then Parameters (Last) /= ASCII.HT
               loop
                  Last := Last + 1;
               end loop;

               if Last = Parameters'Last then
                  Result (K) := Data.Parse (Parameters (First .. Last));
               else
                  Result (K) := Data.Parse (Parameters (First .. Last - 1));
               end if;

               K := K + 1;
            end if;

            First := Last + 1;
         end loop;

         return Result;
      end Load_Include_Parameters;

      -----------
      -- Parse --
      -----------

      function Parse
        (Mode    : in Parse_Mode;
         No_Read : in Boolean := False)
         return Tree
      is
         function Count_Sections (T : in Tree) return Natural;
         pragma Inline (Count_Sections);
         --  Returns the number of sections in T (Section_Stmt)

         function Count_Blocks (T : in Tree) return Natural;
         pragma Inline (Count_Blocks);
         --  Returns the number of sections in T (Table_Stmt)

         ------------------
         -- Count_Blocks --
         ------------------

         function Count_Blocks (T : in Tree) return Natural is
            C : Natural := 0;
            S : Tree    := T;
         begin
            while S /= null loop
               C := C + 1;
               S := S.Next;
            end loop;
            return C;
         end Count_Blocks;

         --------------------
         -- Count_Sections --
         --------------------

         function Count_Sections (T : in Tree) return Natural is
            C : Natural := 0;
            S : Tree    := T;
         begin
            while S /= null loop
               C := C + 1;
               S := S.N_Section;
            end loop;
            return C;
         end Count_Sections;

         T : Tree;

      begin
         if not No_Read
           and then (Mode /= Parse_Section
                     and then Mode /= Parse_Elsif
                     and then Mode /= Parse_Block)
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

            when Parse_Block =>
               if Is_Stmt (End_Table_Token) then
                  return null;
               end if;

               T := new Node (Section_Block);

               T.Line := Line;

               declare
                  Tmp : Tree;
               begin
                  Tmp := Parse (Parse_Section);

                  if Tmp = null then
                     --  This section is empty
                     return null;
                  end if;

                  if Is_Stmt (Begin_Token) then
                     --  It means that the section parsed above was common
                     T.Common   := Tmp.Next;

                     --  Now free the Section_Stmt container

                     Free (Tmp);

                     T.Sections := Parse (Parse_Section);

                  else
                     T.Common   := null;
                     T.Sections := Tmp;
                  end if;
               end;

               --  Count the number of section

               T.Sections_Count := Count_Sections (T.Sections);

               if T.Sections_Count = 1 and then T.Common = null then
                  --  A single section and no common section, rewrite it as a
                  --  simple common section.
                  T.Common := T.Sections.Next;
                  Free (T.Sections);
                  T.Sections_Count := 0;
               end if;

               if Is_Stmt (End_Table_Token) then
                  T.Next := null;
               else
                  T.Next := Parse (Parse_Block);
               end if;

               return T;

            when Parse_Section =>
               if Is_Stmt (End_If_Token) then
                  Fatal_Error ("@@END_IF@@ found, @@END_TABLE@@ expected");
               end if;

               T := new Node (Section_Stmt);

               T.Line := Line;

               T.Next := Parse (Parse_Section_Content);

               if Is_Stmt (End_Table_Token) and then T.Next = null then
                  --  Check if this section was empty, this happen when
                  --  we parse a section after @@END@@ followed by the end
                  --  of the table.
                  Free (T);
                  return null;
               end if;

               if Is_Stmt (End_Table_Token)
                 or else Is_Stmt (Begin_Token)
                 or else Is_Stmt (End_Token)
               then
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
                 or else Is_Stmt (Begin_Token)
                 or else Is_Stmt (End_Token)
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

            declare
               Param : constant Unbounded_String := Get_First_Parameter;
            begin
               if Param = Null_Unbounded_String then
                  T.Terminate_Sections := False;
               elsif Param = Terminate_Sections_Token then
                  T.Terminate_Sections := True;
               else
                  Fatal_Error ("Unknown table parameter " & To_String (Param));
               end if;
            end;

            T.Blocks       := Parse (Parse_Block);
            T.Next         := Parse (Mode);
            T.Blocks_Count := Count_Blocks (T.Blocks);

            --  Check now that if we have TERMINATE_SECTIONS option set and
            --  that there is more than one block, all blocks have the same
            --  number of section.

            if T.Terminate_Sections and then T.Blocks_Count > 1 then
               declare
                  Size : constant Positive := T.Blocks.Sections_Count;
                  B    : Tree              := T.Blocks.Next;
               begin
                  while B /= null loop
                     if B.Sections_Count /= Size
                       and then B.Sections_Count /= 0
                     then
                        Fatal_Error
                          ("All sections must have the same size "
                           & "when using TERMINATE_SECTIONS option");
                     end if;
                     B := B.Next;
                  end loop;
               end;
            end if;

            return T;

         elsif Is_Stmt (Include_Token) then
            T := new Node (Include_Stmt);

            T.Line := Line;

            begin
               T.File
                 := Load (Build_Include_Pathname (Get_First_Parameter),
                          Cached, True);
            exception
               when E : others =>
                  --  Error while parsing the include file, record this
                  --  error. Let the parser exit properly from the recursion
                  --  to be able to release properly the memory before
                  --  raising an exception.

                  Error_Include_Message
                    := To_Unbounded_String (Exception_Message (E));

                  Free (T);
                  return null;
            end;

            T.I_Params := Load_Include_Parameters (Get_All_Parameters);

            I_File
              := new Node'(Include_Stmt, I_File, Line, T.File, T.I_Params);

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
                    or else Is_Stmt (Elsif_Token)
                    or else Is_Stmt (Else_Token)
                    or else Is_Stmt (End_If_Token)
                    or else Is_Stmt (Include_Token)
                    or else Is_Stmt (Table_Token)
                    or else Is_Stmt (Section_Token)
                    or else Is_Stmt (End_Table_Token)
                    or else Is_Stmt (Begin_Token)
                    or else Is_Stmt (End_Token)
                  then
                     T.Next := Parse (Mode, No_Read => True);
                     return Root;
                  end if;
               end loop;
            end;
         end if;
      end Parse;

      T     : Static_Tree;
      New_T : Tree;
      Old   : Tree;

   begin
      if Cached then
         Cached_Files.Prot.Get (Filename, Result => T);

         if T /= Null_Static_Tree then
            pragma Assert (T.C_Info /= null);
            return T;
         end if;
      end if;

      Input.Open (File, Filename, Form => "shared=no");

      begin
         New_T := Parse (Parse_Std);

         Input.Close (File);
      exception
         when others =>
            Input.Close (File);
            raise;
      end;

      --  T is the tree file, add two nodes (Info and C_Info) in front of
      --  the tree.

      --  Add second node (cache info)

      Old := new Node'(C_Info, New_T, 0, False, 1);

      --  Add first node (info about tree)

      New_T := new Node'(Info,
                         Old,
                         0,
                         To_Unbounded_String (Filename),
                         AWS.OS_Lib.File_Timestamp (Filename),
                         I_File);

      if Error_Include_Message /= Null_Unbounded_String then
         --  An include filename was not found, release the memory now and
         --  raise a fatal error.

         Release (New_T);

         Fatal_Error (To_String (Error_Include_Message));
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

   --------------
   -- Callback --
   --------------

   procedure Callback
     (Context  : access Templates_Parser.Context;
      Variable : in     String;
      Result   :    out Unbounded_String;
      Found    :    out Boolean) is
   begin
      Result := Null_Unbounded_String;
      Found  := False;
   end Callback;

   -----------
   -- Parse --
   -----------

   function Parse
     (Filename          : in String;
      Translations      : in Translate_Table := No_Translation;
      Cached            : in Boolean         := False;
      Keep_Unknown_Tags : in Boolean         := False;
      Context           : in Context_Access  := Null_Context)
      return String is
   begin
      return To_String
        (Parse (Filename, Translations, Cached, Keep_Unknown_Tags, Context));
   end Parse;

   function Parse
     (Filename          : in String;
      Translations      : in Translate_Table := No_Translation;
      Cached            : in Boolean         := False;
      Keep_Unknown_Tags : in Boolean         := False;
      Context           : in Context_Access  := Null_Context)
      return Unbounded_String is
   begin
      return Parse
        (Filename, To_Set (Translations), Cached, Keep_Unknown_Tags, Context);
   end Parse;

   function Parse
     (Filename          : in String;
      Translations      : in Translate_Set;
      Cached            : in Boolean        := False;
      Keep_Unknown_Tags : in Boolean        := False;
      Context           : in Context_Access := Null_Context)
      return String is
   begin
      return To_String
        (Parse (Filename, Translations, Cached, Keep_Unknown_Tags, Context));
   end Parse;

   function Parse
     (Filename          : in String;
      Translations      : in Translate_Set;
      Cached            : in Boolean        := False;
      Keep_Unknown_Tags : in Boolean        := False;
      Context           : in Context_Access := Null_Context)
      return Unbounded_String
   is

      type Block_State is record
         Section_Number : Positive;
         Section        : Tree;
      end record;

      Empty_Block_State : constant Block_State := (1, null);

      type Parse_State is record
         Cursor         : Indices (1 .. 10);
         Max_Lines      : Natural;
         Max_Expand     : Natural;
         Table_Level    : Natural;
         Blocks_Count   : Natural;
         I_Params       : Include_Parameters;
         F_Params       : Filter.Include_Parameters;
         Block          : Block_State;
      end record;

      Empty_State : constant Parse_State
        := ((1 .. 10 => 0), 0, 0, 0, 0,
            No_Parameter, Filter.No_Include_Parameters, Empty_Block_State);

      Results : Unbounded_String := Null_Unbounded_String;

      Buffer  : String (1 .. 4 * 1_024);
      Last    : Natural := 0;
      --  Cache to avoid too many reallocation using Append on Results above

      Now     : Calendar.Time;

      procedure Analyze
        (T     : in Tree;
         State : in Parse_State);
      --  Parse T and build results file. State is needed for Vector_Tag and
      --  Matrix_Tag expansion.

      -------------
      -- Analyze --
      -------------

      procedure Analyze
        (T     : in Tree;
         State : in Parse_State)
      is
         use type Containers.Cursor;

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
         pragma Inline (Is_True);
         --  Return True if Str is one of "TRUE", "OUI", the case beeing not
         --  case sensitive.

         function Translate (Var : in Tag_Var) return String;
         --  Translate Tag variable using Translation table and apply all
         --  Filters and Atribute recorded for this variable.

         function I_Translate (Var : in Tag_Var) return String;
         --  As above but for an include variable

         procedure Flush;
         pragma Inline (Flush);
         --  Flush buffer to Results

         function Flatten_Parameters
           (I : in Include_Parameters)
            return Filter.Include_Parameters;
         --  Returns a flat representation of the include parameters, only the
         --  name or the value are kept. The tree are replaced by an empty
         --  value.

         ------------------------
         -- Flatten_Parameter  --
         ------------------------

         function Flatten_Parameters
           (I : in Include_Parameters)
            return Filter.Include_Parameters
         is
            use type Data.Tree;
            F : Filter.Include_Parameters;
         begin
            for K in I'Range loop
               if I (K) = null then
                  F (K) := Null_Unbounded_String;
               else
                  case I (K).Kind is
                     when Data.Text =>
                        F (K) := I (K).Value;
                     when Data.Var  =>
                        F (K) := To_Unbounded_String (Translate (I (K).Var));
                  end case;
               end if;
            end loop;
            return F;
         end Flatten_Parameters;

         -----------------
         -- I_Translate --
         -----------------

         function I_Translate (Var : in Tag_Var) return String is
            use type Data.Tree;
            use type Data.NKind;
         begin
            pragma Assert (Var.N /= -1);

            if Var.N <= Max_Include_Parameters
              and then State.I_Params (Var.N) /= null
            then
               declare
                  T : constant Data.Tree := State.I_Params (Var.N);
                  --  T is the data tree that should be evaluated in place
                  --  of the include variable.
               begin
                  if T.Next = null and then T.Kind = Data.Var then
                     --  Here we have a special case where the include
                     --  variable is replaced by a single variable.

                     declare
                        V : Tag_Var := T.Var;
                     begin
                        --  First thing we want to do is to inherit attributes
                        --  from the include variable if we have no attribute.

                        if V.Attr = Nil then
                           V.Attr := Var.Attr;
                        end if;

                        return Translate
                          (Var, Translate (V), Translations, State.F_Params);
                     end;


                  else
                     --  Here we flush the buffer and then we analyse the
                     --  include parameter. The result is contained into
                     --  the buffer which is large enough for an include
                     --  variable.

                     Flush;
                     Analyze (T);

                     declare
                        L : constant Natural := Last;
                     begin
                        Last := 0;
                        return Translate
                          (Var,
                           Buffer (Buffer'First .. L),
                           Translations,
                           State.F_Params);
                     end;
                  end if;
               end;

            else
               return "";
            end if;
         end I_Translate;

         ---------------
         -- Translate --
         ---------------

         function Translate (Var : in Tag_Var) return String is
            Pos : Containers.Cursor;
         begin
            Pos := Containers.Find
              (Translations.Set.all, To_String (Var.Name));

            if Containers.Has_Element (Pos) then
               declare
                  Tk : constant Association := Containers.Element (Pos);
               begin
                  case Tk.Kind is

                     when Std =>
                        if Var.Attr = Nil then
                           return Translate
                             (Var, To_String (Tk.Value),
                              Translations, State.F_Params);
                        else
                           Exceptions.Raise_Exception
                             (Template_Error'Identity,
                              "Attribute not valid on a discrete tag ("
                              & Image (Var) & ')');
                        end if;

                     when Composite =>
                        if Tk.Comp_Value.Data.Nested_Level = 1 then
                           --  This is a vector

                           if Var.Attr = Length then
                              return Translate
                                (Var,
                                 Image (Tk.Comp_Value.Data.Count),
                                 Translations, State.F_Params);

                           elsif Var.Attr /= Nil then
                              Exceptions.Raise_Exception
                                (Template_Error'Identity,
                                 "This attribute is not valid for a "
                                 & "vector tag (" & Image (Var) & ')');
                           end if;

                        elsif Tk.Comp_Value.Data.Nested_Level = 2 then
                           if Var.Attr = Line then
                              --  'Line on a matrix
                              return Translate
                                (Var,
                                 Image (Tk.Comp_Value.Data.Count),
                                 Translations, State.F_Params);

                           elsif Var.Attr = Min_Column then
                              --  'Min_Column on a matrix
                              return Translate
                                (Var,
                                 Image (Tk.Comp_Value.Data.Min),
                                 Translations, State.F_Params);

                           elsif Var.Attr = Max_Column then
                              --  'Max_Column on a matrix
                              return Translate
                                (Var,
                                 Image (Tk.Comp_Value.Data.Max),
                                 Translations, State.F_Params);

                           elsif Var.Attr /= Nil then
                              Exceptions.Raise_Exception
                                (Template_Error'Identity,
                                 "This attribute is not valid for a "
                                 & "matrix tag (" & Image (Var) & ')');
                           end if;
                        end if;

                        declare
                           Result : Unbounded_String;
                           Found  : Boolean;
                        begin
                           Field
                             (Tk.Comp_Value,
                              State.Cursor (1 .. State.Table_Level),
                              Result, Found);

                           return Translate
                             (Var, To_String (Result),
                              Translations, State.F_Params);
                        end;
                  end case;
               end;
            end if;

            declare
               T_Name : constant String := To_String (Var.Name);
               Found  : Boolean;
               Result : Unbounded_String;
            begin
               --  Check now for an internal tag

               if T_Name = "UP_TABLE_LINE" then
                  if State.Table_Level < 2 then
                     return Translate (Var, "0", Translations, State.F_Params);
                  else
                     return Translate
                       (Var,
                        Image (State.Cursor (State.Table_Level - 1)),
                        Translations, State.F_Params);
                  end if;

               elsif T_Name = "TABLE_LINE" then
                  if State.Table_Level = 0 then
                     return Translate (Var, "0", Translations, State.F_Params);
                  else
                     return Translate
                       (Var,
                        Image (State.Cursor (State.Table_Level)),
                        Translations, State.F_Params);
                  end if;

               elsif T_Name = "NUMBER_LINE" then
                  return Translate
                    (Var, Image (State.Max_Lines),
                     Translations, State.F_Params);

               elsif T_Name = "TABLE_LEVEL" then
                  return Translate
                    (Var, Image (State.Table_Level),
                     Translations, State.F_Params);

               elsif T_Name = "NOW" then
                  return Translate
                    (Var,
                     GNAT.Calendar.Time_IO.Image (Now, "%Y-%m-%d %H:%M:%S"),
                     Translations, State.F_Params);

               elsif T_Name = "YEAR" then
                  return Translate
                    (Var,
                     GNAT.Calendar.Time_IO.Image (Now, "%Y"),
                     Translations, State.F_Params);

               elsif T_Name = "MONTH" then
                  return Translate
                    (Var,
                     GNAT.Calendar.Time_IO.Image (Now, "%m"),
                     Translations, State.F_Params);

               elsif T_Name = "DAY" then
                  return Translate
                    (Var,
                     GNAT.Calendar.Time_IO.Image (Now, "%d"),
                     Translations, State.F_Params);

               elsif T_Name = "HOUR" then
                  return Translate
                    (Var,
                     GNAT.Calendar.Time_IO.Image (Now, "%H"),
                     Translations, State.F_Params);

               elsif T_Name = "MINUTE" then
                  return Translate
                    (Var,
                     GNAT.Calendar.Time_IO.Image (Now, "%M"),
                     Translations, State.F_Params);

               elsif T_Name = "SECOND" then
                  return Translate
                    (Var,
                     GNAT.Calendar.Time_IO.Image (Now, "%S"),
                     Translations, State.F_Params);

               elsif T_Name = "MONTH_NAME" then
                  return Translate
                    (Var,
                     GNAT.Calendar.Time_IO.Image (Now, "%B"),
                     Translations, State.F_Params);

               elsif T_Name = "DAY_NAME" then
                  return Translate
                    (Var,
                     GNAT.Calendar.Time_IO.Image (Now, "%A"),
                     Translations, State.F_Params);
               end if;

               --  Check now if the variable is known in the current user's
               --  context.

               if Context = Null_Context
                 or else Filter.Is_No_Context (Var.Filters)
               then
                  Found := False;
               else
                  Callback (Context, T_Name, Result, Found);
               end if;

               if Found then
                  return Translate
                    (Var, To_String (Result), Translations, State.F_Params);
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

            procedure Add (S : in String);
            --  Add S into Results (using Buffer cache if possible)

            ---------
            -- Add --
            ---------

            procedure Add (S : in String) is
            begin
               if Last + S'Length > Buffer'Last then
                  --  Not enough cache space, flush buffer
                  Flush;
               end if;

               if S'Length >= Buffer'Length then
                  Append (Results, S);
               else
                  Buffer (Last + 1 .. Last + S'Length) := S;
                  Last := Last + S'Length;
               end if;
            end Add;

            T : Data.Tree := D;

         begin
            while T /= null loop

               case T.Kind is

                  when Data.Text =>
                     Add (To_String (T.Value));

                  when Data.Var =>
                     if Is_Include_Variable (T.Var) then
                        Add (I_Translate (T.Var));
                     else
                        Add (Translate (T.Var));
                     end if;
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
               --  ??? remove exception handler
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
              := (Expr.O_Not => F_Not'Access);

         begin
            case E.Kind is
               when Expr.Value =>
                  return To_String (E.V);

               when Expr.Var =>
                  if Is_Include_Variable (E.Var) then
                     return I_Translate (E.Var);
                  else
                     return Translate (E.Var);
                  end if;

               when Expr.Op =>
                  return Op_Table (E.O) (Analyze (E.Left), Analyze (E.Right));

               when Expr.U_Op =>
                  return U_Op_Table (E.U_O) (Analyze (E.Next));
            end case;
         end Analyze;

         -----------
         -- Flush --
         -----------

         procedure Flush is
         begin
            Append (Results, Buffer (1 .. Last));
            Last := 0;
         end Flush;

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
               N : in Positive) return Natural;
            --  Recursivelly descends the tree and compute the max lines that
            --  will be displayed into the table. N is the variable embedded
            --  level regarding the table statement. N=1 means that the
            --  variable is just under the analysed table. N=2 means that the
            --  variable is found inside a nested table statement. And so on.

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

               function Check (T : in Expr.Tree) return Natural;
               --  Idem for an expression subtree as found in a condition

               function Check (T : in Tag_Var) return Natural;
               --  Returns the length of Tag T for the current context

               -----------
               -- Check --
               -----------

               function Check (T : in Tag_Var) return Natural is
                  Table_Level : constant Positive := State.Table_Level + 1;
                  --  This is the current table level, State.Table_Level is
                  --  not yet updated when calling this routine.

                  function Max (T : in Tag; N : in Natural) return Natural;
                  --  ???

                  ---------
                  -- Max --
                  ---------

                  function Max (T : in Tag; N : in Natural) return Natural is
                     Result : Natural := 0;
                  begin
                     declare
                        P : Tag_Node_Access := T.Data.Head;
                     begin
                        while P /= null loop
                           if P.Kind = Value_Set then
                              if N = 1 then
                                 Result :=
                                   Natural'Max (Result, P.VS.Data.Count);
                              else
                                 Result := Natural'Max
                                   (Result, Max (P.VS.all, N - 1));
                              end if;
                           end if;
                           P := P.Next;
                        end loop;
                     end;
                     return Result;
                  end Max;

                  Pos : Containers.Cursor;

               begin
                  Pos := Containers.Find
                    (Translations.Set.all, To_String (T.Name));

                  if Containers.Has_Element (Pos) then
                     declare
                        Tk : constant Association := Containers.Element (Pos);
                     begin
                        if Tk.Kind = Composite then
                           if N > Tk.Comp_Value.Data.Nested_Level then
                              --  Ignore this variable as it is deeper than
                              --  its nested level.
                              return 0;
                           end if;

                           --  We look first at two common cases to handle
                           --  more efficiently tag into a single or two
                           --  table statement.

                           if Table_Level = 1
                             or else Tk.Comp_Value.Data.Nested_Level = 1
                           then
                              --  First table level, or flat composite, the
                              --  number of iterations corresponds to the
                              --  number of item into this tag.
                              return Size (Tk.Comp_Value);

                           elsif Table_Level = 2
                             and then N = 1
                           then
                              --  Table level 2 while looking to nested
                              --  variable.
                              return Tk.Comp_Value.Data.Max;

                           else
                              --  All other cases here
                              declare
                                 K : constant Positive
                                   := Tk.Comp_Value.Data.Nested_Level - N + 1;
                                 --  K is the variable indice for which
                                 --  the number of items is looked for.
                              begin
                                 if K = 1 then
                                    return Size (Tk.Comp_Value);
                                 elsif K = 2 then
                                    return Tk.Comp_Value.Data.Max;
                                 else
                                    return Max (Tk.Comp_Value, K - 1);
                                 end if;
                              end;
                           end if;
                        end if;
                     end;
                  end if;

                  --  Tag not found

                  return Natural'First;
               end Check;

               function Check (T : in Data.Tree) return Natural is
                  use type Data.Tree;
                  use type Data.NKind;
                  Iteration : Natural := Natural'First;
                  D         : Data.Tree := T;
               begin
                  while D /= null loop
                     if D.Kind = Data.Var and then D.Var.Attr = Nil then
                        Iteration := Natural'Max (Iteration, Check (D.Var));
                     end if;

                     D := D.Next;
                  end loop;

                  return Iteration;
               end Check;

               function Check (T : in Expr.Tree) return Natural is
                  Iteration : Natural := Natural'First;
               begin
                  case T.Kind is
                     when Expr.Var   =>
                        Iteration := Natural'Max (Iteration, Check (T.Var));
                     when Expr.Op    =>
                        return Natural'Max (Check (T.Left), Check (T.Right));
                     when Expr.U_Op  =>
                        return Natural'Max (Iteration, Check (T.Next));
                     when Expr.Value =>
                        null;
                  end case;
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
                     return Natural'Max
                       (Check (T.Text), Get_Max_Lines (T.Next, N));

                  when If_Stmt =>
                     return Natural'Max
                       (Check (T.Cond),
                        Natural'Max
                          (Get_Max_Lines (T.Next, N),
                           Natural'Max
                             (Get_Max_Lines (T.N_True, N),
                              Get_Max_Lines (T.N_False, N))));

                  when Table_Stmt =>
                     return Natural'Max
                       (Get_Max_Lines (T.Blocks, N + 1),
                        Get_Max_Lines (T.Next, N));

                  when Section_Block =>
                     return Natural'Max
                       (Get_Max_Lines (T.Next, N),
                        Natural'Max
                          (Get_Max_Lines (T.Common, N),
                           Get_Max_Lines (T.Sections, N)));

                  when Section_Stmt =>
                     return Natural'Max
                       (Get_Max_Lines (T.Next, N),
                        Get_Max_Lines (T.N_Section, N));

                  when Include_Stmt =>
                     return Natural'Max
                       (Get_Max_Lines (T.File.Info, N),
                        Get_Max_Lines (T.Next, N));
               end case;
            end Get_Max_Lines;

            Result : Natural := Get_Max_Lines (T.Blocks, 1);

         begin
            pragma Assert (T.Kind = Table_Stmt);

            Max_Lines := Result;

            if T.Terminate_Sections then

               --  ??? This part of code handle properly only table with a
               --  single block. What should be done if there is multiple
               --  blocks ? Should all blocks of the same size ?

               declare
                  N_Section : constant Natural := T.Blocks.Sections_Count;
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
               declare
                  N : Tree := T;
               begin
                  begin
                     --  Handles all consecutive Text nodes

                     while N /= null and then N.Kind = Text loop
                        Analyze (N.Text);
                        N := N.Next;
                     end loop;

                  exception
                     when E : others =>
                        Exceptions.Raise_Exception
                          (Template_Error'Identity,
                           "In " & Filename
                             & " at line" & Natural'Image (N.Line) & ", "
                             & Exceptions.Exception_Message (E) & '.');
                  end;

                  Analyze (N, State);
               end;

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

                  Analyze (T.Blocks,
                           Parse_State'(State.Cursor,
                                        Max_Lines, Max_Expand,
                                        State.Table_Level + 1,
                                        T.Blocks_Count,
                                        State.I_Params,
                                        State.F_Params,
                                        Empty_Block_State));
               end;

               Analyze (T.Next, State);

            when Section_Block =>
               declare
                  B_State : array (1 .. State.Blocks_Count) of Block_State;
                  B       : Positive;
               begin
                  for K in 1 .. State.Max_Expand loop
                     declare
                        New_Cursor : Indices := State.Cursor;
                        Block      : Tree    := T;
                     begin
                        New_Cursor (State.Table_Level) := K;

                        B := 1;

                        while Block /= null loop
                           --  For all blocks in this table

                           if B_State (B).Section = null
                             or else B_State (B).Section.N_Section = null
                           then
                              B_State (B) := (1, Block.Sections);
                           else
                              B_State (B)
                                := (B_State (B).Section_Number + 1,
                                    B_State (B).Section.N_Section);
                           end if;

                           Analyze
                             (Block.Common,
                              Parse_State'(New_Cursor,
                                           State.Max_Lines, State.Max_Expand,
                                           State.Table_Level,
                                           State.Blocks_Count,
                                           State.I_Params,
                                           State.F_Params,
                                           Empty_Block_State));

                           Analyze
                             (Block.Sections,
                              Parse_State'(New_Cursor,
                                           State.Max_Lines, State.Max_Expand,
                                           State.Table_Level,
                                           State.Blocks_Count,
                                           State.I_Params,
                                           State.F_Params,
                                           B_State (B)));
                           Block := Block.Next;
                           B := B + 1;
                        end loop;
                     end;
                  end loop;
               end;

            when Section_Stmt =>
               Analyze
                 (State.Block.Section.Next,
                  Parse_State'(State.Cursor,
                               State.Max_Lines, State.Max_Expand,
                               State.Table_Level,
                               State.Blocks_Count,
                               State.I_Params,
                               State.F_Params,
                               State.Block));

            when Include_Stmt =>
               Analyze (T.File.Info,
                        Parse_State'(State.Cursor,
                                     State.Max_Lines, State.Max_Expand,
                                     State.Table_Level, State.Blocks_Count,
                                     T.I_Params,
                                     Flatten_Parameters (T.I_Params),
                                     State.Block));
               Analyze (T.Next, State);
         end case;
      end Analyze;

      T : Static_Tree;

   begin
      T := Load (Filename, Cached);

      Now := Ada.Calendar.Clock;
      --  Used for the time related variable

      Analyze (T.C_Info, Empty_State);

      if Cached then
         Cached_Files.Prot.Release (T);
      else
         Release (T.Info);
      end if;

      --  Flush buffer and return result

      Append (Results, Buffer (1 .. Last));

      return Results;
   end Parse;

   ----------------
   -- Print_Tree --
   ----------------

   procedure Print_Tree (T : in Tree; Level : in Natural := 0) is separate;

   procedure Print_Tree (Filename : in String) is
      T : Static_Tree;
   begin
      T := Load (Filename);
      Print_Tree (T.Info);
      Release (T.Info);
   end Print_Tree;

   -------------
   -- Release --
   -------------

   procedure Release (T : in out Tree; Include : in Boolean := True) is
      use type Data.Tree;
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

         when C_Info =>
            Release (T.Next, Include);

         when Text =>
            Data.Release (T.Text);
            Release (T.Next, Include);

         when If_Stmt  =>
            Expr.Release (T.Cond);
            Release (T.N_True, Include);
            Release (T.N_False, Include);
            Release (T.Next, Include);

         when Table_Stmt =>
            Release (T.Blocks, Include);
            Release (T.Next, Include);

         when Section_Block =>
            Release (T.Sections, Include);
            Release (T.Common, Include);
            Release (T.Next, Include);

         when Section_Stmt =>
            Release (T.Next, Include);
            Release (T.N_Section, Include);

         when Include_Stmt =>
            if Include then
               Release (T.File.Info, Include);

               for K in T.I_Params'Range loop
                  exit when T.I_Params (K) = null;
                  Data.Release (T.I_Params (K));
               end loop;

            end if;
            Release (T.Next, Include);
      end case;

      Free (T);
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
      return String is
   begin
      return Translate (Template, To_Set (Translations));
   end Translate;

   function Translate
     (Template     : in String;
      Translations : in Translate_Set)
      return String
   is
      T : Data.Tree := Data.Parse (Template);
      P : Data.Tree := T;

      Results : Unbounded_String;

      function Translate (Var : in Tag_Var) return String;
      --  Returns translation for Var

      ---------------
      -- Translate --
      ---------------

      function Translate (Var : in Tag_Var) return String is
         Pos : Containers.Cursor;
      begin
         Pos := Containers.Find (Translations.Set.all, To_String (Var.Name));

         if Containers.Has_Element (Pos) then
            declare
               Item : constant Association := Containers.Element (Pos);
            begin
               case Item.Kind is
                  when Std =>
                     return Translate (Var, To_String (Item.Value));
                  when others =>
                     return "";
               end case;
            end;
         end if;

         return "";
      end Translate;

      use type Data.Tree;

   begin
      while P /= null loop
         case P.Kind is
            when Data.Text => Append (Results, P.Value);
            when Data.Var  => Append (Results, Translate (P.Var));
         end case;

         P := P.Next;
      end loop;

      Data.Release (T);

      return To_String (Results);
   end Translate;

end Templates_Parser;
