------------------------------------------------------------------------------
--                             Templates Parser                             --
--                                                                          --
--                        Copyright (C) 1999 - 2002                         --
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
with GNAT.OS_Lib;
with GNAT.Regexp;

with Templates_Parser.Input;

package body Templates_Parser is

   use Ada;
   use Ada.Exceptions;
   use Ada.Strings;

   Internal_Error : exception;

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

   Filter_Lower_Token         : aliased constant String := "LOWER";
   Filter_Upper_Token         : aliased constant String := "UPPER";
   Filter_Capitalize_Token    : aliased constant String := "CAPITALIZE";
   Filter_Reverse_Token       : aliased constant String := "REVERSE";
   Filter_Repeat_Token        : aliased constant String := "REPEAT";
   Filter_Size_Token          : aliased constant String := "SIZE";
   Filter_Clean_Text_Token    : aliased constant String := "CLEAN_TEXT";
   Filter_Contract_Token      : aliased constant String := "CONTRACT";
   Filter_No_Space_Token      : aliased constant String := "NO_SPACE";
   Filter_No_Digit_Token      : aliased constant String := "NO_DIGIT";
   Filter_No_Letter_Token     : aliased constant String := "NO_LETTER";
   Filter_Format_Number_Token : aliased constant String := "FORMAT_NUMBER";
   Filter_Yes_No_Token        : aliased constant String := "YES_NO";
   Filter_Oui_Non_Token       : aliased constant String := "OUI_NON";
   Filter_Exist_Token         : aliased constant String := "EXIST";
   Filter_Is_Empty_Token      : aliased constant String := "IS_EMPTY";
   Filter_Match_Token         : aliased constant String := "MATCH";
   Filter_Trim_Token          : aliased constant String := "TRIM";
   Filter_Web_Escape_Token    : aliased constant String := "WEB_ESCAPE";
   Filter_Web_NBSP_Token      : aliased constant String := "WEB_NBSP";
   Filter_Coma_2_Point_Token  : aliased constant String := "COMA_2_POINT";
   Filter_Point_2_Coma_Token  : aliased constant String := "POINT_2_COMA";

   Blank : constant Maps.Character_Set := Maps.To_Set (' ' & ASCII.HT);

   ----------------------
   --  Filters setting --
   ----------------------

   --  A filter appear just before a tag variable (e.g. @_LOWER:SOME_VAR_@
   --  and means that the filter LOWER should be applied to SOME_VAR before
   --  replacing it in the template file.

   type Filters_Mode is
     (Capitalize,
      --  Lower case except char before spaces and underscores.

      Clean_Text,
      --  Only letter/digits all other chars are changed to spaces.

      Coma_2_Point,
      --  Replaces comas by points.

      Contract,
      --  Replaces a suite of spaces by a single space character.

      Exist,
      --  Returns "TRUE" if var is not empty and "FALSE" otherwise.

      Format_Number,
      --  Returns the number with a space added between each 3 digits
      --  blocks. The decimal part is not transformed. If the data is not a
      --  number nothing is done. The data is trimmed before processing it.

      Invert,
      --  Reverse string.

      Is_Empty,
      --  Returns "TRUE" if var is empty and "FALSE" otherwise.

      Lower,
      --  Lower case.

      Match,
      --  Returns "TRUE" if var match the pattern passed as argument.

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
      --  Returns N copy of the original string. The number of copy is passed
      --  as parameter.

      Size,
      --  Returns the number of characters in the string value.

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

   function Expect_Regexp (Mode : in Filters_Mode) return Boolean;
   --  Returns True is the filter named Filter_Name expect a regular
   --  expression as parameter.

   type Parameter_Mode is (Void, Str, Regexp);

   type Parameter_Data (Mode : Parameter_Mode := Void) is record
      case Mode is
         when Void =>
            null;

         when Str =>
            S : Unbounded_String;

         when Regexp =>
            R_Str  : Unbounded_String;
            Regexp : GNAT.Regexp.Regexp;
      end case;
   end record;

   No_Parameter : constant Parameter_Data := Parameter_Data'(Mode => Void);

   function Image (P : in Parameter_Data) return String;
   --  Returns parameter string representation.

   type Filter_Function is
     access function (S : in String; P : in Parameter_Data := No_Parameter)
     return String;
   --  P is the filter parameter, no parameter by default. Parameter are
   --  untyped and will be parsed by the filter function if needed.

   type Filter_Routine is record
      Handle     : Filter_Function;
      Parameters : Parameter_Data;
   end record;

   type Filter_Set is array (Positive range <>) of Filter_Routine;
   type Filter_Set_Access is access Filter_Set;

   type String_Access is access constant String;

   type Filter_Record is record
      Name   : String_Access;
      Handle : Filter_Function;
   end record;

   --  filter functions, see above.

   procedure Check_Null_Parameter (P : in Parameter_Data);
   --  Raises Template_Error if P is not equal to Null_Parameter.

   function Capitalize_Filter
     (S : in String; P : in Parameter_Data := No_Parameter) return String;

   function Clean_Text_Filter
     (S : in String; P : in Parameter_Data := No_Parameter) return String;

   function Coma_2_Point_Filter
     (S : in String; P : in Parameter_Data := No_Parameter) return String;

   function Contract_Filter
     (S : in String; P : in Parameter_Data := No_Parameter) return String;

   function Exist_Filter
     (S : in String; P : in Parameter_Data := No_Parameter) return String;

   function Format_Number_Filter
     (S : in String; P : in Parameter_Data := No_Parameter) return String;

   function Is_Empty_Filter
     (S : in String; P : in Parameter_Data := No_Parameter) return String;

   function Lower_Filter
     (S : in String; P : in Parameter_Data := No_Parameter) return String;

   function Match_Filter
     (S : in String; P : in Parameter_Data := No_Parameter) return String;

   function No_Digit_Filter
     (S : in String; P : in Parameter_Data := No_Parameter) return String;

   function No_Letter_Filter
     (S : in String; P : in Parameter_Data := No_Parameter) return String;

   function No_Space_Filter
     (S : in String; P : in Parameter_Data := No_Parameter) return String;

   function Oui_Non_Filter
     (S : in String; P : in Parameter_Data := No_Parameter) return String;

   function Point_2_Coma_Filter
     (S : in String; P : in Parameter_Data := No_Parameter) return String;

   function Repeat_Filter
     (S : in String; P : in Parameter_Data := No_Parameter) return String;

   function Reverse_Filter
     (S : in String; P : in Parameter_Data := No_Parameter) return String;

   function Size_Filter
     (S : in String; P : in Parameter_Data := No_Parameter) return String;

   function Trim_Filter
     (S : in String; P : in Parameter_Data := No_Parameter) return String;

   function Upper_Filter
     (S : in String; P : in Parameter_Data := No_Parameter) return String;

   function Web_Escape_Filter
     (S : in String; P : in Parameter_Data := No_Parameter) return String;

   function Web_NBSP_Filter
     (S : in String; P : in Parameter_Data := No_Parameter) return String;

   function Yes_No_Filter
     (S : in String; P : in Parameter_Data := No_Parameter) return String;


   function Filter_Handle (Name : in String) return Filter_Function;
   --  Returns the filter function for the given filter name.

   function Filter_Handle (Mode : in Filters_Mode) return Filter_Function;
   --  Returns the filter function for the given filter mode.

   function Filter_Mode (Name : in String) return Filters_Mode;
   --  Returns the Filters_Mode for filter named Name. This is the internal
   --  representation for this filter name.

   function Filter_Name (Handle : in Filter_Function) return String;
   --  Returns the filter name for the given filter function.

   --------------------
   --  Tags variable --
   --------------------

   type Tag is record
      Name    : Unbounded_String;
      Filters : Filter_Set_Access;
   end record;

   function Build (Str : in String) return Tag;
   --  Create a Tag from Str. A tag is composed of a name and a set of
   --  filters.

   function Image (T : in Tag) return String;
   --  Returns string representation for the Tag variable.

   function Translate (T : in Tag; Value : in String) return String;
   --  Returns the result of T.Name after applying all filters.

   procedure Release (T : in out Tag);
   --  Release all memory associated with Tag.

   -----------
   -- Image --
   -----------

   function Image (P : in Parameter_Data) return String is
   begin
      case P.Mode is
         when Void   => return "";
         when Str    => return '(' & To_String (P.S) & ')';
         when Regexp => return '(' & To_String (P.R_Str) & ')';
      end case;
   end Image;

   -----------
   -- Image --
   -----------

   function Image (T : in Tag) return String is
      R : Unbounded_String;
   begin
      R := Begin_Tag;

      if T.Filters /= null then
         for K in reverse T.Filters'Range loop
            Append (R, Filter_Name (T.Filters (K).Handle));
            Append (R, Image (T.Filters (K).Parameters));
            Append (R, ":");
         end loop;
      end if;

      Append (R, T.Name);
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

      function Get_Filter_Set (Tag : in String) return Filter_Set_Access;
      --  Given a tag name, it retruns a set of filter to apply to this
      --  variable when translated.

      --------------------
      -- Get_Filter_Set --
      --------------------

      function Get_Filter_Set (Tag : in String) return Filter_Set_Access is

         Start : Natural;
         Stop  : Natural := Tag'Last;
         FS    : Filter_Set (1 .. Strings.Fixed.Count (Tag, ":"));
         K     : Positive := FS'First;

         function Name_Parameter (Filter : in String) return Filter_Routine;
         --  Given a Filter description, returns the filter handle and
         --  parameter.

         function Name_Parameter (Filter : in String) return Filter_Routine is
            P1 : constant Natural := Strings.Fixed.Index (Filter, "(");
            P2 : constant Natural := Strings.Fixed.Index (Filter, ")",
                                                          Strings.Backward);
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
               return (Filter_Handle (Filter), Parameter_Data'(Mode => Void));

            else
               declare
                  Name : constant String
                    := Filter (Filter'First .. P1 - 1);

                  Mode : constant Filters_Mode := Filter_Mode (Name);

                  Parameter : constant String
                    := Filter (P1 + 1 .. P2 - 1);
               begin
                  if Expect_Regexp (Mode) then
                     return (Filter_Handle (Mode),
                             Parameter_Data'(Regexp,
                                             To_Unbounded_String (Parameter),
                                             GNAT.Regexp.Compile (Parameter)));
                  else
                     return (Filter_Handle (Mode),
                             Parameter_Data'(Templates_Parser.Str,
                                             To_Unbounded_String (Parameter)));
                  end if;
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
               --  last filter found
               FS (K) := Name_Parameter
                 (Tag (Tag'First + Length (Begin_Tag) .. Stop - 1));
            else
               FS (K) := Name_Parameter (Tag (Start + 1 .. Stop - 1));
            end if;

            K := K + 1;

            Stop := Stop - 1;
         end loop;

         return new Filter_Set'(FS);
      end Get_Filter_Set;

      ------------------
      -- Get_Var_Name --
      ------------------

      function Get_Var_Name (Tag : in String) return Unbounded_String is
         Pos : Natural;
      begin
         Pos := Strings.Fixed.Index (Tag, ":", Strings.Backward);

         if Pos = 0 then
            return To_Unbounded_String
              (Tag (Tag'First + Length (Begin_Tag)
                    .. Tag'Last - Length (End_Tag)));
         else
            return To_Unbounded_String
              (Tag (Pos + 1 .. Tag'Last - Length (End_Tag)));
         end if;
      end Get_Var_Name;

   begin
      return (Get_Var_Name (Str), Get_Filter_Set (Str));
   end Build;

   -------------
   -- Release --
   -------------

   procedure Release (T : in out Tag) is
      procedure Free is
         new Ada.Unchecked_Deallocation (Filter_Set, Filter_Set_Access);
   begin
      Free (T.Filters);
   end Release;

   ---------------
   -- Translate --
   ---------------

   function Translate (T : in Tag; Value : in String) return String is
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
            Obsolete  : Boolean := False;    --  True if newerversion in cache
            Used      : Natural := 0;        --  >0 if currently used

         when Text =>
            Text    : Data.Tree;

         when If_Stmt =>
            Cond    : Expr.Tree;
            N_True  : Tree;
            N_False : Tree;

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

   function Field
     (Vect_Value : in Vector_Tag;
      N          : in Positive)
      return String;
   --  returns the Nth value in the vector tag.

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

   -----------
   -- Clear --
   -----------

   procedure Clear (Vect : in out Vector_Tag) is
   begin
      --  Here we just separate current vector from the new one. The memory
      --  used by the current one will be collected by the Finalize
      --  routine. We just want a new independant Vector_Tag here.
      Vect.Ref_Count := new Integer'(1);
      Vect.Count     := 0;
      Vect.Head      := null;
      Vect.Last      := null;
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
      V.Count     := 0;
   end Initialize;

   ----------
   -- Item --
   ----------

   function Item (Vect : in Vector_Tag; N : in Positive) return String is
      K : Positive := 1;
      V : Vector_Tag_Node_Access := Vect.Head;
   begin
      loop
         if K = N then
            return To_String (V.Value);
         end if;

         V := V.Next;
         K := K + 1;

         if V = null then
            raise Constraint_Error;
         end if;
      end loop;
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

   ---------
   -- "+" --
   ---------

   function "+" (Vect : in Vector_Tag) return Matrix_Tag is
      Item : constant Matrix_Tag_Node_Access
        := new Matrix_Tag_Node'(Vect, null);
   begin
      return Matrix_Tag'(M => (Ada.Finalization.Controlled with
                               new Integer'(1), 1, Item, Item));
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
   begin
      Matrix.M.Ref_Count.all := Matrix.M.Ref_Count.all + 1;

      if Matrix.M.Head = null then
         return Matrix_Tag'(M => (Ada.Finalization.Controlled with
                                  Matrix.M.Ref_Count,
                                  Matrix.M.Count + 1,
                                  Head => Item,
                                  Last => Item));
      else
         Matrix.M.Last.Next := Item;
         return Matrix_Tag'(M => (Ada.Finalization.Controlled with
                                  Matrix.M.Ref_Count,
                                  Matrix.M.Count + 1,
                                  Head => Matrix.M.Head,
                                  Last => Item));
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
      M.Count     := 0;
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

   function Vector
     (Matrix : in Matrix_Tag;
      N      : in Positive)
      return Vector_Tag
   is
      P : Matrix_Tag_Node_Access := Matrix.M.Head;
   begin
      for K in 1 .. N - 1 loop
         P := P.Next;
      end loop;

      return P.Vect;
   exception
      when others =>
         Exceptions.Raise_Exception
           (Internal_Error'Identity, "Index out of range");
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

   -------------
   -- Filters --
   -------------

   function Expect_Regexp (Mode : in Filters_Mode) return Boolean is
   begin
      if Mode = Match then
         return True;
      else
         return False;
      end if;
   end Expect_Regexp;

   --------------------------
   -- Check_Null_Parameter --
   --------------------------

   procedure Check_Null_Parameter (P : in Parameter_Data) is
   begin
      if P.Mode /= Void then
         Exceptions.Raise_Exception
           (Template_Error'Identity, "no parameter allowed in this filter");
      end if;
   end Check_Null_Parameter;

   -----------------------
   -- Capitalize_Filter --
   -----------------------

   function Capitalize_Filter
     (S : in String;
      P : in Parameter_Data := No_Parameter)
      return String
   is
      Result : String (S'Range);
      Upper  : Boolean := True;
   begin
      Check_Null_Parameter (P);

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

   function Clean_Text_Filter
     (S : in String;
      P : in Parameter_Data := No_Parameter)
      return String
   is

      use type Strings.Maps.Character_Set;

      Result : String (S'Range);

      Clean_Set : constant Strings.Maps.Character_Set
        := Strings.Maps.Constants.Letter_Set
        or Strings.Maps.Constants.Decimal_Digit_Set
        or Strings.Maps.To_Set (" йикопафз");

   begin
      Check_Null_Parameter (P);

      for K in S'Range loop
         if Strings.Maps.Is_In (S (K), Clean_Set) then
            Result (K) := S (K);
         else
            Result (K) := ' ';
         end if;
      end loop;
      return Result;
   end Clean_Text_Filter;

   -------------------------
   -- Coma_2_Point_Filter --
   -------------------------

   function Coma_2_Point_Filter
     (S : in String;
      P : in Parameter_Data := No_Parameter)
      return String
   is
      Result : String := S;
   begin
      Check_Null_Parameter (P);

      for K in Result'Range loop
         if Result (K) = ',' then
            Result (K) := '.';
         end if;
      end loop;

      return Result;
   end Coma_2_Point_Filter;

   ---------------------
   -- Contract_Filter --
   ---------------------

   function Contract_Filter
     (S : in String;
      P : in Parameter_Data := No_Parameter)
      return String
   is

      use type Strings.Maps.Character_Set;

      Result : String (S'Range);
      R      : Natural := 0;
      Space  : Boolean := False;

   begin
      Check_Null_Parameter (P);

      for K in S'Range loop

         if S (K) = ' ' then

            if Space = False then
               Space := True;

               R := R + 1;
               Result (R) := ' ';
            end if;

         else
            Space := False;

            R := R + 1;
            Result (R) := S (K);
         end if;

      end loop;

      if R = 0 then
         return "";
      else
         return Result (Result'First .. R);
      end if;
   end Contract_Filter;

   ------------------
   -- Exist_Filter --
   ------------------

   function Exist_Filter
     (S : in String;
      P : in Parameter_Data := No_Parameter)
      return String is
   begin
      Check_Null_Parameter (P);

      if S /= "" then
         return "TRUE";
      else
         return "FALSE";
      end if;
   end Exist_Filter;

   --------------------------
   -- Format_Number_Filter --
   --------------------------

   function Format_Number_Filter
     (S : in String;
      P : in Parameter_Data := No_Parameter)
      return String
   is
      TS : constant String := Strings.Fixed.Trim (S, Both);

      function Is_Number return Boolean;
      --  Returns true if S is a number.

      Point : Natural := 0;

      function Is_Number return Boolean is
      begin
         for K in TS'Range loop
            if TS (K) = '.' then
               Point := K;

            elsif not Characters.Handling.Is_Digit (TS (K)) then
               return False;
            end if;
         end loop;

         return True;
      end Is_Number;

      Result : String (1 .. TS'Length * 2);
      K      : Positive := Result'Last;

      I      : Natural;
      Count  : Natural := 0;

   begin
      Check_Null_Parameter (P);

      if Is_Number then

         if Point = 0 then
            I := TS'Last;
         else
            I := Point - 1;
         end if;

         for P in reverse TS'First .. I loop
            Result (K) := TS (P);
            K := K - 1;
            Count := Count + 1;

            if Count mod 3 = 0 and then P /= TS'First then
               Result (K) := ' ';
               K := K - 1;
            end if;
         end loop;

         if Point = 0 then
            return Result (K + 1 .. Result'Last);

         else
            return Result (K + 1 .. Result'Last) & TS (Point .. TS'Last);
         end if;

      else
         return S;
      end if;
   end Format_Number_Filter;

   ---------------------
   -- Is_Empty_Filter --
   ---------------------

   function Is_Empty_Filter
     (S : in String;
      P : in Parameter_Data := No_Parameter)
      return String is
   begin
      Check_Null_Parameter (P);

      if S = "" then
         return "TRUE";
      else
         return "FALSE";
      end if;
   end Is_Empty_Filter;

   ------------------
   -- Lower_Filter --
   ------------------

   function Lower_Filter
     (S : in String;
      P : in Parameter_Data := No_Parameter)
      return String is
   begin
      Check_Null_Parameter (P);

      return Characters.Handling.To_Lower (S);
   end Lower_Filter;

   ------------------
   -- Match_Filter --
   -------------------

   function Match_Filter
     (S : in String;
      P : in Parameter_Data := No_Parameter)
      return String is
   begin
      if P = No_Parameter then
         Exceptions.Raise_Exception
           (Template_Error'Identity, "missing parameter for MATCH filter");
      end if;

      if GNAT.Regexp.Match (S, P.Regexp) then
         return "TRUE";
      else
         return "FALSE";
      end if;
   end Match_Filter;

   ---------------------
   -- No_Digit_Filter --
   ---------------------

   function No_Digit_Filter
     (S : in String;
      P : in Parameter_Data := No_Parameter)
      return String
   is
      Result : String := S;
   begin
      Check_Null_Parameter (P);

      for K in S'Range loop
         if Strings.Maps.Is_In (S (K),
                                Strings.Maps.Constants.Decimal_Digit_Set)
         then
            Result (K) := ' ';
         end if;
      end loop;

      return Result;
   end No_Digit_Filter;

   ---------------------
   -- No_Letter_Filter --
   ----------------------

   function No_Letter_Filter
     (S : in String;
      P : in Parameter_Data := No_Parameter)
      return String
   is
      Result : String := S;
   begin
      Check_Null_Parameter (P);

      for K in S'Range loop
         if Strings.Maps.Is_In (S (K), Strings.Maps.Constants.Letter_Set) then
            Result (K) := ' ';
         end if;
      end loop;

      return Result;
   end No_Letter_Filter;

   ---------------------
   -- No_Space_Filter --
   ---------------------

   function No_Space_Filter
     (S : in String;
      P : in Parameter_Data := No_Parameter)
      return String
   is
      Result : String (S'Range);
      L      : Natural := Result'First - 1;
   begin
      Check_Null_Parameter (P);

      for K in S'Range loop
         if not (S (K) = ' ') then
            L := L + 1;
            Result (L) := S (K);
         end if;
      end loop;

      return Result (Result'First .. L);
   end No_Space_Filter;

   --------------------
   -- Oui_Non_Filter --
   --------------------

   function Oui_Non_Filter
     (S : in String;
      P : in Parameter_Data := No_Parameter)
      return String is
   begin
      Check_Null_Parameter (P);

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

   -------------------------
   -- Point_2_Coma_Filter --
   -------------------------

   function Point_2_Coma_Filter
     (S : in String;
      P : in Parameter_Data := No_Parameter)
      return String
   is
      Result : String := S;
   begin
      Check_Null_Parameter (P);

      for K in Result'Range loop
         if Result (K) = '.' then
            Result (K) := ',';
         end if;
      end loop;

      return Result;
   end Point_2_Coma_Filter;

   -------------------
   -- Repeat_Filter --
   -------------------

   function Repeat_Filter
     (S : in String;
      P : in Parameter_Data := No_Parameter)
      return String
   is
      N : Natural;
   begin
      N := Natural'Value (To_String (P.S));

      declare
         R : String (1 .. N * S'Length);
      begin
         for K in 1 .. N loop
            R (1 + (K - 1) * S'Length .. S'Length * K) := S;
         end loop;

         return R;
      end;

   exception
      when Constraint_Error =>
         Exceptions.Raise_Exception
           (Template_Error'Identity, "repeat filter parameter error");
   end Repeat_Filter;

   --------------------
   -- Reverse_Filter --
   --------------------

   function Reverse_Filter
     (S : in String;
      P : in Parameter_Data := No_Parameter)
      return String
   is
      Result : String (S'Range);
   begin
      Check_Null_Parameter (P);

      for K in S'Range loop
         Result (Result'Last - K + Result'First) := S (K);
      end loop;
      return Result;
   end Reverse_Filter;

   -----------------
   -- Size_Filter --
   -----------------

   function Size_Filter
     (S : in String;
      P : in Parameter_Data := No_Parameter)
      return String is
   begin
      Check_Null_Parameter (P);

      declare
         R : constant String := Integer'Image (S'Length);
      begin
         return R (R'First + 1 .. R'Last);
      end;
   end Size_Filter;

   -----------------
   -- Trim_Filter --
   -----------------

   function Trim_Filter
     (S : in String;
      P : in Parameter_Data := No_Parameter)
      return String is
   begin
      Check_Null_Parameter (P);

      return Ada.Strings.Fixed.Trim (S, Ada.Strings.Both);
   end Trim_Filter;

   ------------------
   -- Upper_Filter --
   ------------------

   function Upper_Filter
     (S : in String;
      P : in Parameter_Data := No_Parameter)
      return String is
   begin
      Check_Null_Parameter (P);

      return Characters.Handling.To_Upper (S);
   end Upper_Filter;

   -------------------
   -- Escape_Filter --
   -------------------

   function Web_Escape_Filter
     (S : in String;
      P : in Parameter_Data := No_Parameter)
      return String
   is
      Max_Escape_Sequence : constant Positive := 5;
      Result              : String (1 .. S'Length * Max_Escape_Sequence);
      Last                : Natural := 0;
   begin
      Check_Null_Parameter (P);

      for I in S'Range loop
         Last := Last + 1;

         case S (I) is
            when '&' =>
               Result (Last .. Last + 4) := "&amp;";
               Last := Last + 4;

            when '>' =>
               Result (Last .. Last + 3) := "&gt;";
               Last := Last + 3;

            when '<' =>
               Result (Last .. Last + 3) := "&lt;";
               Last := Last + 3;

            when '"' =>
               Result (Last .. Last + 5) := "&quot;";
               Last := Last + 5;

            when others =>
               Result (Last) := S (I);
         end case;

      end loop;

      return Result (1 .. Last);
   end Web_Escape_Filter;

   ---------------------
   -- Web_NBSP_Filter --
   ---------------------

   function Web_NBSP_Filter
     (S : in String;
      P : in Parameter_Data := No_Parameter)
      return String
   is
      Nbsp_Token          : constant String := "&nbsp;";
      Max_Escape_Sequence : constant Positive := Nbsp_Token'Length;
      Result              : String (1 .. S'Length * Max_Escape_Sequence);
      Last                : Natural := 0;
   begin
      Check_Null_Parameter (P);

      for I in S'Range loop
         Last := Last + 1;

         if S (I) = ' ' then
            Result (Last .. Last + Nbsp_Token'Length - 1) := Nbsp_Token;
            Last := Last + Nbsp_Token'Length - 1;
         else
            Result (Last) := S (I);
         end if;

      end loop;

      return Result (1 .. Last);
   end Web_NBSP_Filter;

   -------------------
   -- Yes_No_Filter --
   -------------------

   function Yes_No_Filter
     (S : in String;
      P : in Parameter_Data := No_Parameter)
      return String is
   begin
      Check_Null_Parameter (P);

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

   --  Filter Table

   Filter_Table : constant array (Filters_Mode) of Filter_Record
     := (Capitalize     =>
           (Filter_Capitalize_Token'Access,     Capitalize_Filter'Access),

         Clean_Text     =>
           (Filter_Clean_Text_Token'Access,     Clean_Text_Filter'Access),

         Coma_2_Point   =>
           (Filter_Coma_2_Point_Token'Access,   Coma_2_Point_Filter'Access),

         Contract       =>
           (Filter_Contract_Token'Access,       Contract_Filter'Access),

         Exist          =>
           (Filter_Exist_Token'Access,          Exist_Filter'Access),

         Format_Number  =>
           (Filter_Format_Number_Token'Access,  Format_Number_Filter'Access),

         Invert        =>
           (Filter_Reverse_Token'Access,        Reverse_Filter'Access),

         Is_Empty       =>
           (Filter_Is_Empty_Token'Access,       Is_Empty_Filter'Access),

         Lower          =>
           (Filter_Lower_Token'Access,          Lower_Filter'Access),

         Match          =>
           (Filter_Match_Token'Access,          Match_Filter'Access),

         No_Digit       =>
           (Filter_No_Digit_Token'Access,       No_Digit_Filter'Access),

         No_Letter      =>
           (Filter_No_Letter_Token'Access,      No_Letter_Filter'Access),

         No_Space       =>
           (Filter_No_Space_Token'Access,       No_Space_Filter'Access),

         Oui_Non        =>
           (Filter_Oui_Non_Token'Access,        Oui_Non_Filter'Access),

         Point_2_Coma   =>
           (Filter_Point_2_Coma_Token'Access,   Point_2_Coma_Filter'Access),

         Repeat         =>
           (Filter_Repeat_Token'Access,         Repeat_Filter'Access),

         Size           =>
           (Filter_Size_Token'Access,           Size_Filter'Access),

         Trim           =>
           (Filter_Trim_Token'Access,           Trim_Filter'Access),

         Upper          =>
           (Filter_Upper_Token'Access,          Upper_Filter'Access),

         Web_Escape     =>
           (Filter_Web_Escape_Token'Access,     Web_Escape_Filter'Access),

         Web_NBSP =>
           (Filter_Web_NBSP_Token'Access,       Web_NBSP_Filter'Access),

         Yes_No         =>
           (Filter_Yes_No_Token'Access,         Yes_No_Filter'Access)
         );

   -------------------
   -- Filter_Handle --
   -------------------

   function Filter_Handle (Name : in String) return Filter_Function is
      Mode : Filters_Mode := Filter_Mode (Name);
   begin
      return Filter_Table (Mode).Handle;
   end Filter_Handle;

   function Filter_Handle (Mode : in Filters_Mode) return Filter_Function is
   begin
      return Filter_Table (Mode).Handle;
   end Filter_Handle;

   -----------------
   -- Filter_Mode --
   -----------------

   function Filter_Mode (Name : in String) return Filters_Mode is
   begin
      for K in Filter_Table'Range loop
         if Filter_Table (K).Name.all = Name then
            return K;
         end if;
      end loop;

      Exceptions.Raise_Exception
        (Internal_Error'Identity, "Unknown filter " & Name);
   end Filter_Mode;

   -----------------
   -- Filter_Name --
   -----------------

   function Filter_Name (Handle : in Filter_Function) return String is
   begin
      for K in Filter_Table'Range loop
         if Filter_Table (K).Handle = Handle then
            return Filter_Table (K).Name.all;
         end if;
      end loop;

      Exceptions.Raise_Exception
        (Internal_Error'Identity, "Unknown filter handle");
   end Filter_Name;

   -----------
   -- Field --
   -----------

   function Field
     (Vect_Value : in Vector_Tag;
      N          : in Positive)
      return String
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

   function Field
     (Mat_Value : in Matrix_Tag;
      I, J      : in Natural)
      return String
   is
      P : Matrix_Tag_Node_Access := Mat_Value.M.Head;
   begin
      if I = Mat_Value.M.Count then
         return Field (Mat_Value.M.Last.Vect, J);

      elsif I > Mat_Value.M.Count then
         return "";

      else
         for K in 1 .. I - 1 loop
            P := P.Next;
         end loop;
         return Field (P.Vect, J);
      end if;
   end Field;

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
      if Value in Natural then
         return Assoc (Variable,
                       S_Value (S_Value'First + 1 .. S_Value'Last));
      else
         return Assoc (Variable, S_Value);
      end if;
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

      --  Line handling

      procedure Fatal_Error (Message : in String);
      pragma No_Return (Fatal_Error);
      --  raise Template_Error exception with message.

      function Get_Next_Line return Boolean;
      --  Get new line in File and set Buffer, Last and First.

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

      type Parse_Mode is
        (Parse_Std,              --  in standard line
         Parse_If,               --  in a if statement
         Parse_Elsif,            --  in elsif part of a if statement
         Parse_Else,             --  in else part of a if statement
         Parse_Table,            --  in a table statement
         Parse_Section,          --  in new section
         Parse_Section_Content   --  in section content
        );

      function Parse (Mode : in Parse_Mode) return Tree;
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
            Fatal_Error ("@@IF@@ missing condition");
         end if;

         if Buffer (Last) = ASCII.CR then
            --  last character is a DOS CR (certainly because the template
            --  file is in DOS format), ignore it as this is not part of the
            --  parameter.
            Last := Last - 1;
         end if;

         return Buffer (Start .. Last);
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

         Start := Strings.Fixed.Index_Non_Blank (Buffer (Start .. Last));

         if Start = 0 then
            --  We have only spaces after the first word, there is no
            --  parameter in this case.
            return Null_Unbounded_String;
         end if;

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
         if Input.End_Of_File (File) then
            Last := 0;
            return True;

         else
            Line := Line + 1;

            Input.Get_Line (File, Buffer, Last);

            First := Strings.Fixed.Index_Non_Blank (Buffer (1 .. Last));

            if First = 0 then
               --  There is only spaces on this line, this is an empty line
               --  we just have to skip it.
               Last := 0;
               return False;
            end if;

            Last := Strings.Fixed.Index_Non_Blank
              (Buffer (1 .. Last), Strings.Backward);

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
         if Mode /= Parse_Section and then Mode /= Parse_Elsif then
            if Get_Next_Line then
               return null;
            end if;
         end if;

         case Mode is
            when Parse_Std =>
               if Is_Stmt (End_If_Token) then
                  Fatal_Error ("@@END_IF@@ found outside an @@IF@@ statement");
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

            T.Cond    := Expr.Parse (Get_All_Parameters);
            T.N_True  := Parse (Parse_If);

            if Is_Stmt (End_If_Token) then
               T.N_False := null;

            elsif Is_Stmt (Elsif_Token) then
               T.N_False := Parse (Parse_Elsif);

            elsif EOF then
               Fatal_Error ("EOF found, @@END_IF@@ expected");

            else
               T.N_False := Parse (Parse_Else);

            end if;

            T.Next    := Parse (Mode);

            return T;

         elsif Is_Stmt (Table_Token) then
            T := new Node (Table_Stmt);

            T.Line := Line;

            T.Terminate_Sections :=
              Get_First_Parameter = Terminate_Sections_Token;

            T.Sections := Parse (Parse_Section);
            T.Next     := Parse (Mode);

            return T;

         elsif Is_Stmt (Include_Token) then
            T := new Node (Include_Stmt);

            T.Line := Line;

            T.File :=
              Load (Build_Include_Pathname (Get_First_Parameter),
                    Cached, True);

            I_File := new Node'(Include_Stmt, I_File, Line, T.File);

            T.Next := Parse (Mode);

            return T;

         else
            T := new Node (Text);

            T.Line := Line;

            if Input.LF_Terminated (File)
              and then (not Input.End_Of_File (File)
                          or else Include_File)
            then
               --  Add a LF is the read line with terminated by a LF. Do not
               --  add this LF if we reach the end of file except for included
               --  files.
               T.Text := Data.Parse (Buffer (1 .. Last) & ASCII.LF);
            else
               T.Text := Data.Parse (Buffer (1 .. Last));
            end if;

            T.Next := Parse (Mode);
            return T;
         end if;
      end Parse;

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
         --  Filters recorded for this variable.

         ---------------
         -- Translate --
         ---------------

         function Translate (Var : in Tag) return String is

            function Vect_List (A : in Association) return String;
            --  Returns the Vector_Tag for the Association as a String, each
            --  value is separated by the given separator.

            function Mat_List (A : in Association) return String;
            --  Returns the Matrix_Tag as a string. If Matrix_Tag is not into
            --  a table, each Vector_Tag is convected using Vect_List and a LF
            --  is inserted between each rows. If the Matrix_Tag is into a
            --  table of level 1, it returns only the Vector_Tag (converted
            --  using Vect_List) for the current table line.

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

         begin
            for K in Translations'Range loop
               if Var.Name = Translations (K).Variable then

                  declare
                     Tk : constant Association := Translations (K);
                  begin
                     case Tk.Kind is

                        when Std =>
                           return Translate (Var, To_String (Tk.Value));

                        when Vect =>
                           if State.Table_Level = 0 then
                              --  This is a vector tag (outside of a
                              --  table tag statement), we display it as
                              --  a list separated by the specified
                              --  separator.
                              return Translate (Var, Vect_List (Tk));
                           else
                              return Translate
                                (Var, Field (Tk.Vect_Value, State.J));
                           end if;

                        when Matrix =>
                           if State.Table_Level in 0 .. 1 then
                              --  This is a matrix tag (outside of a
                              --  level 2 table tag statement), convert
                              --  it using Mat_List.
                              return Translate (Var, Mat_List (Tk));

                           else
                              return Translate (Var,
                                                Field (Tk.Mat_Value,
                                                       State.I, State.J));
                           end if;
                     end case;
                  end;
               end if;
            end loop;

            --  check now for an internal tag

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
                  D : Data.Tree := T;
               begin
                  while D /= null loop

                     if D.Kind = Data.Var then

                        for K in Translations'Range loop
                           declare
                              Tk : constant Association := Translations (K);
                           begin
                           if D.Var.Name = Translations (K).Variable then

                              if N = 1 then
                              --  First block level analysed.

                              if Tk.Kind = Vect then
                                 --  This is a Vector tag into a top level
                                 --  table statement. The number of iterations
                                 --  for this table statement correspond to
                                 --  the number of item into the vector.
                                 Iteration :=
                                   Natural'Max (Iteration,
                                                Size (Tk.Vect_Value));

                              elsif Tk.Kind = Matrix then

                                 if State.Table_Level = 0 then
                                    --  This is Matrix tag into a top level
                                    --  table statement. The number of
                                    --  iterations for this table statement
                                    --  correspond to the number of vector
                                    --  into the table.
                                    Iteration :=
                                      Natural'Max (Iteration,
                                                   Size (Tk.Mat_Value));
                                 else
                                    --  This is Matrix tag into an embbeded
                                    --  table statement (table statement into
                                    --  a table statement). The number of
                                    --  iterations for this table statement
                                    --  correspond to the smallest number of
                                    --  items in the Matrix tag's vectors.
                                    declare
                                       P : Matrix_Tag_Node_Access
                                         := Translations (K).Mat_Value.M.Head;
                                    begin
                                       while not (P = null) loop
                                          Iteration :=
                                            Natural'Max (Iteration,
                                                         Size (P.Vect));
                                          P := P . Next;
                                       end loop;
                                    end;
                                 end if;
                              end if;

                           elsif N = 2 then
                              --  Second block level analysed.

                              if Tk.Kind = Matrix then
                                 --  This is a Matrix tag into an embedded
                                 --  table statement (table statement into a
                                 --  table statement) analysed at the second
                                 --  block level. This is to report the number
                                 --  of iterations for upper level table
                                 --  statement. This number of iterations
                                 --  correspond to the smallest number of
                                 --  vectors into the table.
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
                        & " at line" & Natural'Image (T.Line) & ' '
                        & Exceptions.Exception_Information (E) & '.');
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
      procedure Free is new Ada.Unchecked_Deallocation (Node, Tree);
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
               Release (T.File.C_Info);
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
