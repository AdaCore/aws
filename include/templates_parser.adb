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
with Ada.Calendar;
with Ada.Text_IO;
with Ada.Strings.Fixed;
with Ada.Strings.Maps.Constants;
with Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;

with GNAT.Calendar.Time_IO;
with GNAT.OS_Lib;

package body Templates_Parser is

   use Ada;
   use Ada.Exceptions;
   use Ada.Strings;

   Internal_Error : exception;

   Begin_Tag : Unbounded_String := To_Unbounded_String (Default_Begin_Tag);
   End_Tag   : Unbounded_String := To_Unbounded_String (Default_End_Tag);

   Table_Token              : constant String := "@@TABLE@@";
   Terminate_Sections_Token : constant String := "@@TERMINATE_SECTIONS@@";
   Section_Token            : constant String := "@@SECTION@@";
   End_Table_Token          : constant String := "@@END_TABLE@@";
   If_Token                 : constant String := "@@IF@@";
   Else_Token               : constant String := "@@ELSE@@";
   End_If_Token             : constant String := "@@END_IF@@";
   Include_Token            : constant String := "@@INCLUDE@@";

   Filter_Reverse_Token     : aliased constant String := "REVERSE";
   Filter_Lower_Token       : aliased constant String := "LOWER";
   Filter_Upper_Token       : aliased constant String := "UPPER";
   Filter_Clean_Text_Token  : aliased constant String := "CLEAN_TEXT";
   Filter_No_Space_Token    : aliased constant String := "NO_SPACE";
   Filter_Capitalize_Token  : aliased constant String := "CAPITALIZE";
   Filter_Yes_No_Token      : aliased constant String := "YES_NO";
   Filter_Oui_Non_Token     : aliased constant String := "OUI_NON";
   Filter_Exist_Token       : aliased constant String := "EXIST";
   Filter_Is_Empty_Token    : aliased constant String := "IS_EMPTY";

   subtype Table_Range     is Positive range Table_Token'Range;
   subtype Section_Range   is Positive range Section_Token'Range;
   subtype End_Table_Range is Positive range End_Table_Token'Range;
   subtype If_Range        is Positive range If_Token'Range;
   subtype Else_Range      is Positive range Else_Token'Range;
   subtype End_If_Range    is Positive range End_If_Token'Range;
   subtype Include_Range   is Positive range Include_Token'Range;

   Blank : constant Maps.Character_Set := Maps.To_Set (' ' & ASCII.HT);

   ----------------------
   --  Filters setting --
   ----------------------

   --  a filter appear just before a tag variable (e.g. @@LOWER@@_SOME_VAT_@@
   --  and means that the filter LOWER should be applied to SOME_VAR before
   --  replacing it in the template file.

   type Filters_Mode is
     (Invert,     -- reverse string
      Lower,      -- lower case
      Upper,      -- upper case
      Capitalize, -- lower case except char before spaces and underscores
      Clean_Text, -- only letter/digits all other chars are changed to spaces.
      No_Space,   -- removes all spaces found in the value.
      Yes_No,     -- if True return Yes, If False returns No, else do nothing.
      Oui_Non,    -- if True return Oui, If False returns Non, else do nothing.
      Exist,      -- return "TRUE" if var is not empty and "FALSE" otherwise.
      Is_Empty    -- return "TRUE" if var is empty and "FALSE" otherwise.
      );

   type Filter_Function is access function (S : in String) return String;

   type Filter_Set is array (Positive range <>) of Filter_Function;
   type Filter_Set_Access is access Filter_Set;

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
   function No_Space_Filter   (S : in String) return String;
   function Yes_No_Filter     (S : in String) return String;
   function Oui_Non_Filter    (S : in String) return String;
   function Exist_Filter      (S : in String) return String;
   function Is_Empty_Filter   (S : in String) return String;

   function Filter_Handle (Name : in String) return Filter_Function;
   --  Returns the filter function for the given filter name.

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

   function Image (T : in Tag) return String is
      R : Unbounded_String;
   begin
      R := Begin_Tag;

      if T.Filters /= null then
         for K in reverse T.Filters'Range loop
            Append (R, Filter_Name (T.Filters (K)));
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
               FS (K) := Filter_Handle
                 (Tag (Tag'First + Length (Begin_Tag) .. Stop - 1));
            else
               FS (K) := Filter_Handle (Tag (Start + 1 .. Stop - 1));
            end if;

            K := K + 1;

            Stop := Stop - 1;
         end loop;

         pragma Assert (K = 1);

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
               R := To_Unbounded_String (T.Filters (K) (To_String (R)));
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

      type Ops is (O_And, O_Or, O_Xor, O_Sup, O_Inf, O_Esup, O_Einf, O_Equal);

      function Image (O : in Ops) return String;
      --  Returns Ops string representation.

      function Value (O : in String) return Ops;
      --  Returns Ops from its string representation. Raises Templates_Error if
      --  the token is not a known operation.

      type Node;
      type Tree is access Node;

      type NKind is (Value, Var, Op);

      type Node (Kind : NKind) is record
         case Kind is
            when Value =>
               V   : Unbounded_String;

            when Var =>
               Var : Tag;

            when Op =>
               O           : Ops;
               Left, Right : Tree;
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
                  Text,          --  this is a text line
                  If_Stmt,       --  an IF tag statement
                  Table_Stmt,    --  a TABLE tag statement
                  Section_Stmt,  --  a TABLE section
                  Include_Stmt); --  an INCLUDE tag statement

   --  A template line is coded as a suite of Data and Var element.

   type Node;
   type Tree is access Node;

   type Node (Kind : Nkind) is record
      Next : Tree;

      case Kind is
         when Info =>
            Filename  : Unbounded_String;
            Timestamp : GNAT.OS_Lib.OS_Time;
            Ref       : Natural := 0;
            I_File    : Tree;

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
            File : Tree;
      end case;
   end record;

   procedure Release (T : in out Tree);
   --  Release all memory associated with the tree.

   -------------------
   --  Cached Files --
   -------------------

   --  Cached_Files keep the Tree for a given file.

   package Cached_Files is

      protected Prot is

         procedure Add
           (Filename : in String;
            T        : in Tree);
         --  Add Filename/T to the list of cached files.

         function Get
           (Filename : in String;
            Load     : in Boolean)
           return Tree;
         --  Returns the Tree for Filename or null if Filename has not been
         --  cached. Load must be set to True at load stage and False at Parse
         --  stage.

      end Prot;

   end Cached_Files;

   ----------------
   -- Vector_Tag --
   ----------------

   function Field
     (Vect_Value : in Vector_Tag;
      N          : in Positive) return String;
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
      --  Here we just separated current vector from the new one. The memory
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

   package body Cached_Files is

      Initial_Size : constant := 20; -- cache initial size
      Growing_Size : constant := 50; -- cache growing size

      type File_Array is array (Positive range <>) of Tree;
      type File_Array_Access is access File_Array;

      Files : File_Array_Access;
      Index : Natural := 0;

      procedure Growth;
      --  Growth the size (by Growing_Size places) of Files array.

      function Get (Filename : in String) return Natural;
      --  Look for Filename into the set and return its index. Returns 0 if
      --  filename was not found.

      function Up_To_Date (T : in Tree) return Boolean;
      --  Returns True if the file tree is up to date (the templates files
      --  have not been modified on disk) or False otherwise.

      protected body Prot is

         ---------
         -- Add --
         ---------

         procedure Add
           (Filename : in String;
            T        : in Tree)
         is
            L_Filename : constant Unbounded_String
              := To_Unbounded_String (Filename);

            S : Natural := 1;
            E : Natural := Index;
            N : Natural;

            Old : Tree;

         begin
            if Files = null or else Index = Files'Last then
               Growth;
            end if;

            loop
               exit when S > E;

               N := (S + E) / 2;

               if Files (N).Filename = L_Filename then

                  --  This is a file that was already loaded. If loaded again
                  --  it is because the file timestamp has changed. We want to
                  --  just update the tree and not the info node.

                  Old := Files (N).Next;
                  --  This is a pointer to the tree, skiping the info node.

                  Files (N).Next      := T.Next;
                  Files (N).Timestamp := T.Timestamp;

                  Release (Old);

                  --  Nothing more to do in this case.

                  return;

               elsif Files (N).Filename < L_Filename then
                  S := N + 1;

               else
                  E := N - 1;
               end if;
            end loop;

            Files (S + 1 .. Index + 1) := Files (S .. Index);

            Index := Index + 1;

            Files (S) := T;
         end Add;

         ---------
         -- Get --
         ---------

         function Get
           (Filename : in String;
            Load     : in Boolean)
           return Tree
         is

            N : constant Natural := Get (Filename);

         begin
            if N = 0 then
               return null;

            else
               if Load then
                  Files (N).Ref := Files (N).Ref + 1;
               end if;

               return Files (N);
            end if;
         end Get;

      end Prot;

      ---------
      -- Get --
      ---------

      function Get (Filename : in String) return Natural is

         use type GNAT.OS_Lib.OS_Time;

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

               if Up_To_Date (Files (N)) then
                  return N;
               else
                  --  File has changed on disk, we need to read it again. Just
                  --  pretend that the file was not found.
                  return 0;
               end if;

            elsif Files (N).Filename < L_Filename then
               S := N + 1;

            else
               E := N - 1;
            end if;
         end loop;

         return 0;
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

      ----------------
      -- Up_To_Date --
      ----------------

      function Up_To_Date (T : in Tree) return Boolean is
         use type GNAT.OS_Lib.OS_Time;
         P : Tree;
      begin
         --  Check main file

         if GNAT.OS_Lib.File_Time_Stamp (To_String (T.Filename))
           /= T.Timestamp
         then
            return False;
         end if;

         --  Check all include files

         P := T.I_File;

         while P /= null loop
            if GNAT.OS_Lib.File_Time_Stamp (To_String (P.File.Filename))
              /= P.File.Timestamp
            then
               return False;
            end if;

            P := P.Next;
         end loop;

         return True;
      end Up_To_Date;

   end Cached_Files;

   ----------
   -- Data --
   ----------

   package body Data is

      -----------
      -- Parse --
      -----------

      function Parse (Line : in String) return Tree is

         Begin_Tag : constant String
           := To_String (Templates_Parser.Begin_Tag);

         End_Tag : constant String
           := To_String (Templates_Parser.End_Tag);

         function Build (Line : in String) return Tree;
         --  Recursive function to build the tree.

         -----------
         -- Build --
         -----------

         function Build (Line : in String) return Tree is
            Start, Stop : Natural;
         begin
            if Line = "" then
               return null;

            else
               Start := Strings.Fixed.Index (Line, Begin_Tag);

               if Start = 0 then
                  --  no more tag
                  return new Node'(Text,
                                   null,
                                   To_Unbounded_String (Line));
               else
                  Stop := Strings.Fixed.Index (Line, End_Tag);

                  if Stop = 0 then
                     Exceptions.Raise_Exception
                       (Internal_Error'Identity,
                        "Tag variable not terminated (missing "
                        & End_Tag & ")");

                  else
                     Stop := Stop + End_Tag'Length - 1;

                     if Start = Line'First then
                        return new Node'
                          (Var,
                           Build (Line (Stop + 1 .. Line'Last)),
                           Build (Line (Start .. Stop)));
                     else
                        return new Node'
                          (Text,
                           Build (Line (Start .. Line'Last)),
                           To_Unbounded_String
                             (Line (Line'First .. Start - 1)));
                     end if;
                  end if;
               end if;
            end if;
         end Build;

      begin
         return Build (Line);
      end Parse;

      ----------------
      -- Print_Tree --
      ----------------

      procedure Print_Tree (D : in Tree) is
      begin
         if D = null then
            return;
         end if;

         case D.Kind is
            when Text =>
               Text_IO.Put (To_String (D.Value));

            when Var =>
               Text_IO.Put (Image (D.Var));
         end case;

         Print_Tree (D.Next);
      end Print_Tree;

      -------------
      -- Release --
      -------------

      procedure Release (D : in out Tree) is

         procedure Free is new Ada.Unchecked_Deallocation (Node, Tree);

         P : Tree;
         T : Tree := D;
      begin
         while T /= null loop
            P := T;
            T := T.Next;

            if P.Kind = Var then
               Release (P.Var);
            end if;

            Free (P);
         end loop;
      end Release;

   end Data;

   ----------
   -- Expr --
   ----------

   package body Expr is

      -----------
      -- Image --
      -----------

      function Image (O : in Ops) return String is
      begin
         case O is
            when O_And   => return "and";
            when O_Or    => return "or";
            when O_Xor   => return "xor";
            when O_Sup   => return ">";
            when O_Inf   => return "<";
            when O_Esup  => return ">=";
            when O_Einf  => return "<=";
            when O_Equal => return "=";
         end case;
      end Image;

      -----------
      -- Parse --
      -----------

      function Parse (Expression : in String) return Tree is

         Index : Natural := Expression'First;

         function Get_Token return String;
         --  Returns next token. Set Index to the last analysed position in
         --  Expression.

         ---------------
         -- Get_Token --
         ---------------

         function Get_Token return String is
            use Strings;
            K, I  : Natural;
         begin
            if Index > Expression'Last then
               --  No more data to read.
               return "";
            end if;

            Index := Fixed.Index_Non_Blank
              (Expression (Index .. Expression'Last));

            if Index = 0 then
               --  There is only one token, return the whole string.
               Index := Expression'Last + 1;
               return Expression (Index .. Expression'Last);

            elsif Expression (Index) = '(' then
               --  This is a sub-expression, returns it.
               K := 0;

               declare
                  L : Natural := 1;
               begin
                  Look_For_Sub_Exp : for I in Index + 1 .. Expression'Last loop
                     if Expression (I) = '(' then
                        L := L + 1;
                     elsif Expression (I) = ')' then
                        K := I;
                        L := L - 1;
                     end if;

                     exit Look_For_Sub_Exp when L = 0;
                  end loop Look_For_Sub_Exp;
               end;

               if K = 0 then
                  --  No matching closing parenthesis.

                  Exceptions.Raise_Exception
                    (Internal_Error'Identity,
                     "condition, no matching parenthesis for parent at pos "
                     & Natural'Image (Index));

               else
                  I := Index;
                  Index := K + 1;
                  return Expression (I + 1 .. K - 1);
               end if;

            else
               --  We have found the start of a token, look for end of it.
               K := Fixed.Index (Expression (Index .. Expression'Last), Blank);

               if K = 0 then
                  --  Token end is the end of Expression.
                  I := Index;
                  Index := Expression'Last + 1;
                  return Expression (I .. Expression'Last);
               else
                  I := Index;
                  Index := K + 1;
                  return Expression (I .. K - 1);
               end if;
            end if;
         end Get_Token;

         L_Tok : constant String := Get_Token;
         O_Tok : constant String := Get_Token;
         R_Tok : constant String := Get_Token;

      begin
         if O_Tok = "" then
            --  No more operator, this is a leaf. It is either a variable or a
            --  value.

            if Strings.Fixed.Index (L_Tok, To_String (Begin_Tag)) = 0 then
               --  a value
               return new Node'(Value, To_Unbounded_String (L_Tok));

            else
               --  a variable
               return new Node'(Var, Build (L_Tok));
            end if;


         else
            if Index > Expression'Last then
               --  This is the latest token

               return new Node'(Op, Value (O_Tok),
                                Parse (L_Tok), Parse (R_Tok));

            else
               declare
                  NO_Tok : constant String := Get_Token;
               begin
                  return new Node'
                    (Op, Value (NO_Tok),
                     Parse (L_Tok & ' ' & O_Tok & ' ' & R_Tok),
                     Parse (Expression (Index .. Expression'Last)));
               end;
            end if;
         end if;
      end Parse;

      ----------------
      -- Print_Tree --
      ----------------

      procedure Print_Tree (E : in Tree) is
      begin
         case E.Kind is
            when Value =>
               Text_IO.Put (To_String (E.V));

            when Var =>
               Text_IO.Put (Image (E.Var));

            when Op =>
               Text_IO.Put ('(');
               Print_Tree (E.Left);
               Text_IO.Put (' ' & Image (E.O) & ' ');
               Print_Tree (E.Right);
               Text_IO.Put (')');
         end case;
      end Print_Tree;

      -------------
      -- Release --
      -------------

      procedure Release (E : in out Tree) is
         procedure Free is new Ada.Unchecked_Deallocation (Node, Tree);
      begin
         case E.Kind is
            when Value =>
               null;

            when Var =>
               Release (E.Var);

            when Op =>
               Release (E.Left);
               Release (E.Right);
               Free (E);
         end case;
      end Release;

      -----------
      -- Value --
      -----------

      function Value (O : in String) return Ops is
      begin
         if O = "and" then
            return O_And;

         elsif O = "or" then
            return O_Or;

         elsif O = "xor" then
            return O_Xor;

         elsif O = ">" then
            return O_Sup;

         elsif O = "<" then
            return O_Inf;

         elsif O = ">=" then
            return O_Esup;

         elsif O = "<=" then
            return O_Einf;

         elsif O = "=" then
            return O_Equal;

         else
            Exceptions.Raise_Exception
              (Internal_Error'Identity, "condition, unknown operator " & O);
         end if;
      end Value;

   end Expr;

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

      Clean_Set : constant Strings.Maps.Character_Set
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

   ------------------
   -- Exist_Filter --
   ------------------

   function Exist_Filter (S : in String) return String is
   begin
      if S /= "" then
         return "TRUE";
      else
         return "FALSE";
      end if;
   end Exist_Filter;

   ---------------------
   -- Is_Empty_Filter --
   ---------------------

   function Is_Empty_Filter (S : in String) return String is
   begin
      if S = "" then
         return "TRUE";
      else
         return "FALSE";
      end if;
   end Is_Empty_Filter;

   ------------------
   -- Lower_Filter --
   ------------------

   function Lower_Filter (S : in String) return String is
   begin
      return Characters.Handling.To_Lower (S);
   end Lower_Filter;

   ---------------------
   -- No_Space_Filter --
   ---------------------

   function No_Space_Filter (S : in String) return String is
      Result : String (S'Range);
      L      : Natural := Result'First - 1;
   begin
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

   --  Filter Table

   Filter_Table : constant array (Filters_Mode) of Filter_Record
     := (Lower      =>
           (Filter_Lower_Token'Access,      Lower_Filter'Access),
         Upper      =>
           (Filter_Upper_Token'Access,      Upper_Filter'Access),
         Capitalize =>
           (Filter_Capitalize_Token'Access, Capitalize_Filter'Access),
         Clean_Text =>
           (Filter_Clean_Text_Token'Access, Clean_Text_Filter'Access),
         No_Space   =>
           (Filter_No_Space_Token'Access,   No_Space_Filter'Access),
         Invert     =>
           (Filter_Reverse_Token'Access,    Reverse_Filter'Access),
         Yes_No     =>
           (Filter_Yes_No_Token'Access,     Yes_No_Filter'Access),
         Oui_Non    =>
           (Filter_Oui_Non_Token'Access,    Oui_Non_Filter'Access),
         Exist      =>
           (Filter_Exist_Token'Access,      Exist_Filter'Access),
         Is_Empty   =>
           (Filter_Is_Empty_Token'Access,   Is_Empty_Filter'Access));

   -------------------
   -- Filter_Handle --
   -------------------

   function Filter_Handle (Name : in String) return Filter_Function is
   begin
      for K in Filter_Table'Range loop
         if Filter_Table (K).Name.all = Name then
            return Filter_Table (K).Handle;
         end if;
      end loop;

      Exceptions.Raise_Exception (Internal_Error'Identity,
                                  "Unknown filter " & Name);
   end Filter_Handle;

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

      Exceptions.Raise_Exception (Internal_Error'Identity,
                                  "Unknown filter handle");
   end Filter_Name;

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

   function Field
     (Mat_Value : in Matrix_Tag;
      I, J      : in Natural) return String
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
     (Filename : in String;
      Cached   : in Boolean := False)
     return Tree
   is

      File   : Text_IO.File_Type;  --  file beeing parsed.

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
      --  Returns True is Stmt is found at the begining of the current line
      --  ignoring leading blank characters.

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
         if Text_IO.End_Of_File (File) then
            return True;
         else
            Line := Line + 1;

            Text_IO.Get_Line (File, Buffer, Last);

            First := Strings.Fixed.Index_Non_Blank (Buffer (1 .. Last));

            if First = 0 then
               --  There is only spaces on this line, this is an empty line
               --  we just have to skip it.
               Last := 0;
            end if;

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

            T.Cond    := Expr.Parse (Get_All_Parameters);
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

            T.File     :=
              Load (Build_Include_Pathname (Get_First_Parameter), Cached);

            I_File     := new Node'(Include_Stmt, I_File, T.File);

            T.Next     := Parse (Mode);

            return T;

         else
            T := new Node (Text);

            T.Text := Data.Parse (Buffer (1 .. Last));

            T.Next := Parse (Mode);
            return T;
         end if;
      end Parse;

      T : Tree;

   begin
      if Cached then
         T := Cached_Files.Prot.Get (Filename, Load => True);

         if T /= null then
            return T;
         end if;
      end if;

      Text_IO.Open (File, Text_IO.In_File, Filename, Form => "shared=no");

      T := Parse (Parse_Std);

      --  Add first node (info about tree)
      T := new Node'(Info,
                     T,
                     To_Unbounded_String (Filename),
                     GNAT.OS_Lib.File_Time_Stamp (Filename),
                     1,
                     I_File);

      Text_IO.Close (File);

      if Cached then
         Cached_Files.Prot.Add (Filename, T);
      end if;

      return T;

   exception
      when E : Internal_Error =>
         Fatal_Error (Exceptions.Exception_Message (E));
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
         when Info =>
            Text_IO.Put_Line ("[INFO] " & To_String (T.Filename)
                              & Natural'Image (T.Ref));
            declare
               I : Tree := T.I_File;
            begin
               while I /= null loop
                  Text_IO.Put (" -> ");
                  Text_IO.Put_Line (To_String (I.File.Filename));
                  I := I.Next;
               end loop;
            end;

            Print_Tree (T.Next, Level);

         when Text =>
            Text_IO.Put ("[TEXT] ");
            Data.Print_Tree (T.Text);
            Text_IO.New_Line;
            Print_Tree (T.Next, Level);

         when If_Stmt  =>
            Text_IO.Put ("[IF_STMT] ");
            Expr.Print_Tree (T.Cond);
            Text_IO.New_Line;
            Print_Tree (T.N_True, Level + 1);
            Print_Indent (Level);
            Text_IO.Put_Line ("[ELSE]");
            Print_Tree (T.N_False, Level + 1);
            Print_Indent (Level);
            Text_IO.Put_Line ("[END_IF_STMT]");
            Print_Tree (T.Next, Level);

         when Table_Stmt =>
            Text_IO.Put_Line ("[TABLE_STMT] TERMINATE_SECTIONS="
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
            Text_IO.Put_Line ("[INCLUDE_STMT] " & To_String (T.File.Filename));
            Print_Tree (T.File, Level + 1);
            Print_Tree (T.Next, Level);
      end case;

   end Print_Tree;

   ----------------
   -- Print_Tree --
   ----------------

   procedure Print_Tree (Filename : in String) is
      T : Tree;
   begin
      T := Load (Filename);
      Print_Tree (T);
      Release (T);
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
                  Result := Result & P.Value;
                  for K in 2 .. V.Count loop
                     P := P.Next;
                     Append (Result, A.Column_Separator & P.Value);
                  end loop;
               end Add_Vector;

            begin
               if State.Table_Level = 0 then
                  --  A Matrix outside a table statement.

                  while P /= null loop
                     Add_Vector (P.Vect);
                     Append (Result, ASCII.LF);
                     P := P.Next;
                  end loop;

               else
                  Add_Vector (Vector (A.Mat_Value, State.J));
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

            return "";
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
                  Expr.O_Equal => F_Equ'Access);

         begin
            case E.Kind is
               when Expr.Value =>
                     return To_String (E.V);

               when Expr.Var =>
                  return Translate (E.Var);

               when Expr.Op =>
                  return Op_Table (E.O) (Analyze (E.Left), Analyze (E.Right));
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
                  when Info =>
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
                     return Natural'Max (Get_Max_Lines (T.File, N),
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

            when Info =>
               Analyze (T.Next, State);

            when Text =>
               Analyze (T.Text);

               Append (Results, ASCII.LF);

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
               Analyze (T.File, State);

               Analyze (T.Next, State);

         end case;
      end Analyze;

      T : Tree;

   begin
      T := Load (Filename, Cached);

      Now := Ada.Calendar.Clock;

      Analyze (T, Empty_State);

      if not Cached then
         Release (T);
      end if;

      return To_String (Results);
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
            T.File.Ref := T.File.Ref - 1;

            if T.File.Ref = 0 then
               --  No more reference to this include file we release it.
               Release (T.File);
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
