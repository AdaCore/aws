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

with Ada.Finalization;
with Ada.Strings.Unbounded;

package Templates_Parser is

   use Ada.Strings.Unbounded;

   Template_Error : exception;

   Default_Begin_Tag : constant String := "@_";
   Default_End_Tag   : constant String := "_@";

   Default_Separator : constant String := ", ";

   procedure Set_Tag_Separators
     (Start_With : in String := Default_Begin_Tag;
      Stop_With  : in String := Default_End_Tag);
   --  Set the tag separators for the whole session. This should be changed as
   --  the very first API call and should not be changed after.

   ----------------
   -- Vector Tag --
   ----------------

   type Vector_Tag is private;
   --  A vector tag is a set of string.

   function "+" (Value : in String) return Vector_Tag;
   --  Vector_Tag constructor.

   function "+" (Value : in Character) return Vector_Tag;
   --  Vector_Tag constructor.

   function "+" (Value : in Boolean) return Vector_Tag;
   --  Vector_Tag constructor.

   function "+" (Value : in Unbounded_String) return Vector_Tag;
   --  Vector_Tag constructor.

   function "+" (Value : in Integer) return Vector_Tag;
   --  Vector_Tag constructor.

   function "&"
     (Vect  : in Vector_Tag;
      Value : in String)
     return Vector_Tag;
   --  Add Value at the end of the vector tag set.

   function "&"
     (Vect  : in Vector_Tag;
      Value : in Character)
     return Vector_Tag;
   --  Add Value at the end of the vector tag set.

   function "&"
     (Vect  : in Vector_Tag;
      Value : in Boolean)
     return Vector_Tag;
   --  Add Value (either string TRUE or FALSE) at the end of the vector tag
   --  set.

   function "&"
     (Vect  : in Vector_Tag;
      Value : in Unbounded_String)
     return Vector_Tag;
   --  Add Value at the end of the vector tag set.

   function "&"
     (Vect  : in Vector_Tag;
      Value : in Integer)
     return Vector_Tag;
   --  Add Value (converted to a String) at the end of the vector tag set.

   procedure Clear (Vect : in out Vector_Tag);
   --  Removes all values in the vector tag.

   function Size (Vect : in Vector_Tag) return Natural;
   --  Returns the number of value into Vect.

   ----------------
   -- Matrix Tag --
   ----------------

   type Matrix_Tag is private;

   function "+" (Vect : in Vector_Tag) return Matrix_Tag;
   --  Matrix_Tag constructor.

   function "&"
     (Matrix : in Matrix_Tag;
      Vect   : in Vector_Tag)
     return Matrix_Tag;
   --  Returns Matrix with Vect added to the end.

   function Size (Matrix : in Matrix_Tag) return Natural;
   --  Returns the number of Vector_Tag inside the Matrix.

   function Vector (Matrix : in Matrix_Tag; N : in Positive) return Vector_Tag;
   --  Returns Nth Vector_Tag in the Matrix.

   -----------------------
   -- Association table --
   -----------------------

   type Association is private;

   type Translate_Table is array (Positive range <>) of Association;

   No_Translation : constant Translate_Table;

   function Assoc
     (Variable  : in String;
      Value     : in String)
     return Association;
   --  Build an Association (Variable = Value) to be added to a
   --  Translate_Table. This is a standard association, value is a string.

   function Assoc
     (Variable  : in String;
      Value     : in Unbounded_String)
     return Association;
   --  Build an Association (Variable = Value) to be added to a
   --  Translate_Table. This is a standard association, value is an
   --  Unbounded_String.

   function Assoc
     (Variable  : in String;
      Value     : in Integer)
     return Association;
   --  Build an Association (Variable = Value) to be added to a
   --  Translate_Table. This is a standard association, value is an Integer.
   --  It will be displayed without leading space if positive.

   function Assoc
     (Variable  : in String;
      Value     : in Boolean)
     return Association;
   --  Build an Association (Variable = Value) to be added to a
   --  Translate_Table. It set the variable to TRUE or FALSE depending on
   --  value.

   function Assoc
     (Variable  : in String;
      Value     : in Vector_Tag;
      Separator : in String     := Default_Separator)
     return Association;
   --  Build an Association (Variable = Value) to be added to a
   --  Translate_Table. This is a vector tag association, value is a
   --  Vector_Tag. If the vector tag is found outside a table tag statement
   --  it is returned as a single string, each value beeing separater by the
   --  specified separator.

   function Assoc
     (Variable  : in String;
      Value     : in Matrix_Tag;
      Separator : in String     := Default_Separator)
     return Association;
   --  Build an Association (Variable = Value) to be added to a
   --  Translate_Table. This is a matrix tag association, value is a
   --  Matrix_Tag. If the matrix tag is found outside of a 2nd level table tag
   --  statement, Separator is used to build string representation of the
   --  matrix tag's vectors.

   -----------------------------
   -- Parsing and Translating --
   -----------------------------

   function Parse
     (Filename     : in String;
      Translations : in Translate_Table := No_Translation;
      Cached       : in Boolean         := False)
     return String;
   --  Parse the Template_File replacing variables' occurences by the
   --  corresponding values. If Cached is set to True, Filename tree will be
   --  recorded into a cache to quick retrieval.

   function Translate
     (Template     : in String;
      Translations : in Translate_Table := No_Translation)
     return String;
   --  Just translate the variable in the Template using the Translations
   --  table. This function does not parse the command tag (TABLE, IF,
   --  INCLUDE).

   procedure Print_Tree (Filename : in String);
   --  Use for debugging purpose only, it will output the internal tree
   --  representation.

private

   ------------------
   --  Vector Tags --
   ------------------

   type Vector_Tag_Node;
   type Vector_Tag_Node_Access is access Vector_Tag_Node;

   type Vector_Tag_Node is record
      Value : Unbounded_String;
      Next  : Vector_Tag_Node_Access;
   end record;

   type Integer_Access is access Integer;

   type Vector_Tag is new Ada.Finalization.Controlled with record
      Ref_Count : Integer_Access;
      Count     : Natural;
      Head      : Vector_Tag_Node_Access;
      Last      : Vector_Tag_Node_Access;
   end record;

   type Vector_Tag_Access is access Vector_Tag;

   procedure Initialize (V : in out Vector_Tag);
   procedure Finalize   (V : in out Vector_Tag);
   procedure Adjust     (V : in out Vector_Tag);

   ------------------
   --  Matrix Tags --
   ------------------

   type Matrix_Tag_Node;

   type Matrix_Tag_Node_Access is access Matrix_Tag_Node;

   type Matrix_Tag_Node is record
      Vect : Vector_Tag;
      Next : Matrix_Tag_Node_Access;
   end record;

   type Matrix_Tag_Int is new Ada.Finalization.Controlled with record
      Ref_Count : Integer_Access;
      Count     : Natural;
      Head      : Matrix_Tag_Node_Access;
      Last      : Matrix_Tag_Node_Access;
   end record;

   type Matrix_Tag is record
      M : Matrix_Tag_Int;
   end record;

   procedure Initialize (M : in out Matrix_Tag_Int);
   procedure Finalize   (M : in out Matrix_Tag_Int);
   procedure Adjust     (M : in out Matrix_Tag_Int);

   ------------------
   --  Association --
   ------------------

   type Var_Kind is (Std, Vect, Matrix);

   type Association (Kind : Var_Kind := Std) is record
      Variable  : Unbounded_String;

      case Kind is
         when Std =>
            Value : Unbounded_String;

         when Vect =>
            Vect_Value : Vector_Tag;
            Separator  : Unbounded_String;

         when Matrix =>
            Mat_Value        : Matrix_Tag;
            Column_Separator : Unbounded_String;
      end case;
   end record;

   No_Translation : constant Translate_Table
     := (2 .. 1 => Association'(Std,
                                Null_Unbounded_String,
                                Null_Unbounded_String));

end Templates_Parser;
