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

   Template_Error : exception;

   Max_Template_Lines : constant := 5_000;
   --  maximum number of lines a template file can have.

   Default_Begin_Tag : constant String := "@@_";
   Default_End_Tag   : constant String := "_@@";
   Default_Separator : constant String := ", ";

   --
   --  Vector Tag
   --

   type Vector_Tag is private;
   --  a vector tag is a set of string.

   function "+" (Value : in String) return Vector_Tag;
   --  Vector_Tag constructor.

   function "&"
     (Vect  : in Vector_Tag;
      Value : in String)
     return Vector_Tag;
   --  add Value at the end of the vector tag set.

   function "&"
     (Vect  : in Vector_Tag;
      Value : in Character)
     return Vector_Tag;
   --  add Value at the end of the vector tag set.

   function "&"
     (Vect  : in Vector_Tag;
      Value : in Boolean)
     return Vector_Tag;
   --  add Value (either string TRUE or FALSE) at the end of the vector tag
   --  set.

   function "&"
     (Vect  : in Vector_Tag;
      Value : in Ada.Strings.Unbounded.Unbounded_String)
     return Vector_Tag;
   --  add Value at the end of the vector tag set.

   function "&"
     (Vect  : in Vector_Tag;
      Value : in Integer)
     return Vector_Tag;
   --  add Value (converted to a String) at the end of the vector tag set.

   --
   --  Association table
   --

   type Association is private;

   type Translate_Table is array (Positive range <>) of Association;

   No_Translation : constant Translate_Table;

   function Assoc
     (Variable  : in String;
      Value     : in String;
      Begin_Tag : in String    := Default_Begin_Tag;
      End_Tag   : in String    := Default_End_Tag)
     return Association;
   --  build an Association (Variable = Value) to be added to a
   --  Translate_Table. This is a standard association, value is a string.

   function Assoc
     (Variable  : in String;
      Value     : in Ada.Strings.Unbounded.Unbounded_String;
      Begin_Tag : in String    := Default_Begin_Tag;
      End_Tag   : in String    := Default_End_Tag)
     return Association;
   --  build an Association (Variable = Value) to be added to a
   --  Translate_Table. This is a standard association, value is an
   --  Unbounded_String.

   function Assoc
     (Variable  : in String;
      Value     : in Integer;
      Begin_Tag : in String    := Default_Begin_Tag;
      End_Tag   : in String    := Default_End_Tag)
     return Association;
   --  build an Association (Variable = Value) to be added to a
   --  Translate_Table. This is a standard association, value is an Integer.
   --  It will be displayed without leading space if positive.

   function Assoc
     (Variable  : in String;
      Value     : in Vector_Tag;
      Separator : in String     := Default_Separator;
      Begin_Tag : in String     := Default_Begin_Tag;
      End_Tag   : in String     := Default_End_Tag)
     return Association;
   --  build an Association (Variable = Value) to be added to a
   --  Translate_Table. This is a vector tag association, value is a
   --  Vector_Tag. If the vector tag is found outside a table tag statement
   --  it is returned as a single string, each value beeing separater by the
   --  specified separator.

   function Assoc
     (Variable  : in String;
      Value     : in Boolean;
      Begin_Tag : in String    := Default_Begin_Tag;
      End_Tag   : in String    := Default_End_Tag)
     return Association;
   --  build an Association (Variable = Value) to be added to a
   --  Translate_Table. It set the variable to TRUE or FALSE depending on
   --  value.

   --
   --  Parsing and Translating
   --

   type Template_File is private;

   function Parse
     (Template_Filename : in String;
      Translations      : in Translate_Table := No_Translation)
     return String;
   --  parse the Template_File replacing variables' occurences by the
   --  corresponding values.

   function Parse
     (Template     : in Template_File;
      Translations : in Translate_Table := No_Translation)
     return String;
   --  parse the Template replacing variables' occurences by the
   --  corresponding values.

   function Translate
     (Template     : in String;
      Translations : in Translate_Table := No_Translation)
     return String;
   --  just translate the variable in the Template using the Translations
   --  table. This function does not parse the command tag (TABLE, IF,
   --  INCLUDE).

   function Open
     (Template_Filename : in String)
     return Template_File;
   --  open a template file on disk and create an in-memory template to be
   --  parsed later.

private

   use Ada.Strings.Unbounded;

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

   procedure Initialize (V : in out Vector_Tag);
   procedure Finalize   (V : in out Vector_Tag);
   procedure Adjust     (V : in out Vector_Tag);

   ------------------
   --  Association --
   ------------------

   type Var_Kind is (Std, Vect);

   type Association (Kind : Var_Kind := Std) is record
      Variable  : Unbounded_String;

      case Kind is
         when Std =>
            Value : Unbounded_String;
         when Vect =>
            Vect_Value : Vector_Tag;
            Separator  : Unbounded_String;
      end case;
   end record;

   No_Translation : constant Translate_Table
     := (2 .. 1 => Association'(Std,
                                Null_Unbounded_String,
                                Null_Unbounded_String));

   -------------------
   -- Template_File --
   -------------------

   subtype Line_Index is Natural range 0 .. Max_Template_Lines;

   type Template_Content is array (Positive range <>) of Unbounded_String;
   type Template_Lines is access Template_Content;

   type Counter is access Natural;

   type Template_File is new Ada.Finalization.Controlled with record
      Count    : Counter;
      Filename : Unbounded_String;
      Lines    : Template_Lines;
   end record;

   procedure Initialize (Template : in out Template_File);
   procedure Finalize   (Template : in out Template_File);
   procedure Adjust     (Template : in out Template_File);

end Templates_Parser;
