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

with Ada.Finalization;
with Ada.Strings.Unbounded;

package Templates_Parser is

   Template_Error : exception;

   type Template_File is private;

   Max_Template_Lines : constant := 5_000;
   --  maximum number of lines a template file can have.

   Default_Begin_Tag  : constant String    := "@@_";
   Default_End_Tag    : constant String    := "_@@";
   Default_Separator  : constant Character := '|';

   type Association is private;

   type Translate_Table is array (Positive range <>) of Association;

   No_Translation : constant Translate_Table;

   function Assoc (Variable  : in String;
                   Value     : in String;
                   Is_Vector : in Boolean   := False;
                   Begin_Tag : in String    := Default_Begin_Tag;
                   End_Tag   : in String    := Default_End_Tag;
                   Separator : in Character := Default_Separator)
                  return Association;
   --  build an Association to be added to a Translate_Table. Is_Vector is
   --  set to true to build a vector variable. Separator can be used to
   --  change the character used between values of a vector variable. If
   --  Is_Vector is false then Separator is ignored.

   function Assoc (Variable  : in String;
                   Value     : in Boolean;
                   Begin_Tag : in String    := Default_Begin_Tag;
                   End_Tag   : in String    := Default_End_Tag)
                  return Association;
   --  build an Association to be added to a Translate_Table. It set an assoc
   --  for variable to "TRUE" if value is true and "FALSE" otherwise.

   function Parse (Template_Filename : in String;
                   Translations      : in Translate_Table := No_Translation)
                  return String;
   --  parse the Template_File replacing variables' occurences by the
   --  corresponding values.

   function Parse (Template     : in Template_File;
                   Translations : in Translate_Table := No_Translation)
                  return String;
   --  parse the Template replacing variables' occurences by the
   --  corresponding values.

   function Translate (Template     : in String;
                       Translations : in Translate_Table := No_Translation)
                      return String;
   --  just translate the variable in the Template using the Translations
   --  table. This function does not parse the command tag (TABLE, IF,
   --  INCLUDE).

   function Open (Template_Filename : in String)
                 return Template_File;
   --  open a template file on disk and create an in-memory template to be
   --  parsed later.

private

   use Ada.Strings.Unbounded;

   type Association is
      record
         Variable  : Unbounded_String;
         Value     : Unbounded_String;
         Separator : Character;
         Vector    : Boolean := False;
      end record;

   No_Translation : constant Translate_Table
     := (2 .. 1 => Association'(Null_Unbounded_String,
                                Null_Unbounded_String,
                                ASCII.Nul,
                                False));

   subtype Line_Index is Natural range 0 .. Max_Template_Lines;

   type Template_Content is array (Positive range <>) of Unbounded_String;
   type Template_Lines is access Template_Content;

   type Counter is access Natural;

   type Template_File is new Ada.Finalization.Controlled with
      record
         Count    : Counter;
         Filename : Unbounded_String;
         Lines    : Template_Lines;
      end record;

   procedure Initialize (Template : in out Template_File);
   procedure Finalize (Template : in out Template_File);
   procedure Adjust (Template : in out Template_File);

end Templates_Parser;
