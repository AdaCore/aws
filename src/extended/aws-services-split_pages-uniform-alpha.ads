------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2004-2012, AdaCore                     --
--                                                                          --
--  This library is free software;  you can redistribute it and/or modify   --
--  it under terms of the  GNU General Public License  as published by the  --
--  Free Software  Foundation;  either version 3,  or (at your  option) any --
--  later version. This library is distributed in the hope that it will be  --
--  useful, but WITHOUT ANY WARRANTY;  without even the implied warranty of --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                    --
--                                                                          --
--  As a special exception under Section 7 of GPL version 3, you are        --
--  granted additional permissions described in the GCC Runtime Library     --
--  Exception, version 3.1, as published by the Free Software Foundation.   --
--                                                                          --
--  You should have received a copy of the GNU General Public License and   --
--  a copy of the GCC Runtime Library Exception along with this program;    --
--  see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see   --
--  <http://www.gnu.org/licenses/>.                                         --
--                                                                          --
--  As a special exception, if other files instantiate generics from this   --
--  unit, or you link this unit with other files to produce an executable,  --
--  this  unit  does not  by itself cause  the resulting executable to be   --
--  covered by the GNU General Public License. This exception does not      --
--  however invalidate any other reasons why the executable file  might be  --
--  covered by the  GNU Public License.                                     --
------------------------------------------------------------------------------

package AWS.Services.Split_Pages.Uniform.Alpha is

   --  Same as the uniform splitter, but builds in addition an alphabetical
   --  secondary index from a key field.
   --  For the references from the index to work, each line of the @@TABLE@@
   --  statement must include the following:
   --    <a name="@_TABLE_LINE_@>
   --  The alphabetical index will include one entry for empty fields, one
   --  entry for all fields that start with a digit, and one entry for each
   --  different initial letter.
   --  Note that leading spaces in the key field are ignored; this means that a
   --  key field containing only spaces is treated as an empty field.
   --  The key field is set by calling Set_Key. If no key is defined, or no
   --  corresponding association is found in Table, or the association is not a
   --  vector, Splitter_Error is raised.
   --  The key field must be sorted, and all values must be empty or start with
   --  a digit or letter (case ignored). Otherwise, Splitter_Error is raised.
   --
   --  Tags (in addition to those of the uniform splitter):
   --  S_HREFS_V     A vector tag containing a set of href to pages in the form
   --                <page>#<line>.
   --  S_INDEXES_V   A vector tag (synchronized with S_HREFS_V) containing
   --                "<>", "0..9", and the letters 'A' .. 'Z'
   --
   --  HREFS_V and INDEXES_V can be used to create an index to the generated
   --  pages. S_HREFS_V and S_INDEXES_V can be used to create a secondary
   --  alphabetical index that points directly to the corresponding element.

   Splitter_Error : exception renames Split_Pages.Splitter_Error;

   type Splitter is new Uniform.Splitter with private;

   overriding function Get_Page_Ranges
     (This  : Splitter;
      Table : Templates.Translate_Set) return Ranges_Table;

   overriding function Get_Translations
     (This   : Splitter;
      Page   : Positive;
      URIs   : URI_Table;
      Ranges : Ranges_Table) return Templates.Translate_Set;

   procedure Set_Key (This : in out Splitter; Key : String);
   --  Set the key field, this is the name of the vector association in the
   --  translate_set that will be used to create the index.

private

   type Splitter is new Uniform.Splitter with record
      Key         : Unbounded_String;
      Lines       : Lines_Table;
      S_HREFS_V   : Templates.Vector_Tag;
      S_INDEXES_V : Templates.Vector_Tag;
   end record;

end AWS.Services.Split_Pages.Uniform.Alpha;
