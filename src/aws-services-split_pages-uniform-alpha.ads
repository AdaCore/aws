------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                            Copyright (C) 2004                            --
--                                ACT-Europe                                --
--                                                                          --
--  Authors: Dmitriy Anisimkov - Pascal Obry                                --
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

--  $RCSfile$
--  $Revision$ $Date$ $Author$

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

   type Splitter is new Uniform.Splitter with private;

   function Get_Page_Ranges
     (This  : in Splitter;
      Table : in Templates.Translate_Set)
      return Ranges_Table;

   function Get_Translations
     (This   : in Splitter;
      Page   : in Positive;
      URIs   : in URI_Table;
      Ranges : in Ranges_Table)
      return Templates.Translate_Set;

   procedure Set_Key (This : in out Splitter; Key : in String);
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
