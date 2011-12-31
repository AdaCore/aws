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

package AWS.Services.Split_Pages.Alpha is

   --  Split in (at most) 28 pages, one for empty fields, one for all fields
   --  that start with a digit, and one for each different initial letter.
   --  Note that leading spaces in the key field are ignored; this means that a
   --  key field containing only spaces is treated as an empty field.
   --  The key field is set by calling Set_Key. If no key is defined, or no
   --  corresponding association is found in Table, or the association is not a
   --  vector, Splitter_Error is raised.
   --  The key field must be sorted, and all values must be empty or start with
   --  a digit or letter (case ignored). Otherwise, Splitter_Error is raised.
   --  Letters that do not appear in the key field are associated to the empty
   --  string; an Href can be specified instead by calling Set_Default_Href.
   --
   --  Tags:
   --  NEXT          The href to the next page.
   --  PREVIOUS      The href to the previous page.
   --  FIRST         The href to the first page.
   --  LAST          The href to the last page.
   --  PAGE_INDEX    Position of the current page in the INDEXES_V vector
   --  HREFS_V       A vector tag containing a set of href to pages, or "" if
   --                their is no page for the corresponding letter.
   --  INDEXES_V     A vector tag (synchronized with HREFS_V) containing ' '
   --                and the letters 'A' .. 'Z'
   --
   --  HREFS_V and INDEXES_V can be used to create an index to the generated
   --  pages.

   Splitter_Error : exception renames Split_Pages.Splitter_Error;

   type Splitter is new Split_Pages.Splitter with private;

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

   procedure Set_Default_Href (This : in out Splitter; Href : String);
   --  Href to use for letter having no entry in the key, if not specified the
   --  empty string is used.

private

   type Splitter is new Split_Pages.Splitter with record
      Key          : Unbounded_String;
      Default_Href : Unbounded_String;
      Index        : Lines_Table;
      HREFS_V      : Templates.Tag;
      INDEXES_V    : Templates.Tag;
   end record;

end AWS.Services.Split_Pages.Alpha;
