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

package AWS.Services.Split_Pages.Alpha.Bounded is

   --  Same as the alpha splitter, but pages larger than Max_Per_Page are
   --  further splitted.
   --  A secondary index is generated that gives the various pages for a given
   --  letter.
   --
   --  Tags (in addition to those of the alpha splitter):
   --  S_NEXT        The href to the next page.
   --  S_PREVIOUS    The href to the previous page.
   --  S_FIRST       The href to the first page.
   --  S_LAST        The href to the last page.
   --  S_PAGE_INDEX  Position of the current page in the S_INDEXES_V vector
   --                Note that for this splitter, this is also the page number.
   --  S_HREFS_V     A vector tag containing a set of href to the different
   --                pages for the current letter.
   --  S_INDEXES_V   A vector tag (synchronized with S_HREFS_V) containing the
   --                page numbers for the hrefs.
   --
   --  HREFS_V and INDEXES_V can be used to create an index to the generated
   --  pages. S_HREFS_V and S_INDEXES_V can be used to create a secondary
   --  alphabetical index that points directly to the corresponding element.

   type Splitter (Max_Per_Page : Positive) is new Alpha.Splitter with private;

   overriding function Get_Page_Ranges
     (This  : Splitter;
      Table : Templates.Translate_Set) return Ranges_Table;

   overriding function Get_Translations
     (This   : Splitter;
      Page   : Positive;
      URIs   : URI_Table;
      Ranges : Ranges_Table) return Templates.Translate_Set;

private

   type Splitter (Max_Per_Page : Positive) is new Alpha.Splitter with record
      Index_Last  : Lines_Table;
   end record;

end AWS.Services.Split_Pages.Alpha.Bounded;
