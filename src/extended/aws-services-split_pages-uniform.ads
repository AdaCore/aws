------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2004-2014, AdaCore                     --
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

package AWS.Services.Split_Pages.Uniform is

   --  Split in pages of length Max_Per_Page (except the last one)
   --
   --  Tags:
   --  NEXT          The href to the next page.
   --  PREVIOUS      The href to the previous page.
   --  FIRST         The href to the first page.
   --  LAST          The href to the last page.
   --  PAGE_INDEX    Position of the current page in the INDEXES_V vector
   --                Note that for this splitter, this is also the page number.
   --  HREFS_V       A vector tag containing a set of href to pages.
   --  INDEXES_V     A vector tag (synchronized with HREFS_V) containing the
   --                page numbers for the hrefs.
   --
   --  HREFS_V and INDEXES_V can be used to create an index to the generated
   --  pages.

   type Splitter (Max_Per_Page : Positive) is
     new Split_Pages.Splitter with private;

   overriding function Get_Page_Ranges
     (This  : Splitter;
      Table : Templates.Translate_Set) return Ranges_Table;

   overriding function Get_Translations
     (This   : Splitter;
      Page   : Positive;
      URIs   : URI_Table;
      Ranges : Ranges_Table) return Templates.Translate_Set;

private

   type Splitter (Max_Per_Page : Positive) is new Split_Pages.Splitter with
      record
         HREFS_V   : Templates.Vector_Tag;
         INDEXES_V : Templates.Vector_Tag;
      end record;

end AWS.Services.Split_Pages.Uniform;
