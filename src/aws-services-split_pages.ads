------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                            Copyright (C) 2003                            --
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

--  $Id$

with AWS.Response;
with AWS.Templates;

package AWS.Services.Split_Pages is

   --  This package provides an API to split a big table in multiple pages
   --  using the transient Web Pages support. In the template file a set of
   --  specific tags are recongnized:
   --
   --  NEXT         The href to the next page.
   --  PREVIOUS     The href to the previous page.
   --  PAGE_INDEX   Current page number.
   --  V_HREFS      A vector tag containing a set of href to pages.
   --  V_INDEXES    A vector tag (synchronized with V_HREFS) containing the
   --               page numnbers for the hrefs.
   --
   --  V_HREFS and V_INDEXES can be used to create an index to the generated
   --  pages. Note that if there is more pages than Max_In_Index a
   --  continuation (the 3 characters "...") will be added.

   function Parse
     (Template     : in String;
      Translations : in Templates.Translate_Table;
      Table        : in Templates.Translate_Table;
      Max_Per_Page : in Positive := 25;
      Max_In_Index : in Positive := 20;
      Cached       : in Boolean  := True)
      return Response.Data;
   --  Parse the Template file and split the result in multiple pages.
   --  Translations is a standard Translate_Table used for all pages. Table
   --  is the Translate_Table containing data for the table to split in
   --  multiple pages. This table will be analysed and according to the
   --  Max_Per_Page value a set of transient pages will be created.
   --  Max_In_Index is the maximum number of items in the page index. If
   --  Cached is True the template will be cached (see Templates_Parser
   --  documentation).

end AWS.Services.Split_Pages;
