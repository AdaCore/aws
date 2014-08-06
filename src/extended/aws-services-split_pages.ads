------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2003-2014, AdaCore                     --
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

with Ada.Strings.Unbounded;
with AWS.Response;
with AWS.Templates;

package AWS.Services.Split_Pages is

   use Ada.Strings.Unbounded;

   Splitter_Error : exception;

   --  This package provides an API to split a big table in multiple pages
   --  using the transient Web Pages support.

   type Page_Range is record
      First : Positive;
      Last  : Natural;  -- For an empty range, Last < First
   end record;

   type Ranges_Table is array (Positive range <>) of Page_Range;
   type URI_Table    is array (Positive range <>) of Unbounded_String;

   type Splitter is abstract tagged limited private;
   --  This is the (abstract) root class of all splitters
   --  Two operations are necessary: Get_Page_Ranges and Get_Translations
   --  The following tags are always defined by the Parse function; however,
   --  if a splitter redefines them in Get_Translations, the new definition
   --  will replace the standard one:
   --  NUMBER_PAGES  Number of pages generated.
   --  PAGE_NUMBER   Position of the current page in all pages
   --  OFFSET        Current table line offset real table line can be computed
   --                using: @_"+"(OFFSET):TABLE_LINE_@

   function Get_Page_Ranges
     (This  : Splitter;
      Table : Templates.Translate_Set) return Ranges_Table is abstract;
   --  Get_Page_Ranges is called to define the range (in lines) of each split
   --  page. Note that the ranges may overlap and need not cover the full
   --  table.

   function Get_Translations
     (This   : Splitter;
      Page   : Positive;
      URIs   : URI_Table;
      Ranges : Ranges_Table) return Templates.Translate_Set is abstract;
   --  Get_Translations builds the translation table for use with the splitter

   function Parse
     (Template     : String;
      Translations : Templates.Translate_Set;
      Table        : Templates.Translate_Set;
      Split_Rule   : Splitter'Class;
      Cached       : Boolean := True) return Response.Data;

   function Parse
     (Template     : String;
      Translations : Templates.Translate_Table;
      Table        : Templates.Translate_Table;
      Split_Rule   : Splitter'Class;
      Cached       : Boolean := True) return Response.Data;
   --  Parse the Template file and split the result in multiple pages.
   --  Translations is a standard Translate_Set used for all pages. Table
   --  is the Translate_Set containing data for the table to split in
   --  multiple pages. This table will be analysed and according to the
   --  Split_Rule, a set of transient pages will be created.
   --  If Cached is True the template will be cached (see Templates_Parser
   --  documentation).
   --  Each Split_Rule define a number of specific tags for use in the template
   --  file.

   function Parse
     (Template     : String;
      Translations : Templates.Translate_Table;
      Table        : Templates.Translate_Table;
      Max_Per_Page : Positive := 25;
      Max_In_Index : Positive := 20;
      Cached       : Boolean  := True) return Response.Data;
   --  Compatibility function with previous version of AWS.
   --  Uses the Uniform_Splitter
   --  Note that the Max_In_Index parameter is ignored.
   --  The same effect can be achieved by using the bounded_index.thtml
   --  template for displaying the index.

private

   type Splitter is abstract tagged limited record
      Self : not null access Splitter'Class := Splitter'Unchecked_Access;
   end record;

   --  Type used to index alpha tables:
   --  1     => empty key
   --  2     => numeric key (0 .. 9)
   --  3..28 => Alpha key (A .. Z)

   type Alpha_Index is range 1 .. 28;

   Alpha_Value : constant array (Character range 'A' .. 'Z') of Alpha_Index
     := (3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15,
         16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28);

   type Lines_Table is array (Alpha_Index) of Natural;

end AWS.Services.Split_Pages;
