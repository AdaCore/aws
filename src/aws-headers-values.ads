------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                            Copyright (C) 2002                            --
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

with Ada.Strings.Unbounded;

package AWS.Headers.Values is

   use Ada.Strings.Unbounded;

   --  Data represent a token from an header line. There is two kinds of
   --  token, either named or un-named.
   --
   --     Content-Type: xyz boundary="uvt"
   --
   --  Here xyz is an un-named value and uvt a named value the name is
   --  boundary.

   type Data (Named_Value : Boolean := True) is record
      Value : Unbounded_String;
      case Named_Value is
         when True =>
            Name : Unbounded_String;
         when False =>
            null;
      end case;
   end record;

   type Set is array (Positive range <>) of Data;

   -----------
   -- Parse --
   -----------

   generic

      with procedure Value
        (Item : in     String;
         Quit : in out Boolean);
      --  Called for every un-named value read from the header value

      with procedure Named_Value
        (Name  : in     String;
         Value : in     String;
         Quit  : in out Boolean);
      --  Called for every named value read from the header value

   procedure Parse (Header_Value : in String);
   --  Look for un-named values and named ones (Name="Value" pairs) in the
   --  header line, and call appropriate routines when found. Quit is set to
   --  False before calling Value or Named_Value, the parsing can be stopped
   --  by setting Quit to True.

   -------------------
   -- Split / Index --
   -------------------

   function Split (Header_Value : in String) return Set;
   --  Returns a Set with each named and un-named values splited from Data.

   function Index
     (Set            : in Values.Set;
      Name           : in String;
      Case_Sensitive : in Boolean := True)
      return Natural;
   --  Returns index for Name in the set or 0 if Name not found.
   --  If Case_Sensitive is false the find is case_insensitive.

   ---------------------------
   -- Other search routines --
   ---------------------------

   function Search
     (Header_Value   : in String;
      Name           : in String;
      Case_Sensitive : in Boolean := True)
      return String;
   --  Returns Value for Name in Header_Value or the empty string if Name not
   --  found. If Case_Sensitive is False the search is case insensitive.

   function Get_Unnamed_Value
     (Header_Value : in String;
      N            : in Positive := 1)
      return String;
   --  Returns N-th un-named value from Header_Value.

end AWS.Headers.Values;
