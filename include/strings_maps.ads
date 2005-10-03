------------------------------------------------------------------------------
--                             Templates Parser                             --
--                                                                          --
--                         Copyright (C) 2004-2005                          --
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

with AI302.Containers.Indefinite_Hashed_Maps;
with AI302.Strings.Hash;

generic

   type Element_Type is private;

   with function "=" (Left, Right : Element_Type)
      return Boolean is <>;

package Strings_Maps is

   package Containers is
     new AI302.Containers.Indefinite_Hashed_Maps
       (String, Element_Type, AI302.Strings.Hash, "=", "=");

   subtype Map is Containers.Map;

   subtype Cursor is Containers.Cursor;
   No_Element : Cursor renames Containers.No_Element;

   function Has_Element
     (C : in Cursor)
      return Boolean
      renames Containers.Has_Element;

end Strings_Maps;
