------------------------------------------------------------------------------
--                             Templates Parser                             --
--                                                                          --
--                            Copyright (C) 2004                            --
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

--  This API provides a way to save a Translate_Set as an XML document.
--  There is special rules to know about composite tags.
--
--  Composite tags :
--
--     If a tag named TAG exists, then the name TAG_LABEL is used as a label
--     for this specific tag. This label is meant to be used as label for this
--     set of values (vector, matrix or deeper).
--
--  Composite tags (more than one nested level)
--
--     If a tag named TAG exists, then the names TAG_DIM[n]_LABEL is used as a
--     label for the tag's nth axis. In this case TAG_DIM[n]_LABEL must be a
--     vector tag, each entry corresponds to a label on this axis.
--
--  ??? Add DTD here

package Templates_Parser.XML is

   function Image (Translations : in Translate_Set) return Unbounded_String;
   --  Returns a string representation encoded in XML for this
   --  translate table.

   procedure Save (Filename : in String; Translations : in Translate_Set);
   --  Write the translate table into filename

end Templates_Parser.XML;
