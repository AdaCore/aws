------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2000-2001                          --
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

with System;
with Ada.Strings.Unbounded;
with MD5;

package AWS.Utils is

   type Random_Integer is range 0 .. System.Max_Int;
   --  Integer type for random number generation.

   --  General helper functions are to be found here.

   function Random return Random_Integer;
   --  Returns a random integer number.

   function Image (N : in Natural) return String;
   --  return image of N without the leading blank.

   function Image (D : in Duration) return String;
   --  return image of N without the leading blank and with only 2 decimals
   --  numbers.

   function Gethostname return String;
   --  Return hostname.

   function Hex (V : in Natural; Width : in Natural := 0) return String;
   --  Returns the hexadecimal string representation of the decimal
   --  number V. if Width /= 0, the result will have exactly Width characters
   --  eventually padded with leading 0 or trimmed on the right.

   function Hex_Value (Hex : in String) return Natural;
   --  Returns the value for the hexadecimal number Hex. Raises
   --  COnstraint_Error is Hex is not an hexadecimal number.

   generic
      type Enum is (<>);
      type Result_Set is
        array (Enum) of Ada.Strings.Unbounded.Unbounded_String;
   procedure Parse_HTTP_Header_Line
     (Data   : in     String;
      Result :    out Result_Set);
   --  Parse the HTTP header line. Set Result_Set (Name) with the value
   --  corresponding to variable Name.

   function Get_MD5 (Data : in String) return MD5.Digest_String;
   --  Returns the MD5 digest value for the Data string.

end AWS.Utils;
