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

with Ada.Strings.Unbounded;

package body SOAP.Utils is

   use Ada.Strings.Unbounded;

   ------------
   -- Encode --
   ------------

   function Encode (Str : in String) return String is
      Result : Unbounded_String;
   begin
      for K in Str'Range loop
         case Str (K) is
            when '<'    => Append (Result, "&lt;");
            when '>'    => Append (Result, "&gt;");
            when '&'    => Append (Result, "&amp;");
            when '''    => Append (Result, "&apos;");
            when '"'    => Append (Result, "&quot;");
            when others => Append (Result, Str (K));
         end case;
      end loop;

      return To_String (Result);
   end Encode;

   ---------
   -- Tag --
   ---------

   function Tag (Name : in String; Start : in Boolean) return String is
   begin
      if Start then
         return '<' & Name & '>';
      else
         return "</" & Name & '>';
      end if;
   end Tag;

end SOAP.Utils;
