------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2005-2008, AdaCore                     --
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

--  Internal unit only, should not be used by end-user. This is designed to
--  convert simple value without xsi:type information to typed data by the
--  SOAP engine.

package SOAP.Types.Untyped is

   type Untyped is new XSD_String with null record;
   --  This type is used to record value as a string for untyped value found
   --  while parsing a payload. Such a type will be automatically converted to
   --  another type (Integer, Short, Unsigned_Long, Time_Instant...) by the
   --  SOAP.Types.Get routine.

   overriding function S
     (V    : in String;
      Name : in String := "item") return Untyped;

   overriding function S
     (V    : in Unbounded_String;
      Name : in String  := "item") return Untyped;

end SOAP.Types.Untyped;
