------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2003-2009, AdaCore                     --
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

with Ada.Strings.Unbounded;

package WSDL_4 is

   use Ada.Strings.Unbounded;

   type My_Int is new Integer;
   type My_Float is new Long_Float;

   subtype S_My_Int is Integer;
   subtype S_My_Float is Long_Float;

   type Rec is record
      Item1 : My_Int;
      Item2 : My_Float;
      Item3 : S_My_Int;
      Item4 : S_My_Float;
      Item5 : Unbounded_String;
      Item6 : Character;
   end record;

   procedure Try
     (Param1 : My_Int;
      Param2 : My_Float;
      Param3 : S_My_Int;
      Param4 : S_My_Float;
      Param5 : Rec);

   function Try2 (Param1 : Integer; Param2 : String) return Rec;

   function Try3 (Param1 : My_Float; Param2 : S_My_Int) return S_My_Float;

   function Try4 return My_Int;

end WSDL_4;
