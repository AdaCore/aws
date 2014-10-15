------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2000-2014, AdaCore                     --
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

pragma Ada_2012;

with AWS.Containers.Tables;

private with Ada.Strings.Unbounded;

package AWS.Parameters is

   type List is new AWS.Containers.Tables.Table_Type with private;

   subtype VString_Array is AWS.Containers.Tables.VString_Array;

   function URI_Format (Parameter_List : List) return String with Inline;
   --  Returns the list of parameters in the URI format. This can be added
   --  after the resource to form the complete URI. The format is:
   --  "?name1=value1&name2=value2..."
   --  If there is no parameter in the list, the empty string is returned.

   overriding function Union
     (Left, Right : List; Unique : Boolean) return List;
   --  Concatenates two parameter lists, if Unique is True do not add Right
   --  list element into result when element with the same name already exists
   --  in the Left list.

   --  See AWS.Containers.Tables for inherited routines

private

   use Ada.Strings.Unbounded;

   type List is new AWS.Containers.Tables.Table_Type with record
      Parameters : Unbounded_String;
   end record;

end AWS.Parameters;
