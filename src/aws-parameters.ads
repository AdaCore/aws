------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2000-2001                          --
--                                ACT-Europe                                --
--                                                                          --
--  Authors: Dmitriy Anisimov - Pascal Obry                                 --
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

with AWS.Key_Value;

package AWS.Parameters is

   type List is private;

   function Count (Parameter_List : in List) return Natural;
   --  Returns the number of item in Parameter_List.

   function Count (Parameter_List : in List; Name : in String) return Natural;
   --  Returns the number of value for Key in Parameter_List. It returns 0 if
   --  Key does not exist.

   function Exist (Parameter_List : in List; Name : in String) return Boolean;
   --  Returns True if Key exist in Parameter_List.

   function Get
     (Parameter_List : in List;
      Name           : in String;
      N              : in Positive := 1)
     return String;
   --  Returns the Nth value associated with Key into Parameter_List. Returns
   --  the emptry string if key does not exist.

   function Get_Name
     (Parameter_List : in List;
      N              : in Positive := 1)
     return String;
   --  Returns the Nth Name in Parameter_List or the empty string if there is
   --  no parameter with this Name.

   function Get_Value
     (Parameter_List : in List;
      N              : in Positive := 1)
     return String;
   --  Returns the Nth Value in Parameter_List or the empty string if there is
   --  no such Value.

private

   type List is record
      Case_Sensitive : Boolean := True;
      Data           : Key_Value.Set_Access := new Key_Value.Set;
      Count          : Natural := 0;
   end record;

   function Internal_Get
     (Parameter_List : in List;
      Name           : in String;
      N              : in Natural)
     return String;
   pragma Inline (Internal_Get);
   --  Returns the Nth value associated with Key into Parameter_List. Returns
   --  the emptry string if key does not exist. If N = 0 it returns as-is all
   --  the values as inserted in the tree for Key.

   Val_Separator : constant Character := ASCII.VT;

end AWS.Parameters;
