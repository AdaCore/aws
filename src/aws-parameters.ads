------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                            Copyright (C) 2000                            --
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

with AWS.Key_Value;

package AWS.Parameters is

   type Set is private;

   Empty_Set : constant Set;

   procedure Add (Parameter_Set : in out Set;
                  Key, Value    : in     String);
   --  Add a new Key/Value pair into the parameter set.

   function Count (Parameter_Set : in Set) return Natural;
   --  Returns the number of item in Parameter_Set.

   function Count (Parameter_Set : in Set; Key : in String) return Natural;
   --  Returns the number of value for Key in Parameter_Set. It returns 0 if
   --  Key does not exist.

   function Exist (Parameter_Set : in Set; Key : in String) return Boolean;
   --  Returns True if Key exist in Parameter_Set.

   function Get (Parameter_Set : in Set;
                 Key           : in String;
                 N             : in Positive := 1) return String;
   --  Returns the Nth value associated with Key into Parameter_Set. Returns
   --  the emptry string if key does not exist.

   function Get_Key   (Parameter_Set : in Set; N : in Positive) return String;
   --  Returns the Nth Key in Parameter_Set or the empty string if there is no
   --  such Key.

   function Get_Value (Parameter_Set : in Set; N : in Positive) return String;
   --  Returns the Nth Value in Parameter_Set or the empty string if there is
   --  no such Value.

   procedure Release (Parameter_Set : in out Set);
   --  Release all memory used by Set. Set will be reinitialized and will be
   --  ready for new use.

private

   type Set is record
      Data  : Key_Value.Set;
      Count : Natural := 0;
   end record;

   ES : Set;
   Empty_Set : constant Set := ES;

end AWS.Parameters;
