------------------------------------------------------------------------------
--                             Templates Parser                             --
--                                                                          --
--                            Copyright (C) 2003                            --
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

with Ada.Exceptions;

package body Templates_Parser.Query is

   use Ada;

   ----------
   -- Kind --
   ----------

   function Kind
     (Association : in Templates_Parser.Association)
      return Association_Kind is
   begin
      return Association.Kind;
   end Kind;

   --------------
   -- Variable --
   --------------

   --------------
   -- Variable --
   --------------

   function Variable
     (Association : in Templates_Parser.Association)
      return String is
   begin
      return To_String (Association.Variable);
   end Variable;

   ------------
   -- Vector --
   ------------

   function Vector
     (Association : in Templates_Parser.Association)
      return Vector_Tag is
   begin
      if Association.Kind = Vect then
         return Association.Vect_Value;
      else
         Exceptions.Raise_Exception
           (Constraint_Error'Identity,
            Variable (Association) & " is not a vector tag.");
      end if;
   end Vector;

end Templates_Parser.Query;
