------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                            Copyright (C) 2004                            --
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

--  $RCSfile$
--  $Revision$ $Date$ $Author$

with Templates_Parser.Query;

package body AWS.Services.Split_Pages.Shared is

   -----------------------
   -- Associated_Vector --
   -----------------------

   function Associated_Vector
     (Table : in Templates.Translate_Set;
      Key   : in String)
      return Templates.Tag
   is
      --  Determine key vector
      use Templates, Templates.Query;
   begin
      if Key = "" then
         raise Splitter_Error;
      end if;

      declare
         Association : constant Templates.Association := Get (Table, Key);
      begin
         if Association = Null_Association
           or else Kind (Association) = Std
         then
            raise Splitter_Error;
         end if;
         return Composite (Association);
      end;
   end Associated_Vector;

   --------------
   -- Safe_URI --
   --------------

   function Safe_URI (URIs : in URI_Table; I : in Natural) return String is
   begin
      if I in URIs'Range then
         return To_String (URIs (I));
      else
         return "";
      end if;
   end Safe_URI;

end AWS.Services.Split_Pages.Shared;
