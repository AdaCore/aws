------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2004-2014, AdaCore                     --
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

with Templates_Parser.Query;

package body AWS.Services.Split_Pages.Shared is

   -----------------------
   -- Associated_Vector --
   -----------------------

   function Associated_Vector
     (Table : Templates.Translate_Set;
      Key   : String)
      return Templates.Tag
   is
      use Templates;
      use Templates.Query;
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

   function Safe_URI (URIs : URI_Table; I : Natural) return String is
   begin
      if I in URIs'Range then
         return To_String (URIs (I));
      else
         return "";
      end if;
   end Safe_URI;

end AWS.Services.Split_Pages.Shared;
