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

with Ada.Strings.Unbounded;

with AWS.URL;

package body AWS.Parameters is

   procedure Refresh_Internal (Parameter_List : in out List);
   --  Refresh internal URI representation of HTTP parameters from container

   ----------------------
   -- Refresh_Internal --
   ----------------------

   procedure Refresh_Internal (Parameter_List : in out List) is
      Split : Character := '?';
   begin
      Parameter_List.Parameters := Null_Unbounded_String;

      for J in 1 .. Parameter_List.Count loop
         declare
            Item : constant Containers.Tables.Element :=
                     Parameter_List.Get (J);
         begin
            if Item.Value = "" then
               Append (Parameter_List.Parameters, Split & Item.Name);
            else
               Append
                 (Parameter_List.Parameters,
                  Split & URL.Encode (Item.Name) & "="
                  & URL.Encode (Item.Value));
            end if;

            if J = 1 then
               Split := '&';
            end if;
         end;
      end loop;
   end Refresh_Internal;

   -----------
   -- Union --
   -----------

   overriding function Union
     (Left, Right : List; Unique : Boolean) return List
   is
      subtype Table_Type is Containers.Tables.Table_Type;
      Result : List :=
        (Table_Type (Left).Union (Table_Type (Right), Unique)
         with others => <>);
   begin
      Refresh_Internal (Result);

      return Result;
   end Union;

   ----------------
   -- URI_Format --
   ----------------

   function URI_Format (Parameter_List : List) return String is
   begin
      return To_String (Parameter_List.Parameters);
   end URI_Format;

end AWS.Parameters;
