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
with Ada.Strings.Fixed;
with Ada.Unchecked_Deallocation;

with AWS.URL;
with AWS.Utils;
with AWS.Key_Value;

package body AWS.Parameters.Set is

   use Ada.Strings.Unbounded;
   use Ada.Strings;

   ---------
   -- Add --
   ---------

   procedure Add
     (Parameter_List : in out List;
      Name, Value    : in     String)
   is

      C       : constant Positive := Count (Parameter_List) + 1;

      K_Key   : constant String   := "__AWS_K" & Utils.Image (C);
      K_Value : constant String   := "__AWS_V" & Utils.Image (C);

      OL_Key  : constant String   := URL.Decode (Name);
      L_Key   : constant String   := Normalize_Name
        (OL_Key, not Parameter_List.Case_Sensitive);

      L_Value : constant String   := URL.Decode (Value);
   begin

      begin
         Key_Value.Insert
           (Parameter_List.Data.all,
            L_Key,
            To_Unbounded_String (L_Value));
      exception
         --  This key already exist, catenate the new value to the old one
         --  separated with Val_Separator.

         when AWS.Key_Value.Table.Duplicate_Item_Error =>
            declare
               Current_Value : constant String :=
                 Internal_Get (Parameter_List, L_Key, 0);
            begin
               Key_Value.Replace_Value
                 (Parameter_List.Data.all,
                  L_Key,
                  To_Unbounded_String
                  (Current_Value & Val_Separator & L_Value));
            end;
      end;

      Key_Value.Insert
        (Parameter_List.HTTP_Data.all, K_Key, To_Unbounded_String (OL_Key));

      Key_Value.Insert
        (Parameter_List.HTTP_Data.all, K_Value, To_Unbounded_String (L_Value));
   end Add;

   ---------
   -- Add --
   ---------

   procedure Add (Parameter_List : in out List; Parameters : in String) is
      P : String renames Parameters;
      C : Positive := P'First;
      I : Natural;
      S : Positive := P'First;
      E : Natural;
   begin
      --  Skip leading question mark if present.

      if P /= "" and then P (C) = '?' then
         C := Positive'Succ (C);
         S := Positive'Succ (S);
      end if;

      Parameter_List.Parameters := To_Unbounded_String ('?' & P (C .. P'Last));

      loop
         I := Fixed.Index (P (C .. P'Last), "=");

         exit when I = 0;

         S := I + 1;

         E := Fixed.Index (P (S .. P'Last), "&");

         if E = 0 then
            --  last parameter

            Add (Parameter_List, P (C .. I - 1), P (S .. P'Last));
            exit;

         else
            Add (Parameter_List, P (C .. I - 1), P (S .. E - 1));
            C := E + 1;
         end if;
      end loop;
   end Add;

   --------------------
   -- Case_Sensitive --
   --------------------

   procedure Case_Sensitive
     (Parameter_List : in out List;
      Mode           : in     Boolean) is
   begin
      Parameter_List.Case_Sensitive := Mode;
   end Case_Sensitive;

   ----------
   -- Free --
   ----------

   procedure Free (Parameter_List : in out List) is

      procedure Free is
         new Ada.Unchecked_Deallocation (Key_Value.Set, Key_Value.Set_Access);

      use type Key_Value.Set_Access;

   begin
      if not (Parameter_List.Data = null) then
         Key_Value.Destroy (Parameter_List.Data.all);
         Key_Value.Destroy (Parameter_List.HTTP_Data.all);
         Free (Parameter_List.Data);
         Free (Parameter_List.HTTP_Data);
      end if;
   end Free;

   -----------
   -- Reset --
   -----------

   procedure Reset (Parameter_List : in out List) is
      use type Key_Value.Set_Access;
   begin
      if Parameter_List.Data = null then
         Parameter_List.Data := new Key_Value.Set;
         Parameter_List.HTTP_Data := new Key_Value.Set;
      else
         Key_Value.Destroy (Parameter_List.Data.all);
         Key_Value.Destroy (Parameter_List.HTTP_Data.all);
      end if;

      Parameter_List.Parameters := Null_Unbounded_String;
   end Reset;

end AWS.Parameters.Set;
