------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2000-2001                          --
--                     Dmitriy Anisimkov & Pascal Obry                      --
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
with Ada.Characters.Handling;

with AWS.Translator;
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

      function Normalize_Name
        (Name : in String; To_Upper : in Boolean)
        return String;
      --  Returns Name in upper case if To_Upper is set to True and it returns
      --  Name unchanged otherwise.

      --------------------
      -- Normalize_Name --
      --------------------

      function Normalize_Name
        (Name : in String; To_Upper : in Boolean)
        return String is
      begin
         if To_Upper then
            return Ada.Characters.Handling.To_Upper (Name);
         else
            return Name;
         end if;
      end Normalize_Name;

      C       : constant Positive := Parameter_List.Count + 1;

      K_Key   : constant String   := "__AWS_K" & Utils.Image (C);
      K_Value : constant String   := "__AWS_V" & Utils.Image (C);

      L_Key   : constant String   := Normalize_Name
        (Translator.Decode_URL (Name), not Parameter_List.Case_Sensitive);

      L_Value : constant String   := Translator.Decode_URL (Value);
   begin
      Parameter_List.Count := Parameter_List.Count + 1;

      begin
         Key_Value.Insert_Node
           ((To_Unbounded_String (L_Key), To_Unbounded_String (L_Value)),
            Parameter_List.Data);
      exception
         --  This key already exist, catenate the new value to the old one
         --  separated with Val_Separator.

         when AWS.Key_Value.Tree.Duplicate_Key =>
            declare
               Current_Value : constant String :=
                 Internal_Get (Parameter_List, L_Key, 0);
            begin
               Key_Value.Update_Node
                 ((To_Unbounded_String (L_Key),
                   To_Unbounded_String
                     (Current_Value & Val_Separator & L_Value)),
                  Parameter_List.Data);
            end;
      end;

      Key_Value.Insert_Node
        ((To_Unbounded_String (K_Key), To_Unbounded_String (L_Key)),
         Parameter_List.Data);

      Key_Value.Insert_Node
        ((To_Unbounded_String (K_Value), To_Unbounded_String (L_Value)),
         Parameter_List.Data);
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

   -----------
   -- Reset --
   -----------

   procedure Reset (Parameter_List : in out List) is
   begin
      Key_Value.Delete_Tree (Parameter_List.Data);
      Parameter_List.Count := 0;
   end Reset;

end AWS.Parameters.Set;
