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

--  Parameters are put into an AVL Tree. Each entry in the tree is composed of
--  a Key and a Value. The parameters must be accessible through their name
--  and also using an index. So given a set of parameters (K1=V1, K2=V2...),
--  one must be able to ask for the value for K1 but also the name of the
--  second key or the value of the third key.
--
--  Each K/V pair is then inserted into the Data tree and two times into the
--  HTTP_Data tree:
--
--  Into Data:
--
--  1) key=K with value=V
--
--  Into HTTP_Data:
--
--  1) key=__AWS_K<n> with value=K     (n beeing an indice representing the
--  2) key=__AWS_V<n> with value=V      entry number in the tree)
--
--  So to get the third key name we ask for the entry indexed under __AWS_K3
--  into HTTP_Data tree.
--
--  Another important point is that a key can have many values. For example
--  with an HTML multiple select entry in a form. In such a case all values
--  associated with the key K are concatenated together with a specific
--  separator.

with Ada.Unchecked_Deallocation;

with AWS.Utils;

package body AWS.Containers.Tables.Set is

   use Ada.Strings.Unbounded;

   ---------
   -- Add --
   ---------

   procedure Add
     (Table       : in out Table_Type;
      Name, Value : in     String)
   is

      C       : constant Positive := Count (Table) + 1;

      K_Key   : constant String   := "__AWS_K" & Utils.Image (C);
      K_Value : constant String   := "__AWS_V" & Utils.Image (C);

      L_Key   : constant String   := Normalize_Name
        (Name, not Table.Case_Sensitive);

   begin

      begin
         Key_Value.Insert
           (Table.Data.all,
            L_Key,
            To_Unbounded_String (Value));
      exception
         --  This key already exist, catenate the new value to the old one
         --  separated with Val_Separator.

         when Key_Value.Table.Duplicate_Item_Error =>
            declare
               Current_Value : constant String :=
                 Internal_Get (Table, L_Key, 0);
            begin
               Key_Value.Replace_Value
                 (Table.Data.all,
                  L_Key,
                  To_Unbounded_String
                  (Current_Value & Val_Separator & Value));
            end;
      end;

      Key_Value.Insert
        (Table.Ordered_Data.all, K_Key, To_Unbounded_String (Name));

      Key_Value.Insert
        (Table.Ordered_Data.all, K_Value, To_Unbounded_String (Value));
   end Add;

   --------------------
   -- Case_Sensitive --
   --------------------

   procedure Case_Sensitive
     (Table : in out Table_Type;
      Mode           : in     Boolean) is
   begin
      Table.Case_Sensitive := Mode;
   end Case_Sensitive;

   ----------
   -- Free --
   ----------

   procedure Free (Table : in out Table_Type) is

      procedure Free is
         new Ada.Unchecked_Deallocation (Key_Value.Set, Key_Value.Set_Access);

      use type Key_Value.Set_Access;

   begin
      if not (Table.Data = null) then
         Key_Value.Destroy (Table.Data.all);
         Key_Value.Destroy (Table.Ordered_Data.all);
         Free (Table.Data);
         Free (Table.Ordered_Data);
      end if;
   end Free;

   -----------
   -- Reset --
   -----------

   procedure Reset (Table : in out Table_Type) is
      use type Key_Value.Set_Access;
   begin
      if Table.Data = null then
         Table.Data := new Key_Value.Set;
         Table.Ordered_Data := new Key_Value.Set;
      else
         Key_Value.Destroy (Table.Data.all);
         Key_Value.Destroy (Table.Ordered_Data.all);
      end if;

   end Reset;

end AWS.Containers.Tables.Set;
