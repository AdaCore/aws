------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2000-2010, AdaCore                     --
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

package body AWS.Containers.Tables.Set is

   procedure Update_Internal
     (Table : in out Table_Type;
      Name  : String;
      Value : String;
      N     : Natural);
   --  Update the N-th Value with the given Name into the Table.
   --  The container could already have more than one value associated with
   --  this name. If there is M values with this Name, then if:
   --     N <= M      => update the value
   --     N  = 0      => the pair name=value is appended to the table
   --     N  = M + 1  => idem
   --     N  > M + 1  => Constraint_Error raised

   ---------
   -- Add --
   ---------

   procedure Add
     (Table       : in out Table_Type;
      Name, Value : String) is
   begin
      Update_Internal (Table, Name, Value, 0);
   end Add;

   --------------------
   -- Case_Sensitive --
   --------------------

   procedure Case_Sensitive
     (Table : in out Table_Type;
      Mode  : Boolean) is
   begin
      Table.Case_Sensitive := Mode;
   end Case_Sensitive;

   -----------
   -- Reset --
   -----------

   procedure Reset (Table : in out Table_Type) is
   begin
      Index_Table.Clear (Table.Index);
      Data_Table.Clear (Table.Data);
   end Reset;

   ------------
   -- Update --
   ------------

   procedure Update
     (Table : in out Table_Type;
      Name  : String;
      Value : String;
      N     : Positive := 1) is
   begin
      Update_Internal (Table, Name, Value, N);
   end Update;

   ---------------------
   -- Update_Internal --
   ---------------------

   procedure Update_Internal
     (Table : in out Table_Type;
      Name  : String;
      Value : String;
      N     : Natural)
   is
      L_Key  : constant String :=
                 Normalize_Name (Name, not Table.Case_Sensitive);

      Cursor : Index_Table.Cursor := Index_Table.Find (Table.Index, L_Key);

      procedure Process
        (Key  : String;
         Item : in out Name_Index_Table);

      -------------
      -- Process --
      -------------

      procedure Process
        (Key  : String;
         Item : in out Name_Index_Table)
      is
         pragma Unreferenced (Key);
         NV : constant Element :=
                (Name_Length  => Name'Length,
                 Value_Length => Value'Length,
                 Name         => Name,
                 Value        => Value);
      begin
         if N = 0 or else N = Natural (Name_Indexes.Length (Item)) + 1 then
            --  Add item at then end of the table

            Data_Table.Append (Table.Data, NV);

            Name_Indexes.Append
              (Item, Key_Positive (Data_Table.Length (Table.Data)));

         elsif N <= Natural (Name_Indexes.Length (Item)) then
            --  Replace item

            Data_Table.Replace_Element
              (Table.Data, Name_Indexes.Element (Item, N), NV);

         else
            --  This item does not exist

            raise Constraint_Error;
         end if;
      end Process;

   begin
      if not Index_Table.Has_Element (Cursor) then
         --  Insert empty vector into Table.Index

         if N > 1 then
            raise Constraint_Error;
         end if;

         declare
            Values  : Name_Index_Table;
            Success : Boolean;
         begin
            Index_Table.Insert
              (Table.Index, L_Key, Values, Cursor, Success);
            pragma Assert (Success);
         end;
      end if;

      --  Update index vector just in place

      Index_Table.Update_Element (Table.Index, Cursor, Process'Access);
   end Update_Internal;

end AWS.Containers.Tables.Set;
