------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2000-2004                          --
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

with Ada.Unchecked_Deallocation;

package body AWS.Containers.Tables.Set is

   procedure Reset (Table : in out Index_Table_Type);
   --  Free all elements and destroy his entries

   procedure Free is new Ada.Unchecked_Deallocation
     (Element, Element_Access);

   procedure Free_Elements (Data : in out Data_Table.Instance);
   --  Free all dynamically allocated strings in the data table

   ---------
   -- Add --
   ---------

   procedure Add
     (Table       : in out Table_Type;
      Name, Value : in     String)
   is
      L_Key  : constant String
        :=  Normalize_Name (Name, not Table.Case_Sensitive);

      Cursor : Index_Table.Cursor;
   begin
      --  Add name/value pair into the Data table

      Data_Table.Append
        (Table.Data,
         new Element'
           (Name_Length  => Name'Length,
            Value_Length => Value'Length,
            Name         => Name,
            Value        => Value));

      --  Add Data_Table.Last index into the corresponding Name_Indexes table

      Cursor := Index_Table.Find (Table.Index.all, L_Key);

      if Index_Table.Has_Element (Cursor) then
         declare
            Item : Name_Index_Table := Index_Table.Element (Cursor);
         begin
            Name_Indexes.Append (Item, Data_Table.Last (Table.Data));
            Index_Table.Replace_Element (Cursor, By => Item);
         end;

      else
         declare
            Value   : Name_Index_Table;
            Success : Boolean;
         begin
            Name_Indexes.Init (Value);
            Name_Indexes.Append (Value, Data_Table.Last (Table.Data));
            Index_Table.Insert
              (Table.Index.all, L_Key, Value, Cursor, Success);
            pragma Assert (Success);
         end;
      end if;
   end Add;

   --------------------
   -- Case_Sensitive --
   --------------------

   procedure Case_Sensitive
     (Table : in out Table_Type;
      Mode  : in     Boolean) is
   begin
      Table.Case_Sensitive := Mode;
   end Case_Sensitive;

   ----------
   -- Free --
   ----------

   procedure Free (Table : in out Table_Type) is

      procedure Free is
         new Ada.Unchecked_Deallocation (Index_Table_Type, Index_Access);

   begin
      if Table.Index /= null then
         Reset (Table.Index.all);
         Free (Table.Index);

         Free_Elements (Table.Data);
         Data_Table.Free (Table.Data);
      end if;
   end Free;

   -------------------
   -- Free_Elements --
   -------------------

   procedure Free_Elements (Data : in out Data_Table.Instance) is
   begin
      for I in Data_Table.First .. Data_Table.Last (Data) loop
         Free (Data.Table (I));
      end loop;
   end Free_Elements;

   -----------
   -- Reset --
   -----------

   procedure Reset (Table : in out Index_Table_Type) is
      Cursor : Index_Table.Cursor;
   begin
      Cursor := Index_Table.First (Table);

      while Index_Table.Has_Element (Cursor) loop
         declare
            Item : Name_Index_Table := Index_Table.Element (Cursor);
         begin
            Name_Indexes.Free (Item);
         end;
         Index_Table.Next (Cursor);
      end loop;

      Index_Table.Clear (Table);
   end Reset;

   procedure Reset (Table : in out Table_Type) is
   begin
      if Table.Index = null then
         Table.Index := new Index_Table_Type;

      else
         Reset (Table.Index.all);
         Free_Elements (Table.Data);
      end if;

      Data_Table.Init (Table.Data);
   end Reset;

   ------------
   -- Update --
   ------------

   procedure Update
     (Table : in out Table_Type;
      Name  : in     String;
      Value : in     String;
      N     : in     Positive := 1)
   is
      L_Key  : constant String
        :=  Normalize_Name (Name, not Table.Case_Sensitive);

      Cursor : Index_Table.Cursor;
   begin
      Cursor := Index_Table.Find (Table.Index.all, L_Key);

      if not Index_Table.Has_Element (Cursor) then
         if N /= 1 then
            raise Constraint_Error;
         end if;

         declare
            Values  : Name_Index_Table;
            Success : Boolean;
         begin
            Name_Indexes.Init (Values);

            Data_Table.Append
              (Table.Data,
               new Element'
                 (Name_Length  => Name'Length,
                  Value_Length => Value'Length,
                  Name         => Name,
                  Value        => Value));

            Name_Indexes.Append (Values, Data_Table.Last (Table.Data));
            Index_Table.Insert
              (Table.Index.all, L_Key, Values, Cursor, Success);
            pragma Assert (Success);
         end;

      else
         declare
            Item : Name_Index_Table := Index_Table.Element (Cursor);
         begin
            if Key_Positive (N) <= Name_Indexes.Last (Item) then
               --  Replace item
               declare
                  Index : constant Positive := Item.Table (Key_Positive (N));
               begin
                  Free (Table.Data.Table (Index));
                  Table.Data.Table (Index) :=
                    new Element'
                      (Name_Length  => Name'Length,
                       Value_Length => Value'Length,
                       Name         => Name,
                       Value        => Value);
               end;

            elsif Key_Positive (N) = Name_Indexes.Last (Item) + 1 then
               --  Add item at then end of the table
               Data_Table.Append
                 (Table.Data,
                  new Element'
                    (Name_Length  => Name'Length,
                     Value_Length => Value'Length,
                     Name         => Name,
                     Value        => Value));

               Name_Indexes.Append (Item, Data_Table.Last (Table.Data));
               Index_Table.Replace_Element (Cursor, By => Item);

            else
               --  This item does not exist
               raise Constraint_Error;
            end if;
         end;
      end if;
   end Update;

end AWS.Containers.Tables.Set;
