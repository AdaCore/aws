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

--  Parameters name/value are put into the GNAT.Dynamic_Tables.Table_Type
--  (Data field). The name as a key and the numeric index as a value is
--  placing to the AVL Tree for the fast find all Name/Value pairs with the
--  same name. Each value of the AVL Tree is a table of numeric indexes
--  in the Data field. The parameters must be accessible
--  through their string index by name and also using an numeric index in
--  the place order. So given a set of parameters (K1=V1, K2=V2...),
--  one must be able to ask for the value for K1 but also the name of the
--  second key or the value of the third key.
--
--  Each K/V pair is then inserted into the Data table for access by numeric
--  index. And its numeric index is placing to the AVL tree indexed by name.
--  The AVL Tree values is a tables of numeric indexes in the Data table.

with Ada.Characters.Handling;

package body AWS.Containers.Tables is

   use Ada.Strings.Unbounded;

   Missing_Item_Error : exception
      renames Index_Table.Missing_Item_Error;

   -----------
   -- Count --
   -----------

   function Count (Table : in Table_Type) return Natural is
   begin
      pragma Assert (Table.Index /= null);
      return Data_Table.Last (Table.Data);
   end Count;

   -----------
   -- Count --
   -----------

   function Count
     (Table : in Table_Type;
      Name           : in String)
      return Natural
   is
      Value : Name_Index_Table;
   begin
      pragma Assert (Table.Index /= null);
      Get_Value
        (Table.Index.all,
         Normalize_Name (Name, not Table.Case_Sensitive),
         Value);

      return Natural (Name_Indexes.Last (Value));

   exception
      when Missing_Item_Error =>
         return 0;
   end Count;

   -----------
   -- Exist --
   -----------

   function Exist
     (Table : in Table_Type;
      Name  : in String)
      return Boolean is
   begin
      pragma Assert (Table.Index /= null);
      return Is_Present
        (Table.Index.all,
         Normalize_Name (Name, not Table.Case_Sensitive));
   end Exist;

   ---------
   -- Get --
   ---------

   function Get
     (Table : in Table_Type;
      Name  : in String;
      N     : in Positive := 1)
      return String is
   begin
      return Internal_Get (Table, Name, N);
   end Get;

   function Get
     (Table : in Table_Type;
      N     : in Positive)
      return Element
   is
   begin
      pragma Assert (Table.Index /= null);

      if N <= Data_Table.Last (Table.Data) then
         return Table.Data.Table (N).all;
      else
         return
           (Name_Length  => 0,
            Value_Length => 0,
            Name         => "",
            Value        => "");
      end if;
   end Get;

   --------------
   -- Get_Name --
   --------------

   function Get_Name
     (Table : in Table_Type;
      N     : in Positive := 1)
      return String is
   begin
      pragma Assert (Table.Index /= null);
      if N <= Data_Table.Last (Table.Data) then
         return Table.Data.Table (N).Name;
      else
         return "";
      end if;
   end Get_Name;

   ---------------
   -- Get_Names --
   ---------------

   function Get_Names (Table : in Table_Type) return VString_Array is

      procedure Process
        (Key      : in     String;
         Value    : in     Name_Index_Table;
         Order    : in     Positive;
         Continue : in out Boolean);

      Result : VString_Array (1 .. Name_Count (Table));

      -------------
      -- Process --
      -------------

      procedure Process
        (Key      : in     String;
         Value    : in     Name_Index_Table;
         Order    : in     Positive;
         Continue : in out Boolean)
      is
         pragma Warnings (Off, Value);
         pragma Warnings (Off, Continue);
      begin
         Result (Order) := To_Unbounded_String (Key);
      end Process;

      --------------------
      -- Each_Key_Value --
      --------------------

      procedure Each_Key_Value is
         new Index_Table.Disorder_Traverse_G (Process);

   begin
      if Table.Index /= null then
         Each_Key_Value (Index_Table.Table_Type (Table.Index.all));
      end if;

      return Result;
   end Get_Names;

   ---------------
   -- Get_Value --
   ---------------

   function Get_Value
     (Table : in Table_Type;
      N              : in Positive := 1)
      return String is
   begin
      pragma Assert (Table.Index /= null);

      if N <= Data_Table.Last (Table.Data) then
         return Table.Data.Table (N).Value;
      else
         return "";
      end if;
   end Get_Value;

   ----------------
   -- Get_Values --
   ----------------

   function Get_Values
     (Table : in Table_Type;
      Name  : in String)
      return VString_Array
   is
      Value : Name_Index_Table;
   begin
      pragma Assert (Table.Index /= null);

      Get_Value
        (Table.Index.all,
         Normalize_Name (Name, not Table.Case_Sensitive),
         Value);

      declare
         Last :  Key_Positive := Name_Indexes.Last (Value);
         Result : VString_Array (1 .. Natural (Last));
      begin
         for I in 1 .. Last loop
            Result (Natural (I)) := To_Unbounded_String (Table.Data.Table
               (Value.Table (I)).Value);
         end loop;
         return Result;
      end;

   exception
      when Missing_Item_Error =>
         return (1 .. 0 => Null_Unbounded_String);
   end Get_Values;

   ------------------
   -- Internal_Get --
   ------------------

   function Internal_Get
     (Table : in Table_Type;
      Name  : in String;
      N     : in Natural)
      return String
   is
      Value : Name_Index_Table;
   begin
      pragma Assert (Table.Index /= null);

      Get_Value
        (Table.Index.all,
         Normalize_Name (Name, not Table.Case_Sensitive),
         Value);

      if Key_Positive (N) <= Name_Indexes.Last (Value) then
         return Table.Data.Table (Value.Table (Key_Positive (N))).Value;
      else
         return "";
      end if;

   exception
      when Missing_Item_Error =>
         return "";
   end Internal_Get;

   ----------------
   -- Name_Count --
   ----------------

   function Name_Count (Table : in Table_Type) return Natural is
   begin
      if Table.Index = null then
         return 0;
      else
         return Size (Table.Index.all);
      end if;
   end Name_Count;

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

end AWS.Containers.Tables;
