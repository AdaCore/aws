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

--  $Id$

--  Parameters name/value are put into the GNAT.Dynamic_Tables.Table_Type (Data
--  field). The name as a key and the numeric index as a value is placed into a
--  map for fast retrieval of all Name/Value pairs having the same name. Each
--  value in the map is a table of numeric indexes pointing into the Data
--  field. The parameters must be accessible using their name (string index)
--  but also using an numeric index. So given a set of parameters (K1=V1,
--  K2=V2...), one must be able to ask for the value for K1 but also the name
--  of the second key or the value of the third key.
--
--  Each K/V pair is then inserted into the Data table for access by numeric
--  index. And its numeric index is placed into the map indexed by name.

with Ada.Characters.Handling;

package body AWS.Containers.Tables is

   use Ada.Strings.Unbounded;

   procedure Get_Indexes
     (Table   : in     Table_Type;
      Name    : in     String;
      Indexes :    out Name_Index_Table;
      Found   :    out Boolean);
   pragma Inline (Get_Indexes);
   --  Returns all Name/Value indexes for the specified name.
   --  Found is set to False if Name was not found in Table and True otherwise.

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
      Name  : in String)
      return Natural
   is
      Value : Name_Index_Table;
      Found : Boolean;
   begin
      pragma Assert (Table.Index /= null);

      Get_Indexes (Table, Name, Value, Found);

      if Found then
         return Natural (Name_Indexes.Last (Value));
      else
         return 0;
      end if;
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

      return Index_Table.Is_In
        (Normalize_Name (Name, not Table.Case_Sensitive), Table.Index.all);
   end Exist;

   ---------
   -- Get --
   ---------

   function Get
     (Table : in Table_Type;
      Name  : in String;
      N     : in Positive := 1)
      return String
   is
      Value : Name_Index_Table;
      Found : Boolean;

   begin
      pragma Assert (Table.Index /= null);

      Get_Indexes (Table, Name, Value, Found);

      if Found and then Key_Positive (N) <= Name_Indexes.Last (Value) then
         return Table.Data.Table (Value.Table (Key_Positive (N))).Value;
      else
         return "";
      end if;
   end Get;

   function Get
     (Table : in Table_Type;
      N     : in Positive)
      return Element is
   begin
      pragma Assert (Table.Index /= null);

      if N <= Data_Table.Last (Table.Data) then
         return Table.Data.Table (N).all;
      else
         return Null_Element;
      end if;
   end Get;

   -----------------
   -- Get_Indexes --
   -----------------

   procedure Get_Indexes
     (Table   : in     Table_Type;
      Name    : in     String;
      Indexes :    out Name_Index_Table;
      Found   :    out Boolean)
   is
      Cursor : Index_Table.Cursor;
   begin
      Cursor := Index_Table.Find
        (Table.Index.all, Normalize_Name (Name, not Table.Case_Sensitive));

      if not Index_Table.Has_Element (Cursor) then
         Found := False;
      else
         Found   := True;
         Indexes := Index_Table.Element (Cursor);
      end if;
   end Get_Indexes;

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

   function Get_Names
     (Table : in Table_Type;
      Sort  : in Boolean := False)
      return VString_Array
   is
      Result : VString_Array (1 .. Name_Count (Table));
      Cursor : Index_Table.Cursor;
      Index  : Natural := Result'First - 1;
   begin
      Cursor := Index_Table.First (Table.Index.all);

      while Index_Table.Has_Element (Cursor) loop
         Index := Index + 1;
         Result (Index) :=
           To_Unbounded_String (Index_Table.Key (Cursor));
         Index_Table.Next (Cursor);
      end loop;

      if Sort then
         --  ??? should use a sort from AI302
         for K in Result'Range loop
            for J in K .. Result'Last loop
               if Result (K) > Result (J) then
                  declare
                     Tmp : Unbounded_String := Result (K);
                  begin
                     Result (K) := Result (J);
                     Result (J) := Tmp;
                  end;
               end if;
            end loop;
         end loop;
      end if;

      return Result;
   end Get_Names;

   ---------------
   -- Get_Value --
   ---------------

   function Get_Value
     (Table : in Table_Type;
      N     : in Positive := 1)
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
      Found : Boolean;
   begin
      pragma Assert (Table.Index /= null);

      Get_Indexes (Table, Name, Value, Found);

      if Found then
         declare
            Last   : constant Key_Positive := Name_Indexes.Last (Value);
            Result : VString_Array (1 .. Natural (Last));
         begin
            for I in Name_Indexes.First .. Last loop
               Result (Natural (I))
                  := To_Unbounded_String
                        (Table.Data.Table (Value.Table (I)).Value);
            end loop;
            return Result;
         end;
      else
         return (1 .. 0 => Null_Unbounded_String);
      end if;
   end Get_Values;

   ----------------
   -- Name_Count --
   ----------------

   function Name_Count (Table : in Table_Type) return Natural is
   begin
      if Table.Index = null then
         return 0;
      else
         return Natural (Index_Table.Length (Table.Index.all));
      end if;
   end Name_Count;

   --------------------
   -- Normalize_Name --
   --------------------

   function Normalize_Name
     (Name     : in String;
      To_Upper : in Boolean)
      return String is
   begin
      if To_Upper then
         return Ada.Characters.Handling.To_Upper (Name);
      else
         return Name;
      end if;
   end Normalize_Name;

end AWS.Containers.Tables;
