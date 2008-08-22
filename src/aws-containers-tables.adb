------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2000-2008, AdaCore                     --
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

--  Parameters name/value are put into the Table_Type.Data field (vector). The
--  name as a key and the numeric index as a value is placed into map for fast
--  retrieval of all Name/Value pairs having the same name. Each value in the
--  map is a table of numeric indexes pointing into the Data field. The
--  parameters must be accessible using their name (string index) but also
--  using an numeric index. So given a set of parameters (K1=V1, K2=V2...), one
--  must be able to ask for the value for K1 but also the name of the second
--  key or the value of the third key.
--
--  Each K/V pair is then inserted into the Data table for access by numeric
--  index. And its numeric index is placed into the map indexed by name.

with Ada.Characters.Handling;

with Ada.Containers.Generic_Array_Sort;

package body AWS.Containers.Tables is

   procedure Get_Indexes
     (Table   : in     Table_Type;
      Name    : in     String;
      Indexes :    out Name_Index_Table;
      Found   :    out Boolean);
   pragma Inline (Get_Indexes);
   --  Returns all Name/Value indexes for the specified name.
   --  Found is set to False if Name was not found in Table and True otherwise.

   --------------------
   -- Case_Sensitive --
   --------------------

   function Case_Sensitive (Table : in Table_Type) return Boolean is
   begin
      return Table.Case_Sensitive;
   end Case_Sensitive;

   -----------
   -- Count --
   -----------

   function Count (Table : in Table_Type) return Natural is
   begin
      return Natural (Data_Table.Length (Table.Data));
   end Count;

   -----------
   -- Count --
   -----------

   function Count (Table : in Table_Type; Name : in String) return Natural is
      Value : Name_Index_Table;
      Found : Boolean;
   begin
      Get_Indexes (Table, Name, Value, Found);

      if Found then
         return Natural (Name_Indexes.Length (Value));
      else
         return 0;
      end if;
   end Count;

   -----------
   -- Exist --
   -----------

   function Exist (Table : in Table_Type; Name : in String) return Boolean is
   begin
      return Index_Table.Contains
        (Table.Index, Normalize_Name (Name, not Table.Case_Sensitive));
   end Exist;

   ---------------------------
   -- Generic_Iterate_Names --
   ---------------------------

   procedure Generic_Iterate_Names
     (Table : in Table_Type; Coupler : in String)
   is
      use type Ada.Containers.Count_Type;

      CN  : Index_Table.Cursor;
      NI  : Name_Indexes.Vector;
      Idx : Positive;

   begin
      CN := Index_Table.First (Table.Index);

      while Index_Table.Has_Element (CN) loop
         NI := Index_Table.Element (CN);

         Idx := Positive (Name_Indexes.Element (NI, 1));

         if Name_Indexes.Length (NI) = 1 then
            Process
              (Data_Table.Element (Table.Data, Idx).Name,
               Data_Table.Element (Table.Data, Idx).Value);
         else
            declare
               Value : Unbounded_String;
            begin
               for J in 1 .. Positive (Name_Indexes.Length (NI)) loop
                  Idx := Positive (Name_Indexes.Element (NI, J));

                  Append (Value, Data_Table.Element (Table.Data, Idx).Value);

                  if J < Positive (Name_Indexes.Length (NI)) then
                     Append (Value, Coupler);
                  end if;
               end loop;

               Process
                 (Data_Table.Element (Table.Data, Idx).Name,
                  To_String (Value));
            end;
         end if;

         CN := Index_Table.Next (CN);
      end loop;
   end Generic_Iterate_Names;

   ---------
   -- Get --
   ---------

   function Get
     (Table : in Table_Type;
      Name  : in String;
      N     : in Positive := 1) return String
   is
      Value : Name_Index_Table;
      Found : Boolean;
   begin
      Get_Indexes (Table, Name, Value, Found);

      if Found and then N <= Natural (Name_Indexes.Length (Value)) then
         return Data_Table.Element
           (Table.Data,
            Natural ((Name_Indexes.Element (Value, N)))).Value;
      else
         return "";
      end if;
   end Get;

   function Get (Table : in Table_Type; N : in Positive) return Element is
   begin
      if N <= Natural (Data_Table.Length (Table.Data)) then
         return Data_Table.Element (Table.Data, N);
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
        (Table.Index, Normalize_Name (Name, not Table.Case_Sensitive));

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
     (Table : in Table_Type; N : in Positive := 1) return String is
   begin
      if N <= Natural (Data_Table.Length (Table.Data)) then
         return Data_Table.Element (Table.Data, N).Name;
      else
         return "";
      end if;
   end Get_Name;

   ---------------
   -- Get_Names --
   ---------------

   function Get_Names
     (Table : in Table_Type; Sort : in Boolean := False) return VString_Array
   is
      procedure Sort_Names is
        new Ada.Containers.Generic_Array_Sort
          (Positive, Unbounded_String, VString_Array);

      Result : VString_Array (1 .. Name_Count (Table));
      Cursor : Index_Table.Cursor;
      Index  : Natural := Result'First - 1;
   begin
      Cursor := Index_Table.First (Table.Index);

      while Index_Table.Has_Element (Cursor) loop
         Index := Index + 1;
         Result (Index) := To_Unbounded_String (Index_Table.Key (Cursor));
         Index_Table.Next (Cursor);
      end loop;

      if Sort then
         Sort_Names (Result);
      end if;

      return Result;
   end Get_Names;

   ---------------
   -- Get_Value --
   ---------------

   function Get_Value
     (Table : in Table_Type; N : in Positive := 1) return String is
   begin
      if N <= Natural (Data_Table.Length (Table.Data)) then
         return Data_Table.Element (Table.Data, N).Value;
      else
         return "";
      end if;
   end Get_Value;

   ----------------
   -- Get_Values --
   ----------------

   function Get_Values
     (Table : in Table_Type; Name : in String) return VString_Array
   is
      Value : Name_Index_Table;
      Found : Boolean;
   begin
      Get_Indexes (Table, Name, Value, Found);

      if Found then
         declare
            Last   : constant Natural
              := Natural (Name_Indexes.Length (Value));
            Result : VString_Array (1 .. Last);
         begin
            for I in 1 .. Last loop
               Result (I)
                  := To_Unbounded_String
                   (Data_Table.Element
                        (Table.Data,
                         Natural ((Name_Indexes.Element (Value, I)))).Value);
            end loop;
            return Result;
         end;

      else
         return (1 .. 0 => Null_Unbounded_String);
      end if;
   end Get_Values;

   -------------------
   -- Iterate_Names --
   -------------------

   procedure Iterate_Names
     (Table   : in Table_Type;
      Coupler : in String;
      Process : access procedure (Name, Value : in String))
   is
      procedure For_Each is new Generic_Iterate_Names (Process.all);
   begin
      For_Each (Table, Coupler);
   end Iterate_Names;

   ----------------
   -- Name_Count --
   ----------------

   function Name_Count (Table : in Table_Type) return Natural is
   begin
      return Natural (Index_Table.Length (Table.Index));
   end Name_Count;

   --------------------
   -- Normalize_Name --
   --------------------

   function Normalize_Name
     (Name : in String; To_Upper : in Boolean) return String is
   begin
      if To_Upper then
         return Ada.Characters.Handling.To_Upper (Name);
      else
         return Name;
      end if;
   end Normalize_Name;

end AWS.Containers.Tables;
