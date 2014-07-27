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

pragma Ada_2012;

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
     (Table   : Table_Type;
      Name    : String;
      Indexes : out Name_Index_Table;
      Found   : out Boolean)
     with Inline;
   --  Returns all Name/Value indexes for the specified name.
   --  Found is set to False if Name was not found in Table and True otherwise.

   --------------------
   -- Case_Sensitive --
   --------------------

   function Case_Sensitive (Table : Table_Type) return Boolean is
   begin
      return Table.Case_Sensitive;
   end Case_Sensitive;

   -----------
   -- Count --
   -----------

   function Count (Table : Table_Type) return Natural is
   begin
      return Natural (Data_Table.Length (Table.Data));
   end Count;

   -----------
   -- Count --
   -----------

   function Count (Table : Table_Type; Name : String) return Natural is
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

   function Exist (Table : Table_Type; Name : String) return Boolean is
   begin
      return Index_Table.Contains
        (Table.Index, Normalize_Name (Name, not Table.Case_Sensitive));
   end Exist;

   ---------------------------
   -- Generic_Iterate_Names --
   ---------------------------

   procedure Generic_Iterate_Names
     (Table : Table_Type; Separator : String)
   is
      use type Ada.Containers.Count_Type;

      CN  : Index_Table.Cursor;
      NI  : Name_Indexes.Vector;
      Idx : Key_Positive;

   begin
      CN := Index_Table.First (Table.Index);

      while Index_Table.Has_Element (CN) loop
         NI := Index_Table.Element (CN);

         Idx := Name_Indexes.Element (NI, 1);

         if Name_Indexes.Length (NI) = 1 then
            Process
              (Data_Table.Element (Table.Data, Idx).Name,
               Data_Table.Element (Table.Data, Idx).Value);
         else
            declare
               Value : Unbounded_String;
            begin
               for J in 1 .. Positive (Name_Indexes.Length (NI)) loop
                  Idx := Name_Indexes.Element (NI, J);

                  Append (Value, Data_Table.Element (Table.Data, Idx).Value);

                  if J < Positive (Name_Indexes.Length (NI)) then
                     Append (Value, Separator);
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
     (Table : Table_Type;
      Name  : String;
      N     : Positive := 1) return String
   is
      Value : Name_Index_Table;
      Found : Boolean;
   begin
      Get_Indexes (Table, Name, Value, Found);

      if Found and then N <= Natural (Name_Indexes.Length (Value)) then
         return Data_Table.Element
           (Table.Data, Name_Indexes.Element (Value, N)).Value;
      else
         return "";
      end if;
   end Get;

   function Get (Table : Table_Type; N : Positive) return Element is
   begin
      if N <= Natural (Data_Table.Length (Table.Data)) then
         return Data_Table.Element (Table.Data, Key_Positive (N));
      else
         return Null_Element;
      end if;
   end Get;

   -----------------
   -- Get_Indexes --
   -----------------

   procedure Get_Indexes
     (Table   : Table_Type;
      Name    : String;
      Indexes : out Name_Index_Table;
      Found   : out Boolean)
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
     (Table : Table_Type; N : Positive := 1) return String is
   begin
      if N <= Natural (Data_Table.Length (Table.Data)) then
         return Data_Table.Element (Table.Data, Key_Positive (N)).Name;
      else
         return "";
      end if;
   end Get_Name;

   ---------------
   -- Get_Names --
   ---------------

   function Get_Names
     (Table : Table_Type; Sort : Boolean := False) return VString_Array
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
     (Table : Table_Type; N : Positive := 1) return String is
   begin
      if N <= Natural (Data_Table.Length (Table.Data)) then
         return Data_Table.Element (Table.Data, Key_Positive (N)).Value;
      else
         return "";
      end if;
   end Get_Value;

   ----------------
   -- Get_Values --
   ----------------

   function Get_Values
     (Table : Table_Type; Name : String) return VString_Array
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
                        (Table.Data, Name_Indexes.Element (Value, I)).Value);
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
     (Table     : Table_Type;
      Separator : String;
      Process   : not null access procedure (Name, Value : String))
   is
      procedure For_Each is new Generic_Iterate_Names (Process.all);
   begin
      For_Each (Table, Separator);
   end Iterate_Names;

   ----------------
   -- Name_Count --
   ----------------

   function Name_Count (Table : Table_Type) return Natural is
   begin
      return Natural (Index_Table.Length (Table.Index));
   end Name_Count;

   --------------------
   -- Normalize_Name --
   --------------------

   function Normalize_Name
     (Name : String; To_Upper : Boolean) return String is
   begin
      if To_Upper then
         return Ada.Characters.Handling.To_Upper (Name);
      else
         return Name;
      end if;
   end Normalize_Name;

end AWS.Containers.Tables;
