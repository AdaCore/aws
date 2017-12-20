------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2000-2017, AdaCore                     --
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

with Ada.Strings.Unbounded;

private with Ada.Containers.Indefinite_Ordered_Maps;
private with Ada.Containers.Indefinite_Vectors;
private with Ada.Containers.Vectors;

package AWS.Containers.Tables is

   use Ada.Strings.Unbounded;

   type Table_Type is tagged private;

   Empty_Table : constant Table_Type;

   type Element is record
      Name  : Unbounded_String;
      Value : Unbounded_String;
   end record;
   --  Data type to store name/value pair retrieved from a Table_Type

   Null_Element : constant Element;

   type VString_Array is array (Positive range <>) of Unbounded_String;

   function Count (Table : Table_Type) return Natural;
   --  Returns the number of items in Table

   function Is_Empty (Table : Table_Type) return Boolean;
   --  Returns true if table is empty

   function Name_Count (Table : Table_Type) return Natural;
   --  Returns the number of unique key name in Table

   function Case_Sensitive (Table : Table_Type) return Boolean with Inline;
   --  Returns case sensitivity flag of the Table

   function Count (Table : Table_Type; Name : String) return Natural;
   --  Returns the number of value for Key Name in Table. It returns
   --  0 if Key does not exist.

   function Exist (Table : Table_Type; Name : String) return Boolean;
   --  Returns True if Key exist in Table

   function Get
     (Table : Table_Type;
      Name  : String;
      N     : Positive := 1) return String
   with Post => (if N > Count (Table, Name) then Get'Result'Length = 0);
   --  Returns the Nth value associated with Key into Table. Returns
   --  the emptry string if key does not exist.

   function Get_Name
     (Table : Table_Type; N : Positive := 1) return String
   with Post => (if N > Count (Table) then Get_Name'Result'Length = 0);
   --  Returns the Nth Name in Table or the empty string if there is
   --  no parameter with this number.

   function Get_Value
     (Table : Table_Type; N : Positive := 1) return String
   with Post => (if N > Count (Table) then Get_Value'Result'Length = 0);
   --  Returns the Nth Value in Table or the empty string if there is
   --  no parameter with this number.

   function Get (Table : Table_Type; N : Positive) return Element with
     Post => (if N > Count (Table) then Get'Result = Null_Element);
   --  Returns N'th name/value pair. Returns Null_Element if there is no
   --  such item in the table.

   function Get_Names (Table : Table_Type) return VString_Array
   with Post => Get_Names'Result'Length = Name_Count (Table);
   --  Returns sorted array of unique key names

   function Get_Values
     (Table : Table_Type; Name : String) return VString_Array
   with Post => Get_Values'Result'Length = Count (Table, Name);
   --  Returns all values for the specified parameter key name

   generic
      with procedure Process (Name, Value : String);
   procedure Generic_Iterate_Names
     (Table : Table_Type; Separator : String);
   --  Iterates over all names in the table.
   --  All Values of the same name are separated by Separator string.

   procedure Iterate_Names
     (Table     : Table_Type;
      Separator : String;
      Process   : not null access procedure (Name, Value : String));

   function Union
     (Left   : Table_Type;
      Right  : Table_Type;
      Unique : Boolean) return Table_Type;
   --  Concatenates two tables, If Unique is True do not add Right container
   --  element into result when element with the same name already exists in
   --  the Left container.

   procedure Add (Table : in out Table_Type; Name, Value : String);

   procedure Add
     (Table       : in out Table_Type;
      Name, Value : Unbounded_String)
   with Post => Count (Table) = Count (Table'Old) + 1
               or else
                Count (Table, To_String (Name))
                = Count (Table'Old, To_String (Name)) + 1;
   --  Add a new Key/Value pair into Table. A new value is always added,
   --  even if there is already an entry with the same name.

   procedure Update
     (Table : in out Table_Type;
      Name  : String;
      Value : String;
      N     : Positive := 1);

   procedure Update
     (Table : in out Table_Type;
      Name  : Unbounded_String;
      Value : Unbounded_String;
      N     : Positive := 1)
   with
     Pre  =>
       --  Count + 1 means it is added at the end of the table
       N <= Count (Table, To_String (Name)) + 1,
     Post =>
       --  Value already exists, it is updated
       (N <= Count (Table'Old, To_String (Name))
        and then Count (Table, To_String (Name))
                 = Count  (Table'Old, To_String (Name)))
       --  New value appended
       or else
         (N = Count (Table'Old, To_String (Name)) + 1
          and then N = Count (Table, To_String (Name)));
   --  Update the N-th Value with the given Name into the Table.
   --  The container could already have more than one value associated with
   --  this name.

   procedure Case_Sensitive
     (Table : in out Table_Type;
      Mode  : Boolean);
   --  If Mode is True it will use all parameters with case sensitivity

   procedure Reset (Table : in out Table_Type) with
     Post => Count (Table) = 0;
   --  Removes all object from Table. Table will be reinitialized and will be
   --  ready for new use.

private

   Null_Element : constant Element := (others => Null_Unbounded_String);

   type Key_Positive is new Positive;

   package Name_Indexes is
     new Ada.Containers.Vectors (Positive, Key_Positive);

   subtype Name_Index_Table is Name_Indexes.Vector;

   package Data_Table is
     new Ada.Containers.Indefinite_Vectors (Key_Positive, Element);

   package Index_Table is new Ada.Containers.Indefinite_Ordered_Maps
     (String, Name_Index_Table, "<", Name_Indexes."=");
   --  Index of the Element_Array

   subtype Index_Table_Type is Index_Table.Map;

   type Table_Type is tagged record
      Case_Sensitive : Boolean := True;
      Index          : Index_Table.Map;
      --  Index to find appropriate Name/Value pairs in Data by the name
      Data           : Data_Table.Vector;
      --  Ordered array of name and value pairs
   end record;

   Empty_Table : constant Table_Type :=
                   (True, Index_Table.Empty_Map, Data_Table.Empty_Vector);

   function Normalize_Name
     (Name : String; To_Upper : Boolean) return String;
   --  Returns Name in upper case if To_Upper is set to True and it returns
   --  Name unchanged otherwise.

end AWS.Containers.Tables;
