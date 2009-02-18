------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2000-2009, AdaCore                     --
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

with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Vectors;
with Ada.Strings.Hash;
with Ada.Strings.Unbounded;

package AWS.Containers.Tables is

   use Ada.Strings.Unbounded;

   type Table_Type is tagged private;

   Empty_Table : constant Table_Type;

   type Element (Name_Length, Value_Length : Natural) is record
      Name  : String (1 .. Name_Length);
      Value : String (1 .. Value_Length);
   end record;
   --  Data type to store name/value pair retrieved from a Table_Type

   Null_Element : constant Element;

   type VString_Array is array (Positive range <>) of Unbounded_String;

   function Count (Table : in Table_Type) return Natural;
   --  Returns the number of item in Table

   function Name_Count (Table : in Table_Type) return Natural;
   --  Returns the number of unique key name in Table

   function Case_Sensitive (Table : in Table_Type) return Boolean;
   pragma Inline (Case_Sensitive);
   --  Returns case sensitivity flag of the Table

   function Count (Table : in Table_Type; Name : in String) return Natural;
   --  Returns the number of value for Key Name in Table. It returns
   --  0 if Key does not exist.

   function Exist (Table : in Table_Type; Name : in String) return Boolean;
   --  Returns True if Key exist in Table

   function Get
     (Table : in Table_Type;
      Name  : in String;
      N     : in Positive := 1) return String;
   --  Returns the Nth value associated with Key into Table. Returns
   --  the emptry string if key does not exist.

   function Get_Name
     (Table : in Table_Type; N : in Positive := 1) return String;
   --  Returns the Nth Name in Table or the empty string if there is
   --  no parameter with this number.

   function Get_Value
     (Table : in Table_Type; N : in Positive := 1) return String;
   --  Returns the Nth Value in Table or the empty string if there is
   --  no parameter with this number.

   function Get (Table : in Table_Type; N : in Positive) return Element;
   --  Returns N'th name/value pair. Returns Null_Element if there is no
   --  such item in the table.

   function Get_Names
     (Table : in Table_Type; Sort : in Boolean := False) return VString_Array;
   --  Returns array of unique key names. If Sort is True, the returned names
   --  array is sorted in alphabetical order. This is of course slightly
   --  slower than returning unsorted results.

   function Get_Values
     (Table : in Table_Type; Name : in String) return VString_Array;
   --  Returns all values for the specified parameter key name

   generic
      with procedure Process (Name, Value : in String);
   procedure Generic_Iterate_Names
     (Table : in Table_Type; Coupler : in String);
   --  Iterates over all names in the table.
   --  All Values of the same name would be given Coupler separated.

   procedure Iterate_Names
     (Table   : in Table_Type;
      Coupler : in String;
      Process : not null access procedure (Name, Value : in String));

private

   Null_Element : constant Element := (0, 0, "", "");

   type Key_Positive is new Positive;

   package Name_Indexes is
     new Ada.Containers.Vectors (Positive, Key_Positive);

   subtype Name_Index_Table is Name_Indexes.Vector;

   package Data_Table is
     new Ada.Containers.Indefinite_Vectors (Key_Positive, Element);

   package Index_Table is
     new Ada.Containers.Indefinite_Hashed_Maps
       (String, Name_Index_Table, Ada.Strings.Hash, "=", Name_Indexes."=");
   --  Index of the Element_Array.

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
     (Name : in String; To_Upper : in Boolean) return String;
   --  Returns Name in upper case if To_Upper is set to True and it returns
   --  Name unchanged otherwise.

end AWS.Containers.Tables;
