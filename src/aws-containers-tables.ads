------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2000-2002                          --
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

with Ada.Strings.Unbounded;

with AWS.Containers.Key_Value;

package AWS.Containers.Tables is

   type Table_Type is tagged private;

   type VString_Array is array (Positive range <>)
     of Ada.Strings.Unbounded.Unbounded_String;

   function Count (Table : in Table_Type) return Natural;
   --  Returns the number of item in Table.

   function Name_Count (Table : in Table_Type) return Natural;
   --  Returns the number of unique key name in Table.

   function Count (Table : in Table_Type; Name : in String) return Natural;
   --  Returns the number of value for Key Name in Table. It returns
   --  0 if Key does not exist.

   function Exist (Table : in Table_Type; Name : in String) return Boolean;
   --  Returns True if Key exist in Table.

   function Get
     (Table : in Table_Type;
      Name  : in String;
      N     : in Positive := 1)
      return String;
   --  Returns the Nth value associated with Key into Table. Returns
   --  the emptry string if key does not exist.

   function Get_Name
     (Table : in Table_Type;
      N     : in Positive := 1)
      return String;
   --  Returns the Nth Name in Table or the empty string if there is
   --  no parameter with this number.

   function Get_Value
     (Table : in Table_Type;
      N     : in Positive := 1)
      return String;
   --  Returns the Nth Value in Table or the empty string if there is
   --  no parameter with this number.

   function Get_Names (Table : in Table_Type) return VString_Array;
   --  Returns array of unique key names.

   function Get_Values
     (Table : in Table_Type;
      Name  : in String)
      return VString_Array;
   --  Returns all values for the specified parameter key name.

private
   --  A Table_Type must be initialized by calling
   --  AWS.Containers.Tables.Set.Reset, Server is responsible for doing that.

   type Table_Type is tagged record
      Case_Sensitive : Boolean := True;
      Data           : Key_Value.Set_Access;
      --  Main data tree.
      Ordered_Data   : Key_Value.Set_Access;
      --  Tree to record key and value by order.
   end record;

   function Internal_Get
     (Table : in Table_Type;
      Name  : in String;
      N     : in Natural)
      return String;
   pragma Inline (Internal_Get);
   --  Returns the Nth value associated with Key into Table. Returns
   --  the emptry string if key does not exist. If N = 0 it returns as-is all
   --  the values as inserted in the tree for Key.

   Val_Separator : constant Character := ASCII.VT;

   function Normalize_Name
     (Name : in String; To_Upper : in Boolean)
      return String;
   --  Returns Name in upper case if To_Upper is set to True and it returns
   --  Name unchanged otherwise.

end AWS.Containers.Tables;
