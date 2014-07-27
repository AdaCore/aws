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

package AWS.Containers.Tables.Set is

   procedure Add
     (Table       : in out Table_Type;
      Name, Value : String)
   with Post => Count (Table) = Count (Table'Old) + 1
               or else
                Count (Table, Name) = Count (Table'Old, Name) + 1;
   --  Add a new Key/Value pair into Table. A new value is always added,
   --  even if there is already an entry with the same name.

   procedure Update
     (Table : in out Table_Type;
      Name  : String;
      Value : String;
      N     : Positive := 1)
   with
     Pre  =>
       --  Count + 1 means it is added at the end of the table
       N <= Count (Table, Name) + 1,
     Post =>
       --  Value already exists, it is updated
       (N <= Count (Table'Old, Name)
        and then Count (Table, Name) = Count  (Table'Old, Name))
       --  New value appended
       or else
         (N = Count (Table'Old, Name) + 1
          and then N = Count (Table, Name));
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

end AWS.Containers.Tables.Set;
