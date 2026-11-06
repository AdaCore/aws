------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                       Copyright (C) 2026, AdaCore                        --
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

pragma Ada_2022;

with Ada.Containers;
with Ada.Iterator_Interfaces;

private with Ada.Containers.Indefinite_Vectors;

package SOAP.Types.Set is

   type Object is tagged private
   with
     Constant_Indexing => Constant_Reference,
     Variable_Indexing => Reference,
     Default_Iterator  => Iterate,
     Iterator_Element  => Types.Object'Class,
     Aggregate         =>
       (Empty          => Empty,
        Add_Unnamed    => Append,
        New_Indexed    => New_Vector,
        Assign_Indexed => Replace_Element);
   --  A set of SOAP objects, providing indexed access and iteration
   --  capabilities.

   type Cursor is private;
   No_Element : constant Cursor;

   Empty_Object : constant Object;

   procedure Reserve_Capacity
     (Container : in out Object; Capacity : Containers.Count_Type);

   function Length (Container : Object) return Containers.Count_Type
     with Inline;

   function Empty (Capacity : Containers.Count_Type := 10) return Object
     with Inline;

   function Is_Empty (Container : Object) return Boolean
     with Inline;

   procedure Append
     (Container : in out Object; New_Item : Types.Object'Class)
     with Inline;

   function New_Vector (First, Last : Positive) return Object
     with Inline;

   function Element
     (Container : Object; Index : Positive)
      return Types.Object'Class with Inline;

   function Element
     (Position : Cursor) return Types.Object'Class with Inline;

   function First_Element
     (Container : Object) return Types.Object'Class with Inline;

   procedure Replace_Element
     (Container : in out Object;
      Index     : Positive;
      New_Item  : Types.Object'Class);
   procedure Replace_Element
     (Container : in out Object;
      Position  : Cursor;
      New_Item  : Types.Object'Class);

   function Has_Element (Position : Cursor) return Boolean with Inline;

   package Object_Iterator_Interfaces is new
     Iterator_Interfaces (Cursor, Has_Element);

   function Iterate
     (Container : Object)
      return Object_Iterator_Interfaces.Reversible_Iterator'Class;

   function Iterate
     (Container : Object; Start : Cursor)
      return Object_Iterator_Interfaces.Reversible_Iterator'Class;

   type Constant_Reference_Type
     (Element : not null access constant Types.Object'Class)
   is
     limited private
   with Implicit_Dereference => Element;

   type Reference_Type (Element : not null access Types.Object'Class) is
     limited private
   with Implicit_Dereference => Element;

   function Constant_Reference
     (Container : aliased Object; Position : Cursor)
      return Constant_Reference_Type;

   function Reference
     (Container : aliased in out Object; Position : Cursor)
      return Reference_Type;

   function Constant_Reference
     (Container : aliased Object; Index : Positive)
      return Constant_Reference_Type;

   function Reference
     (Container : aliased in out Object; Index : Positive)
      return Reference_Type;

private

   package Set_Pck is new
     Containers.Indefinite_Vectors (Positive, Types.Object'Class);

   type Object is tagged record
      V : Set_Pck.Vector;
   end record;

   Empty_Object : constant Object := (V => Set_Pck.Empty_Vector);

   --  Cursor

   type Cursor is new Set_Pck.Cursor;
   No_Element : constant Cursor := Cursor (Set_Pck.No_Element);

   --  Iterator

   type R_Iterator is limited
     new Object_Iterator_Interfaces.Reversible_Iterator
   with record
      I :
        aliased access constant
          Set_Pck.Vector_Iterator_Interfaces.Reversible_Iterator'Class;
   end record;

   overriding
   function First (O : R_Iterator) return Cursor
   is (Cursor (Set_Pck.Vector_Iterator_Interfaces.First (O.I.all)))
   with Inline;

   overriding
   function Next (O : R_Iterator; Position : Cursor) return Cursor
   is (Cursor
         (Set_Pck.Vector_Iterator_Interfaces.Next
            (O.I.all, Set_Pck.Cursor (Position))))
   with Inline;

   overriding
   function Last (O : R_Iterator) return Cursor
   is (Cursor (Set_Pck.Vector_Iterator_Interfaces.Last (O.I.all)))
   with Inline;

   overriding
   function Previous (O : R_Iterator; Position : Cursor) return Cursor
   is (Cursor
         (Set_Pck.Vector_Iterator_Interfaces.Previous
            (O.I.all, Set_Pck.Cursor (Position))))
   with Inline;

   -------------
   -- Element --
   -------------

   function Element
     (Container : Object; Index : Positive) return Types.Object'Class
   is (Container.V.Element (Index));

   overriding
   function Element (Position : Cursor) return Types.Object'Class
   is (Set_Pck.Element (Set_Pck.Cursor (Position)));

   -----------------
   -- Has_Element --
   -----------------

   overriding
   function Has_Element (Position : Cursor) return Boolean
   is (Set_Pck.Has_Element (Set_Pck.Cursor (Position)));

   --  References

   type Constant_Reference_Type
     (Element : not null access constant Types.Object'Class)
   is limited record
      R : Set_Pck.Constant_Reference_Type (Element);
   end record;

   type Reference_Type (Element : not null access Types.Object'Class) is
   limited record
      R : Set_Pck.Reference_Type (Element);
   end record;

end SOAP.Types.Set;