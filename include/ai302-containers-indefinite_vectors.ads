------------------------------------------------------------------------------
--                                                                          --
--                   AI-302 Reference Implementation                        --
--                                                                          --
--              Copyright (C) 2003-2004 Matthew J Heaney                    --
--                                                                          --
-- The AI-302 Reference Implementation is free software; you can            --
-- redistribute it and/or modify it under terms of the GNU General Public   --
-- License as published by the Free Software Foundation; either version 2,  --
-- or (at your option) any later version.  Charles is distributed in the    --
-- hope that it will be useful, but WITHOUT ANY WARRANTY; without even the  --
-- implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. --
-- See the GNU General Public License for more details.  You should have    --
-- received a copy of the GNU General Public License distributed with       --
-- Charles;  see file COPYING.TXT.  If not, write to the Free Software      --
-- Foundation,  59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.    --
--                                                                          --
-- As a special exception, if other files instantiate generics from this    --
-- unit, or you link this unit with other files to produce an executable,   --
-- this unit does not by itself cause the resulting executable to be        --
-- covered by the GNU General Public License.  This exception does not      --
-- however invalidate any other reasons why the executable file might be    --
-- covered by the GNU Public License.                                       --
--                                                                          --
-- The AI-302 Reference Implementation is maintained by Matthew J Heaney.   --
--                                                                          --
-- mailto:matthewjheaney@earthlink.net                                      --
-- http://home.earthlink.net/~matthewjheaney/index.html                     --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Finalization;
with Ada.Streams;

generic

   type Index_Type is range <>;

   type Element_Type (<>) is private;

   with function "=" (Left, Right : Element_Type)
     return Boolean is <>;

package AI302.Containers.Indefinite_Vectors is
   pragma Preelaborate (Indefinite_Vectors);

   pragma Assert (Index_Type'Base'First < Index_Type'First);

   subtype Index_Subtype is Index_Type;

   type Vector is tagged private;

   type Cursor is private;

   function Empty_Vector return Vector;
   --  NOTE:
   --  The subcommittee report has this as a constant,
   --  but you can't do that without an Ada 0X compiler.
   --  For now I'll declare this as a function.

   No_Element : constant Cursor;

   function To_Vector (Count : Count_Type) return Vector;

   function To_Vector (New_Item : Element_Type;
                       Count    : Count_Type)
     return Vector;

   function "&" (Left, Right : Vector) return Vector;

   function "&" (Left  : Vector;
                 Right : Element_Type) return Vector;

   function "&" (Left  : Element_Type;
                 Right : Vector) return Vector;

   function "&" (Left, Right : Element_Type) return Vector;

   function "=" (Left, Right : Vector) return Boolean;

   function Capacity (Container : Vector) return Count_Type;

   procedure Set_Capacity (Container : in out Vector;
                           Capacity  : in     Count_Type);

   function Length (Container : Vector) return Count_Type;

   function Is_Empty (Container : Vector) return Boolean;

   procedure Clear (Container : in out Vector);

   function To_Cursor (Container : Vector;
                       Index     : Index_Type'Base)
      return Cursor;

   function To_Index (Position : Cursor) return Index_Type'Base;

   function Element (Container : Vector;
                     Index     : Index_Type'Base)
      return Element_Type;

   function Element (Position : Cursor) return Element_Type;

   generic
      with procedure Process (Element : in out Element_Type);
   procedure Generic_Update_Element_By_Index (Container : in Vector;
                                              Index     : in Index_Type'Base);

   generic
      with procedure Process (Element : in out Element_Type);
   procedure Generic_Update_Element (Position : in Cursor);

   procedure Replace_Element (Container : in Vector;
                              Index     : in Index_Type'Base;
                              By        : in Element_Type);

   procedure Replace_Element (Position : in Cursor;
                              By       : in Element_Type);

   procedure Assign (Target : in out Vector;
                     Source : in     Vector);

   procedure Move (Target : in out Vector;
                   Source : in out Vector);

   procedure Insert (Container : in out Vector;
                     Before    : in     Index_Type'Base;
                     New_Item  : in     Vector);

   procedure Insert (Container : in out Vector;
                     Before    : in     Cursor;
                     New_Item  : in     Vector);

   procedure Insert (Container : in out Vector;
                     Before    : in     Cursor;
                     New_Item  : in     Vector;
                     Position  :    out Cursor);

   procedure Insert (Container : in out Vector;
                     Before    : in     Index_Type'Base;
                     New_Item  : in     Element_Type;
                     Count     : in     Count_Type := 1);

   procedure Insert (Container : in out Vector;
                     Before    : in     Cursor;
                     New_Item  : in     Element_Type;
                     Count     : in     Count_Type := 1);

   procedure Insert (Container : in out Vector;
                     Before    : in     Cursor;
                     New_Item  : in     Element_Type;
                     Position  :    out Cursor;
                     Count     : in     Count_Type := 1);

   procedure Prepend (Container : in out Vector;
                      New_Item  : in     Vector);

   procedure Prepend (Container : in out Vector;
                      New_Item  : in     Element_Type;
                      Count     : in     Count_Type := 1);

   procedure Append (Container : in out Vector;
                     New_Item  : in     Vector);

   procedure Append (Container : in out Vector;
                     New_Item  : in     Element_Type;
                     Count     : in     Count_Type := 1);

   procedure Insert_Space (Container : in out Vector;
                           Before    : in     Index_Type'Base;
                           Count     : in     Count_Type := 1);

   procedure Insert_Space (Container : in out Vector;
                           Before    : in     Cursor;
                           Position  :    out Cursor;
                           Count     : in     Count_Type := 1);

   procedure Set_Length (Container : in out Vector;
                         Length    : in     Count_Type);

   procedure Delete (Container : in out Vector;
                     Index     : in     Index_Type'Base;
                     Count     : in     Count_Type := 1);

   procedure Delete (Container : in out Vector;
                     Position  : in out Cursor;
                     Count     : in     Count_Type := 1);

   procedure Delete_First (Container : in out Vector;
                           Count     : in     Count_Type := 1);

   procedure Delete_Last (Container : in out Vector;
                          Count     : in     Count_Type := 1);

   function First_Index (Container : Vector) return Index_Type;

   function First (Container : Vector) return Cursor;

   function First_Element (Container : Vector) return Element_Type;

   function Last_Index (Container : Vector) return Index_Type'Base;

   function Last (Container : Vector) return Cursor;

   function Last_Element (Container : Vector) return Element_Type;

   procedure Swap (Container : in Vector;
                   I, J      : in Index_Type'Base);

   procedure Swap (Container : in Vector;
                   I, J      : in Cursor);

   generic
      with function "<" (Left, Right : Element_Type) return Boolean is <>;
   procedure Generic_Sort (Container : in Vector);

   function Find (Container : Vector;
                  Item      : Element_Type;
                  Index     : Index_Type'Base := Index_Type'First)
      return Index_Type'Base;

   function Find (Container : Vector;
                  Item      : Element_Type;
                  Position  : Cursor := No_Element)
      return Cursor;

   function Reverse_Find (Container : Vector;
                          Item      : Element_Type;
                          Index     : Index_Type'Base := Index_Type'Last)
      return Index_Type'Base;

   function Reverse_Find (Container : Vector;
                          Item      : Element_Type;
                          Position  : Cursor := No_Element)
      return Cursor;

   function Is_In (Item      : Element_Type;
                   Container : Vector)
      return Boolean;

   function Next (Position : Cursor) return Cursor;

   function Previous (Position : Cursor) return Cursor;

   procedure Next (Position : in out Cursor);

   procedure Previous (Position : in out Cursor);

   function Has_Element (Position : Cursor) return Boolean;

   generic
      with procedure Process (Position : in Cursor);
   procedure Generic_Iteration (Container : in Vector);

   generic
      with procedure Process (Position : in Cursor);
   procedure Generic_Reverse_Iteration (Container : in Vector);

private

   type Element_Access is access Element_Type;

   type Elements_Type is array (Index_Type range <>) of Element_Access;

   function "=" (L, R : Elements_Type) return Boolean is abstract;

   type Elements_Access is access Elements_Type;

   use Ada.Finalization;

   subtype Last_Subtype is Index_Type'Base range
     Index_Type'Pred (Index_Type'First) .. Index_Type'Last;

   type Vector is new Controlled with record
      Elements : Elements_Access;
      Last     : Last_Subtype := Last_Subtype'First;
   end record;

   procedure Adjust (Container : in out Vector);

   procedure Finalize (Container : in out Vector);


   use Ada.Streams;

   procedure Write
     (Stream    : access Root_Stream_Type'Class;
      Container : in     Vector);

   for Vector'Write use Write;


   procedure Read
     (Stream    : access Root_Stream_Type'Class;
      Container :    out Vector);

   for Vector'Read use Read;


   type Vector_Constant_Access is access constant Vector;
   for Vector_Constant_Access'Storage_Size use 0;

   type Cursor is record
      Container : Vector_Constant_Access;
      Index     : Index_Type'Base := Index_Type'Pred (Index_Type'First);
   end record;

   No_Element : constant Cursor :=
     (Container => null,
      Index     => Index_Type'Pred (Index_Type'First));


end AI302.Containers.Indefinite_Vectors;

