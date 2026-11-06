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

package body SOAP.Types.Set is

   ------------
   -- Append --
   ------------

   procedure Append (Container : in out Object; New_Item : Types.Object'Class)
   is
   begin
      Container.V.Append (New_Item);
   end Append;

   ------------------------
   -- Constant_Reference --
   ------------------------

   function Constant_Reference
     (Container : aliased Set.Object; Position : Cursor)
      return Constant_Reference_Type is
   begin
      return
        (Element =>
           Container.V.Constant_Reference (Set_Pck.Cursor (Position))
             .Element,
         R       =>
           Container.V.Constant_Reference (Set_Pck.Cursor (Position)));
   end Constant_Reference;

   function Constant_Reference
     (Container : aliased Set.Object; Index : Positive)
      return Constant_Reference_Type is
   begin
      return
        (Element => Container.V.Constant_Reference (Index).Element,
         R       => Container.V.Constant_Reference (Index));
   end Constant_Reference;

   -----------
   -- Empty --
   -----------

   function Empty (Capacity : Containers.Count_Type := 10) return Set.Object is
   begin
      return Set.Object'(V => Set_Pck.Empty (Capacity));
   end Empty;

   -------------------
   -- First_Element --
   -------------------

   function First_Element (Container : Set.Object) return Types.Object'Class is
   begin
      return Container.V.First_Element;
   end First_Element;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (Container : Set.Object) return Boolean is
   begin
      return Container.V.Is_Empty;
   end Is_Empty;

   -------------
   -- Iterate --
   -------------

   function Iterate
     (Container : Object)
      return Object_Iterator_Interfaces.Reversible_Iterator'Class is
   begin
      return
        R_Iterator'
          (Object_Iterator_Interfaces.Reversible_Iterator
           with I => Container.V.Iterate'Unrestricted_Access);
   end Iterate;

   function Iterate
     (Container : Object; Start : Cursor)
      return Object_Iterator_Interfaces.Reversible_Iterator'Class is
   begin
      return
        R_Iterator'
          (Object_Iterator_Interfaces.Reversible_Iterator
           with
             I =>
               Container.V.Iterate
                 (Set_Pck.Cursor (Start))'Unrestricted_Access);
   end Iterate;

   ------------
   -- Length --
   ------------

   function Length (Container : Set.Object) return Containers.Count_Type is
   begin
      return Container.V.Length;
   end Length;

   ----------------
   -- New_Vector --
   ----------------

   function New_Vector (First, Last : Positive) return Set.Object is
   begin
      return Set.Object'(V => Set_Pck.New_Vector (First, Last));
   end New_Vector;

   ---------------
   -- Reference --
   ---------------

   function Reference
     (Container : aliased in out Object; Position : Cursor)
      return Reference_Type is
   begin
      return
        (Element => Container.V.Reference (Set_Pck.Cursor (Position)).Element,
         R       => Container.V.Reference (Set_Pck.Cursor (Position)));
   end Reference;

   function Reference
     (Container : aliased in out Object; Index : Positive)
      return Reference_Type is
   begin
      return
        (Element => Container.V.Reference (Index).Element,
         R       => Container.V.Reference (Index));
   end Reference;

   ---------------------
   -- Replace_Element --
   ---------------------

   procedure Replace_Element
     (Container : in out Object;
      Index     : Positive;
      New_Item  : Types.Object'Class) is
   begin
      Container.V.Replace_Element (Index, New_Item);
   end Replace_Element;

   procedure Replace_Element
     (Container : in out Object;
      Position  : Cursor;
      New_Item  : Types.Object'Class) is
   begin
      Container.V.Replace_Element (Set_Pck.Cursor (Position), New_Item);
   end Replace_Element;

   ----------------------
   -- Reserve_Capacity --
   ----------------------

   procedure Reserve_Capacity
     (Container : in out Object; Capacity : Containers.Count_Type) is
   begin
      Container.V.Reserve_Capacity (Capacity);
   end Reserve_Capacity;

end SOAP.Types.Set;