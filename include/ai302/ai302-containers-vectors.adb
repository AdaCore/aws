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

with AI302.Containers.Generic_Array_Sort;
with Ada.Unchecked_Deallocation;
with System;  use type System.Address;

package body AI302.Containers.Vectors is

   type Int is range System.Min_Int .. System.Max_Int;

   procedure Free is
      new Ada.Unchecked_Deallocation (Elements_Type, Elements_Access);


   procedure Adjust (Container : in out Vector) is
   begin

      if Container.Elements = null then
         return;
      end if;

      if Container.Elements'Length = 0
        or else Container.Last < Index_Type'First
      then
         Container.Elements := null;
         return;
      end if;

      declare
         X : constant Elements_Access := Container.Elements;
         L : constant Index_Type'Base := Container.Last;
         E : Elements_Type renames X (Index_Type'First .. L);
      begin
         Container.Elements := null;
         Container.Last := Index_Type'Pred (Index_Type'First);

         Container.Elements := new Elements_Type'(E);
         Container.Last := L;
      end;

   end Adjust;


   procedure Finalize (Container : in out Vector) is

      X : Elements_Access := Container.Elements;

   begin

      Container.Elements := null;
      Container.Last := Index_Type'Pred (Index_Type'First);

      Free (X);

   end Finalize;


   procedure Write
     (Stream    : access Root_Stream_Type'Class;
      Container : in     Vector) is

   begin

      Count_Type'Base'Write (Stream, Length (Container));

      for I in Index_Type'First .. Container.Last loop
         Element_Type'Write (Stream, Container.Elements (I));
      end loop;

   end Write;


   procedure Read
     (Stream    : access Root_Stream_Type'Class;
      Container :    out Vector) is

      Length : Count_Type'Base;
      Last   : Index_Type'Base := Index_Type'Pred (Index_Type'First);

   begin

      Clear (Container);

      Count_Type'Base'Read (Stream, Length);

      if Length > Capacity (Container) then
         Set_Capacity (Container, Capacity => Length);
      end if;

      for I in Count_Type range 1 .. Length loop

         Last := Index_Type'Succ (Last);

         Element_Type'Read (Stream, Container.Elements (Last));

         Container.Last := Last;

      end loop;

   end Read;


   function Empty_Vector return Vector is
   begin
      return (Controlled with null, Index_Type'Pred (Index_Type'First));
   end Empty_Vector;


   function To_Vector (Count : Count_Type) return Vector is
   begin

      if Count = 0 then
         return Empty_Vector;
      end if;

      declare

         First : constant Int := Int (Index_Type'First);

         Last_As_Int : constant Int'Base := First + Int (Count) - 1;

         Last : constant Index_Type := Index_Type (Last_As_Int);

         Elements : constant Elements_Access :=
           new Elements_Type (Index_Type'First .. Last);

      begin

         return (Controlled with Elements, Last);

      end;

   end To_Vector;


   function To_Vector
     (New_Item : Element_Type;
      Count    : Count_Type) return Vector is

   begin

      if Count = 0 then
         return Empty_Vector;
      end if;

      declare

         First : constant Int := Int (Index_Type'First);

         Last_As_Int : constant Int'Base := First + Int (Count) - 1;

         Last : constant Index_Type := Index_Type (Last_As_Int);

         Elements : constant Elements_Access :=
           new Elements_Type'(Index_Type'First .. Last => New_Item);

      begin

         return (Controlled with Elements, Last);

      end;

   end To_Vector;



   function "&" (Left, Right : Vector) return Vector is

      LN : constant Count_Type := Length (Left);
      RN : constant Count_Type := Length (Right);

   begin

      if LN = 0 then

         if RN = 0 then
            return Empty_Vector;
         end if;

         declare
            RE : Elements_Type renames
              Right.Elements (Index_Type'First .. Right.Last);

            Elements : constant Elements_Access :=
              new Elements_Type'(RE);
         begin
            return (Controlled with Elements, Right.Last);
         end;

      end if;

      if RN = 0 then

         declare
            LE : Elements_Type renames
              Left.Elements (Index_Type'First .. Left.Last);

            Elements : constant Elements_Access :=
              new Elements_Type'(LE);
         begin
            return (Controlled with Elements, Left.Last);
         end;

      end if;

      declare
         Last_As_Int : constant Int'Base :=
            Int (Index_Type'First) + Int (LN) + Int (RN) - 1;

         Last : constant Index_Type := Index_Type (Last_As_Int);

         LE : Elements_Type renames
           Left.Elements (Index_Type'First .. Left.Last);

         RE : Elements_Type renames
           Right.Elements (Index_Type'First .. Right.Last);

         Elements : constant Elements_Access :=
           new Elements_Type'(LE & RE);
      begin
         return (Controlled with Elements, Last);
      end;

   end "&";


   function "&" (Left  : Vector;
                 Right : Element_Type) return Vector is

      LN : constant Count_Type := Length (Left);

   begin

      if LN = 0 then

         declare
            subtype Elements_Subtype is
              Elements_Type (Index_Type'First .. Index_Type'First);

            Elements : constant Elements_Access :=
              new Elements_Subtype'(others => Right);
         begin
            return (Controlled with Elements, Index_Type'First);
         end;

      end if;

      declare
         Last_As_Int : constant Int'Base :=
            Int (Index_Type'First) + Int (LN);

         Last : constant Index_Type := Index_Type (Last_As_Int);

         LE : Elements_Type renames
           Left.Elements (Index_Type'First .. Left.Last);

         subtype ET is Elements_Type (Index_Type'First .. Last);

         Elements : constant Elements_Access := new ET'(LE & Right);
      begin
         return (Controlled with Elements, Last);
      end;

   end "&";



   function "&" (Left  : Element_Type;
                 Right : Vector) return Vector is

      RN : constant Count_Type := Length (Right);

   begin

      if RN = 0 then

         declare
            subtype Elements_Subtype is
              Elements_Type (Index_Type'First .. Index_Type'First);

            Elements : constant Elements_Access :=
              new Elements_Subtype'(others => Left);
         begin
            return (Controlled with Elements, Index_Type'First);
         end;

      end if;

      declare
         Last_As_Int : constant Int'Base :=
            Int (Index_Type'First) + Int (RN);

         Last : constant Index_Type := Index_Type (Last_As_Int);

         RE : Elements_Type renames
           Right.Elements (Index_Type'First .. Right.Last);

         subtype ET is Elements_Type (Index_Type'First .. Last);

         Elements : constant Elements_Access := new ET'(Left & RE);
      begin
         return (Controlled with Elements, Last);
      end;

   end "&";



   function "&" (Left, Right  : Element_Type) return Vector is

      subtype IT is Index_Type'Base range
        Index_Type'First .. Index_Type'Succ (Index_Type'First);

      subtype ET is Elements_Type (IT);

      Elements : constant Elements_Access := new ET'(Left, Right);
   begin
      return Vector'(Controlled with Elements, Elements'Last);
   end "&";



   function "=" (Left, Right : Vector) return Boolean is
   begin

      if Left'Address = Right'Address then
         return True;
      end if;

      if Left.Last /= Right.Last then
         return False;
      end if;

      for I in Index_Type range Index_Type'First .. Left.Last loop

         if Left.Elements (I) /= Right.Elements (I) then
            return False;
         end if;

      end loop;

      return True;

   end "=";



   function Length (Container : Vector) return Count_Type is

      L : constant Int := Int (Container.Last);
      F : constant Int := Int (Index_Type'First);

      N : constant Int'Base := L - F + 1;
   begin
      return Count_Type (N);
   end Length;


   function Is_Empty (Container : Vector) return Boolean is
   begin
      return Container.Last < Index_Type'First;
   end Is_Empty;


   procedure Clear (Container : in out Vector) is
   begin
      Container.Last := Index_Type'Pred (Index_Type'First);
   end Clear;


   function To_Cursor (Container : Vector;
                       Index     : Index_Type'Base)
      return Cursor is
   begin
      if Index not in Index_Type'First .. Container.Last then
         return No_Element;
      end if;

      return Cursor'(Container'Unchecked_Access, Index);
   end To_Cursor;


   function To_Index (Position : Cursor) return Index_Type'Base is
   begin
      if Position.Container = null then
         raise Constraint_Error;
      end if;

      if Position.Index not in Index_Type'First .. Position.Container.Last then
         raise Constraint_Error;   --  TODO: verify these semantics
      end if;

      return Position.Index;
   end To_Index;



   procedure Assign
     (Target : in out Vector;
      Source : in     Vector) is

      N : constant Count_Type := Length (Source);

   begin

      if Target'Address = Source'Address then
         return;
      end if;

      Clear (Target);

      if N = 0 then
         return;
      end if;

      if N > Capacity (Target) then
         Set_Capacity (Target, Capacity => N);
      end if;

      Target.Elements (Index_Type'First .. Source.Last) :=
        Source.Elements (Index_Type'First .. Source.Last);

      Target.Last := Source.Last;

   end Assign;


   procedure Move
     (Target : in out Vector;
      Source : in out Vector) is

      X : Elements_Access := Target.Elements;

   begin

      if Target'Address = Source'Address then
         return;
      end if;

      if Target.Last >= Index_Type'First then
         raise Constraint_Error;
      end if;

      Target.Elements := null;
      Free (X);

      Target.Elements := Source.Elements;
      Target.Last := Source.Last;

      Source.Elements := null;
      Source.Last := Index_Type'Pred (Index_Type'First);

   end Move;


   procedure Prepend (Container : in out Vector;
                      New_Item  : in     Vector) is
   begin
      Insert (Container,
              Index_Type'First,
              New_Item);
   end Prepend;


   procedure Prepend (Container : in out Vector;
                      New_Item  : in     Element_Type;
                      Count     : in     Count_Type := 1) is
   begin
      Insert (Container,
              Index_Type'First,
              New_Item,
              Count);
   end Prepend;


   procedure Append (Container : in out Vector;
                     New_Item  : in     Vector) is
   begin
      if Is_Empty (New_Item) then
         return;
      end if;

      Insert
        (Container,
         Index_Type'Succ (Container.Last),
         New_Item);
   end Append;



   procedure Append (Container : in out Vector;
                     New_Item  : in     Element_Type;
                     Count     : in     Count_Type := 1) is
   begin
      if Count = 0 then
         return;
      end if;

      Insert
        (Container,
         Index_Type'Succ (Container.Last),
         New_Item,
         Count);
   end Append;


   procedure Insert
     (Container : in out Vector;
      Before    : in     Index_Type'Base;
      New_Item  : in     Element_Type;
      Count     : in     Count_Type := 1) is

      Old_Last : constant Index_Type'Base := Container.Last;

      Old_Last_As_Int : constant Int := Index_Type'Pos (Old_Last);

      N : constant Int := Count_Type'Pos (Count);

      New_Last_As_Int : constant Int'Base := Old_Last_As_Int + N;

      New_Last : constant Last_Subtype := Last_Subtype (New_Last_As_Int);

      Index : Index_Type;

      Dst_Last : Index_Type;
      Dst      : Elements_Access;

   begin

      if Count = 0 then
         return;
      end if;

      declare
         subtype Before_Subtype is Index_Type'Base range
           Index_Type'First .. Index_Type'Succ (Container.Last);

         Old_First : constant Before_Subtype := Before;

         Old_First_As_Int : constant Int := Index_Type'Pos (Old_First);

         New_First_As_Int : constant Int'Base := Old_First_As_Int + N;
      begin
         Index := Index_Type (New_First_As_Int);
      end;

      if Container.Elements = null then

         declare
            subtype Elements_Subtype is
              Elements_Type (Index_Type'First .. New_Last);
         begin
            Container.Elements := new Elements_Subtype'(others => New_Item);
         end;

         Container.Last := New_Last;

         return;

      end if;

      if New_Last <= Container.Elements'Last then

         declare
            E : Elements_Type renames Container.Elements.all;
         begin
            E (Index .. New_Last) := E (Before .. Container.Last);
            E (Before .. Index_Type'Pred (Index)) := (others => New_Item);
         end;

         Container.Last := New_Last;

         return;

      end if;

      declare

         First : constant Int := Int (Index_Type'First);

         New_Size : constant Int'Base :=
           New_Last_As_Int - First + 1;

         Max_Size : constant Int'Base :=
           Int (Index_Type'Last) - First + 1;

         Size, Dst_Last_As_Int : Int'Base;

      begin

         if New_Size >= Max_Size / 2 then

            Dst_Last := Index_Type'Last;

         else

            Size := Container.Elements'Length;

            if Size = 0 then
               Size := 1;
            end if;

            while Size < New_Size loop
               Size := 2 * Size;
            end loop;

            Dst_Last_As_Int := First + Size - 1;
            Dst_Last := Index_Type (Dst_Last_As_Int);

         end if;

      end;

      Dst := new Elements_Type (Index_Type'First .. Dst_Last);

      declare
         Src : Elements_Type renames Container.Elements.all;
      begin
         Dst (Index_Type'First .. Index_Type'Pred (Before)) :=
           Src (Index_Type'First .. Index_Type'Pred (Before));

         Dst (Before .. Index_Type'Pred (Index)) :=
           (others => New_Item);

         Dst (Index .. New_Last) :=
           Src (Before .. Container.Last);
      exception
         when others =>
            Free (Dst);
            raise;
      end;

      declare
         X : Elements_Access := Container.Elements;
      begin
         Container.Elements := Dst;
         Container.Last := New_Last;

         Free (X);
      end;

   end Insert;


--     procedure Insert (Container : in out Vector;
--                       Before    : in     Index_Type'Base;
--                       New_Item  : in     Element_Type;
--                       Count     : in     Count_Type := 1) is
--     begin
--
--        if Count = 0 then
--           return;
--        end if;
--
--        declare
--           subtype T is Index_Type'Base range
--             Index_Type'First .. Index_Type'Succ (Container.Last);
--        begin
--           Insert_Internal (Container,
--                            T'(Before),
--                            Positive_Int (Count),
--                            New_Item);
--        end;
--
--     end Insert;


   procedure Insert_Space
     (Container : in out Vector;
      Before    : in     Index_Type'Base;
      Count     : in     Count_Type := 1) is

      Old_Last : constant Index_Type'Base := Container.Last;

      Old_Last_As_Int : constant Int := Index_Type'Pos (Old_Last);

      N : constant Int := Count_Type'Pos (Count);

      New_Last_As_Int : constant Int'Base := Old_Last_As_Int + N;

      New_Last : constant Last_Subtype := Last_Subtype (New_Last_As_Int);

      Index : Index_Type;

      Dst_Last : Index_Type;
      Dst      : Elements_Access;

   begin

      if Count = 0 then
         return;
      end if;

      declare
         subtype Before_Subtype is Index_Type'Base range
           Index_Type'First .. Index_Type'Succ (Container.Last);

         Old_First : constant Before_Subtype := Before;

         Old_First_As_Int : constant Int := Index_Type'Pos (Old_First);

         New_First_As_Int : constant Int'Base := Old_First_As_Int + N;
      begin
         Index := Index_Type (New_First_As_Int);
      end;

      if Container.Elements = null then

         Container.Elements :=
           new Elements_Type (Index_Type'First .. New_Last);

         Container.Last := New_Last;

         return;

      end if;

      if New_Last <= Container.Elements'Last then

         declare
            E : Elements_Type renames Container.Elements.all;
         begin
            E (Index .. New_Last) := E (Before .. Container.Last);
         end;

         Container.Last := New_Last;

         return;

      end if;

      declare

         First : constant Int := Int (Index_Type'First);

         New_Size : constant Int'Base :=
           New_Last_As_Int - First + 1;

         Max_Size : constant Int'Base :=
           Int (Index_Type'Last) - First + 1;

         Size, Dst_Last_As_Int : Int'Base;

      begin

         if New_Size >= Max_Size / 2 then

            Dst_Last := Index_Type'Last;

         else

            Size := Container.Elements'Length;

            if Size = 0 then
               Size := 1;
            end if;

            while Size < New_Size loop
               Size := 2 * Size;
            end loop;

            Dst_Last_As_Int := First + Size - 1;
            Dst_Last := Index_Type (Dst_Last_As_Int);

         end if;

      end;

      Dst := new Elements_Type (Index_Type'First .. Dst_Last);

      declare
         Src : Elements_Type renames Container.Elements.all;
      begin
         Dst (Index_Type'First .. Index_Type'Pred (Before)) :=
           Src (Index_Type'First .. Index_Type'Pred (Before));

         Dst (Index .. New_Last) :=
           Src (Before .. Container.Last);
      exception
         when others =>
            Free (Dst);
            raise;
      end;

      declare
         X : Elements_Access := Container.Elements;
      begin
         Container.Elements := Dst;
         Container.Last := New_Last;

         Free (X);
      end;

   end Insert_Space;


--     procedure Insert_Space (Container : in out Vector;
--                             Before    : in     Index_Type'Base;
--                             Count     : in     Count_Type := 1) is
--     begin
--
--        if Count = 0 then
--           return;
--        end if;
--
--        declare
--           subtype T is Index_Type'Base range
--             Index_Type'First .. Index_Type'Succ (Container.Last);
--        begin
--           Insert_Space_Internal (Container,
--                                  T'(Before),
--                                  Positive_Int (Count));
--        end;
--
--     end Insert_Space;


   procedure Set_Length (Container : in out Vector;
                         Length    : in     Count_Type) is
   begin

      if Length = 0 then
         Clear (Container);
         return;
      end if;

      declare
         Last_As_Int : constant Int'Base :=
           Int (Index_Type'First) + Int (Length) - 1;

         Last : constant Index_Type :=
           Index_Type (Last_As_Int);
      begin
         if Length > Capacity (Container) then
            Set_Capacity (Container, Capacity => Length);
         end if;

         Container.Last := Last;
      end;

   end Set_Length;




   procedure Insert (Container : in out Vector;
                     Before    : in     Index_Type'Base;
                     New_Item  : in     Vector) is

      N : constant Count_Type := Length (New_Item);

   begin

      if N = 0 then
         return;
      end if;

      Insert_Space (Container, Before, Count => N);

      declare
         Dst_Last_As_Int : constant Int'Base :=
           Int'Base (Before) + Int'Base (N) - 1;

         Dst_Last : constant Index_Type :=
           Index_Type (Dst_Last_As_Int);
      begin

         if Container'Address = New_Item'Address then

            declare
               subtype Src_Index_Subtype is Index_Type'Base range
                 Index_Type'First .. Index_Type'Pred (Before);

               Src : Elements_Type renames
                 Container.Elements (Src_Index_Subtype);

               Index_As_Int : constant Int'Base :=
                 Int (Before) + Src'Length - 1;

               Index : constant Index_Type'Base :=
                 Index_Type'Base (Index_As_Int);

               Dst : Elements_Type renames
                 Container.Elements (Before .. Index);
            begin
               Dst := Src;
            end;

            declare
               subtype Src_Index_Subtype is Index_Type'Base range
                 Index_Type'Succ (Dst_Last) .. Container.Last;

               Src : Elements_Type renames
                 Container.Elements (Src_Index_Subtype);

               Index_As_Int : constant Int'Base :=
                 Dst_Last_As_Int - Src'Length + 1;

               Index : constant Index_Type'Base :=
                 Index_Type'Base (Index_As_Int);

               Dst : Elements_Type renames
                 Container.Elements (Index .. Dst_Last);
            begin
               Dst := Src;
            end;

         else

            Container.Elements (Before .. Dst_Last) :=
              New_Item.Elements (Index_Type'First .. New_Item.Last);

         end if;

      end;

   end Insert;


   procedure Insert (Container : in out Vector;
                     Before    : in     Cursor;
                     New_Item  : in     Vector) is

      Index : Index_Type'Base;

   begin

      if Is_Empty (New_Item) then  -- TODO: not in 2004/04/29 yet
         return;
      end if;

      if Before.Container = null
        or else Before.Index not in Index_Type'First .. Container.Last
      then
         Index := Index_Type'Succ (Container.Last);
      else
         Index := Before.Index;
      end if;

      Insert (Container, Index, New_Item);

   end Insert;


   procedure Insert (Container : in out Vector;
                     Before    : in     Cursor;
                     New_Item  : in     Vector;
                     Position  :    out Cursor) is

      Index : Index_Type'Base;

   begin

      if Is_Empty (New_Item) then  --  TODO: not in 2004/04/29 yet

         if Before.Container = null
           or else Before.Index not in Index_Type'First .. Container.Last
         then
            Position := No_Element;
         else
            Position := (Container'Unchecked_Access, Before.Index);
         end if;

         return;

      end if;

      if Before.Container = null
        or else Before.Index not in Index_Type'First .. Container.Last
      then
         Index := Index_Type'Succ (Container.Last);
      else
         Index := Before.Index;
      end if;

      Insert (Container, Index, New_Item);

      Position := Cursor'(Container'Unchecked_Access, Index);

   end Insert;


   procedure Insert (Container : in out Vector;
                     Before    : in     Cursor;
                     New_Item  : in     Element_Type;
                     Count     : in     Count_Type := 1) is

      Index : Index_Type'Base;

   begin

      if Count = 0 then  --  TODO: not in 2004/04/29 spec yet
         return;
      end if;

      if Before.Container = null
        or else Before.Index not in Index_Type'First .. Container.Last
      then
         Index := Index_Type'Succ (Container.Last);
      else
         Index := Before.Index;
      end if;

      Insert (Container, Index, New_Item, Count);

   end Insert;


   procedure Insert (Container : in out Vector;
                     Before    : in     Cursor;
                     New_Item  : in     Element_Type;
                     Position  :    out Cursor;
                     Count     : in     Count_Type := 1) is

      Index : Index_Type'Base;

   begin

      if Count = 0 then  --  TODO: not in 2004/04/29 spec yet

         if Before.Container = null
           or else Before.Index not in Index_Type'First .. Container.Last
         then
            Position := No_Element;
         else
            Position := (Container'Unchecked_Access, Before.Index);
         end if;

         return;

      end if;

      if Before.Container = null
        or else Before.Index not in Index_Type'First .. Container.Last
      then
         Index := Index_Type'Succ (Container.Last);
      else
         Index := Before.Index;
      end if;

      Insert (Container, Index, New_Item, Count);

      Position := Cursor'(Container'Unchecked_Access, Index);

   end Insert;


   procedure Insert_Space (Container : in out Vector;
                           Before    : in     Cursor;
                           Position  :    out Cursor;
                           Count     : in     Count_Type := 1) is

      Index : Index_Type'Base;

   begin

      if Count = 0 then  --  TODO: not in 2004/04/29 spec

         if Before.Container = null
           or else Before.Index not in Index_Type'First .. Container.Last
         then
            Position := No_Element;
         else
            Position := (Container'Unchecked_Access, Before.Index);
         end if;

         return;

      end if;

      if Before.Container = null
        or else Before.Index not in Index_Type'First .. Container.Last
      then
         Index := Index_Type'Succ (Container.Last);
      else
         Index := Before.Index;
      end if;

      Insert_Space (Container, Index, Count);

      Position := Cursor'(Container'Unchecked_Access, Index);

   end Insert_Space;


   procedure Delete_First (Container : in out Vector;
                           Count     : in     Count_Type := 1) is
   begin

      if Count = 0 then
         return;
      end if;

      if Count >= Length (Container) then
         Clear (Container);
         return;
      end if;

      Delete (Container, Index_Type'First, Count);

   end Delete_First;



   procedure Delete_Last (Container : in out Vector;
                          Count     : in     Count_Type := 1) is

      Index : Int'Base;

   begin

      if Count = 0 then
         return;
      end if;

      if Count >= Length (Container) then
         Clear (Container);
         return;
      end if;

      Index := Int'Base (Container.Last) - Int'Base (Count) + 1;

      Delete (Container, Index_Type'Base (Index), Count);

   end Delete_Last;


   procedure Delete (Container : in out Vector;
                     Index     : in     Index_Type'Base;
                     Count     : in     Count_Type := 1) is
   begin

      if Count = 0 then
         return;
      end if;

      declare
         subtype Index_Subtype is Index_Type'Base range
           Index_Type'First .. Container.Last;

         I : constant Index_Subtype := Index;
         I_As_Int : constant Int := Int (I);

         Old_Last_As_Int : constant Int := Index_Type'Pos (Container.Last);

         Count1 : constant Int'Base := Count_Type'Pos (Count);
         Count2 : constant Int'Base := Old_Last_As_Int - I_As_Int + 1;

         N : constant Int'Base := Int'Min (Count1, Count2);

         J_As_Int : constant Int'Base := I_As_Int + N;
         J : constant Index_Type'Base := Index_Type'Base (J_As_Int);

         E : Elements_Type renames Container.Elements.all;

         New_Last_As_Int : constant Int'Base := Old_Last_As_Int - N;

         New_Last : constant Last_Subtype :=
           Last_Subtype (New_Last_As_Int);
      begin
         E (I .. New_Last) := E (J .. Container.Last);
         Container.Last := New_Last;
      end;

   end Delete;


--     procedure Delete (Container : in out Vector;
--                       Index     : in     Index_Type'Base;
--                       Count     : in     Count_Type := 1) is
--     begin
--
--        if Count = 0 then
--           return;
--        end if;
--
--        declare
--           subtype T is Index_Type'Base range
--             Index_Type'First .. Container.Last;
--        begin
--           Delete_Internal (Container, T'(Index), Positive_Int (Count));
--        end;
--
--     end Delete;


   procedure Delete (Container : in out Vector;
                     Position  : in out Cursor;
                     Count     : in     Count_Type := 1) is

   begin

      if Count = 0 then

         if Position.Container = null
           or else Position.Index not in Index_Type'First .. Container.Last
         then
            Position := No_Element;
         else
            Position := (Container'Unchecked_Access, Position.Index);
         end if;

         return;

      end if;

      if Position.Container = null
        or else Position.Index not in Index_Type'First .. Container.Last
      then
         raise Constraint_Error;
      end if;

      Delete (Container, Position.Index, Count);

      if Position.Index in Index_Type'First .. Container.Last then
         Position := (Container'Unchecked_Access, Position.Index);
      else
         Position := No_Element;
      end if;

   end Delete;



   function Capacity (Container : Vector) return Count_Type is
   begin
      if Container.Elements = null then
         return 0;
      end if;

      return Container.Elements'Length;
   end Capacity;


   procedure Set_Capacity (Container : in out Vector;
                           Capacity  : in     Count_Type) is

      N : constant Count_Type := Length (Container);

   begin

      if Capacity = 0 then

         if N = 0 then

            declare
               X : Elements_Access := Container.Elements;
            begin
               Container.Elements := null;
               Free (X);
            end;

         elsif N < Container.Elements'Length then

            declare
               subtype Array_Index_Subtype is Index_Type'Base range
                 Index_Type'First .. Container.Last;

               Src : Elements_Type renames
                 Container.Elements (Array_Index_Subtype);

               subtype Array_Subtype is
                 Elements_Type (Array_Index_Subtype);

               X : Elements_Access := Container.Elements;
            begin
               Container.Elements := new Array_Subtype'(Src);
               Free (X);
            end;

         end if;

         return;

      end if;

      if Container.Elements = null then

         declare
            Last_As_Int : constant Int'Base :=
              Int (Index_Type'First) + Int (Capacity) - 1;

            Last : constant Index_Type :=
              Index_Type (Last_As_Int);

            subtype Array_Subtype is
              Elements_Type (Index_Type'First .. Last);
         begin
            Container.Elements := new Array_Subtype;
         end;

         return;

      end if;

      if Capacity <= N then

         if N < Container.Elements'Length then

            declare
               subtype Array_Index_Subtype is Index_Type'Base range
                 Index_Type'First .. Container.Last;

               Src : Elements_Type renames
                 Container.Elements (Array_Index_Subtype);

               subtype Array_Subtype is
                 Elements_Type (Array_Index_Subtype);

               X : Elements_Access := Container.Elements;
            begin
               Container.Elements := new Array_Subtype'(Src);
               Free (X);
            end;

         end if;

         return;

      end if;

      if Capacity = Container.Elements'Length then
         return;
      end if;

      declare
         Last_As_Int : constant Int'Base :=
           Int (Index_Type'First) + Int (Capacity) - 1;

         Last : constant Index_Type :=
           Index_Type (Last_As_Int);

         subtype Array_Subtype is
           Elements_Type (Index_Type'First .. Last);

         E : Elements_Access := new Array_Subtype;
      begin

         declare
            Src : Elements_Type renames
              Container.Elements (Index_Type'First .. Container.Last);

            Tgt : Elements_Type renames
              E (Index_Type'First .. Container.Last);
         begin
            Tgt := Src;
         exception
            when others =>
               Free (E);
               raise;
         end;

         declare
            X : Elements_Access := Container.Elements;
         begin
            Container.Elements := E;
            Free (X);
         end;

      end;

   end Set_Capacity;


   function First_Index (Container : Vector) return Index_Type is
      pragma Warnings (Off, Container);
   begin
      return Index_Type'First;
   end First_Index;


   function First (Container : Vector) return Cursor is
   begin
      if Is_Empty (Container) then
         return No_Element;
      end if;

      return (Container'Unchecked_Access, Index_Type'First);
   end First;


   function First_Element (Container : Vector) return Element_Type is
   begin
      return Element (Container, Index_Type'First);
   end First_Element;


   function Last_Index (Container : Vector) return Index_Type'Base is
   begin
      return Container.Last;
   end Last_Index;


   function Last (Container : Vector) return Cursor is
   begin
      if Is_Empty (Container) then
         return No_Element;
      end if;

      return (Container'Unchecked_Access, Container.Last);
   end Last;


   function Last_Element (Container : Vector) return Element_Type is
   begin
      return Element (Container, Container.Last);
   end Last_Element;


   function Element (Container : Vector;
                     Index     : Index_Type'Base)
     return Element_Type is

      subtype T is Index_Type'Base range
        Index_Type'First .. Container.Last;
   begin
      return Container.Elements (T'(Index));
   end Element;


   function Element (Position : Cursor) return Element_Type is
   begin
      return Element (Position.Container.all, Position.Index);
   end Element;



   procedure Generic_Update_Element_By_Index
     (Container : in Vector;
      Index     : in Index_Type'Base) is

      subtype T is Index_Type'Base range
        Index_Type'First .. Container.Last;
   begin
      Process (Container.Elements (T'(Index)));
   end Generic_Update_Element_By_Index;


   procedure Generic_Update_Element (Position : in Cursor) is

      subtype T is Index_Type'Base range
        Index_Type'First .. Position.Container.Last;
   begin
      Process (Position.Container.Elements (T'(Position.Index)));
   end Generic_Update_Element;



   procedure Replace_Element (Container : in Vector;
                              Index     : in Index_Type'Base;
                              By        : in Element_Type) is

      subtype T is Index_Type'Base range
        Index_Type'First .. Container.Last;
   begin
      Container.Elements (T'(Index)) := By;
   end Replace_Element;


   procedure Replace_Element (Position : in Cursor;
                              By       : in Element_Type) is

      subtype T is Index_Type'Base range
        Index_Type'First .. Position.Container.Last;
   begin
      Position.Container.Elements (T'(Position.Index)) := By;
   end Replace_Element;



   procedure Swap
     (Container : in Vector;
      I, J      : in Index_Type'Base) is

      subtype T is Index_Type'Base range
        Index_Type'First .. Container.Last;

      EI : constant Element_Type := Container.Elements (T'(I));
   begin
      Container.Elements (T'(I)) := Container.Elements (T'(J));
      Container.Elements (T'(J)) := EI;
   end Swap;


   procedure Swap (Container : in Vector;
                   I, J      : in Cursor) is
   begin
      Swap (Container, I => I.Index, J => J.Index);
   end Swap;


   procedure Generic_Sort (Container : in Vector) is

      procedure Sort is
         new Generic_Array_Sort
          (Index_Type   => Index_Type,
           Element_Type => Element_Type,
           Array_Type   => Elements_Type,
           "<"          => "<");

   begin

      if Container.Elements = null then
         return;
      end if;

      Sort (Container.Elements (Index_Type'First .. Container.Last));

   end Generic_Sort;


   function Find
     (Container : Vector;
      Item      : Element_Type;
      Index     : Index_Type'Base := Index_Type'First)
     return Index_Type'Base is

      subtype T is Index_Type'Base
        range Index_Type'First .. Index_Type'Base'Last;

   begin

      for I in T'(Index) .. Container.Last loop
         if Container.Elements (I) = Item then
            return I;
         end if;
      end loop;

      return Index_Type'Succ (Container.Last);  --  ?

   end Find;


   function Find (Container : Vector;
                  Item      : Element_Type;
                  Position  : Cursor := No_Element) return Cursor is

      Index : Index_Type'Base;

   begin

      if Is_Empty (Container) then
         return No_Element;
      end if;

      if Position.Container = null
        or else Position.Index > Container.Last
                  --  TODO: error, or counts as No_Element?
      then
         Index := Index_Type'First;
      else
         Index := Position.Index;
      end if;

      for I in Index .. Container.Last loop
         if Container.Elements (I) = Item then
            return (Container'Unchecked_Access, I);
         end if;
      end loop;

      return No_Element;

   end Find;



   function Reverse_Find (Container : Vector;
                          Item      : Element_Type;
                          Index     : Index_Type'Base := Index_Type'Last)
      return Index_Type'Base is

      Last : Index_Type'Base;

   begin

      if Index > Container.Last then
         Last := Container.Last;
      else
         Last := Index;
      end if;

      for I in reverse Index_Type'First .. Last loop
         if Container.Elements (I) = Item then
            return I;
         end if;
      end loop;

      return Index_Type'Succ (Container.Last);  --  ?

   end Reverse_Find;


   function Reverse_Find (Container : Vector;
                          Item      : Element_Type;
                          Position  : Cursor := No_Element) return Cursor is

      Last : Index_Type'Base;

   begin

      if Is_Empty (Container) then  --  TODO: verify with Randy and Tucker
         return No_Element;
      end if;

      if Position.Container = null
        or else Position.Index > Container.Last  --  TODO: correct semantics?
      then
         Last := Container.Last;
      else
         Last := Position.Index;
      end if;

      for I in reverse Index_Type'First .. Last loop
         if Container.Elements (I) = Item then
            return (Container'Unchecked_Access, I);
         end if;
      end loop;

      return No_Element;

   end Reverse_Find;


   function Is_In (Item      : Element_Type;
                   Container : Vector)
      return Boolean is
   begin
      return Find (Container, Item) /= Index_Type'Succ (Container.Last);
   end Is_In;


   function Next (Position : Cursor) return Cursor is
   begin

      if Position.Container = null then
         return No_Element;
      end if;

      if Position.Index < Position.Container.Last then
         return (Position.Container, Index_Type'Succ (Position.Index));
      end if;

      return No_Element;

   end Next;


   function Previous (Position : Cursor) return Cursor is
   begin

      if Position.Container = null then
         return No_Element;
      end if;

      if Position.Index > Index_Type'First then
         return (Position.Container, Index_Type'Pred (Position.Index));
      end if;

      return No_Element;

   end Previous;


   procedure Next (Position : in out Cursor) is
   begin

      if Position.Container = null then
         return;
      end if;

      if Position.Index < Position.Container.Last then
         Position.Index := Index_Type'Succ (Position.Index);
      else
         Position := No_Element;
      end if;

   end Next;


   procedure Previous (Position : in out Cursor) is
   begin

      if Position.Container = null then
         return;
      end if;

      if Position.Index > Index_Type'First then
         Position.Index := Index_Type'Pred (Position.Index);
      else
         Position := No_Element;
      end if;

   end Previous;


   function Has_Element (Position : Cursor) return Boolean is
   begin

      if Position.Container = null then
         return False;
      end if;

      return Position.Index <= Position.Container.Last;

   end Has_Element;


   procedure Generic_Iteration (Container : in Vector) is
   begin

      for I in Index_Type'First .. Container.Last loop
         Process (Cursor'(Container'Unchecked_Access, I));
      end loop;

   end Generic_Iteration;


   procedure Generic_Reverse_Iteration (Container : in Vector) is
   begin

      for I in reverse Index_Type'First .. Container.Last loop
         Process (Cursor'(Container'Unchecked_Access, I));
      end loop;

   end Generic_Reverse_Iteration;


end AI302.Containers.Vectors;

