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

with Charles.Algorithms.Generic_Set_Intersection;
with Charles.Algorithms.Generic_Set_Difference;
with Charles.Algorithms.Generic_Set_Symmetric_Difference;
with Charles.Algorithms.Generic_Includes;
with Ada.Unchecked_Deallocation;
with System;  use type System.Address;

package body AI302.Containers.Indefinite_Ordered_Sets is

   type Element_Access is access Element_Type;

   type Node_Type is
      record   --make this a limited record for Ada0Y
         Parent  : Node_Access;
         Left    : Node_Access;
         Right   : Node_Access;
         Color   : Color_Type;
         Element : Element_Access;
      end record;

   function "=" (L, R : Node_Type) return Boolean is abstract;
   pragma Warnings (Off, "=");


   function Parent (Node : Node_Access)
      return Node_Access is
   begin
      return Node.Parent;
   end;

   function Left (Node : Node_Access)
      return Node_Access is
   begin
      return Node.Left;
   end;

   function Right (Node : Node_Access)
      return Node_Access is
   begin
      return Node.Right;
   end;

   function Color (Node : Node_Access)
      return Color_Type is
   begin
      return Node.Color;
   end;

   procedure Set_Parent
     (Node   : Node_Access;
      Parent : Node_Access) is
   begin
      Node.Parent := Parent;
   end;

   procedure Set_Left
     (Node : Node_Access;
      Left : Node_Access) is
   begin
      Node.Left := Left;
   end;

   procedure Set_Right
     (Node  : Node_Access;
      Right : Node_Access) is
   begin
      Node.Right := Right;
   end;

   procedure Set_Color
     (Node  : Node_Access;
      Color : Color_Type) is
   begin
      Node.Color := Color;
   end;


   function New_Back return Node_Access is

      Back : constant Node_Access := new Node_Type;
   begin
      Back.Color := White;

      Back.Left := Back;
      Back.Right := Back;

      return Back;
   end;


   procedure Free is
      new Ada.Unchecked_Deallocation (Element_Type, Element_Access);

   procedure Free (X : in out Node_Access) is

      procedure Deallocate is
         new Ada.Unchecked_Deallocation (Node_Type, Node_Access);
   begin
      Free (X.Element);
      Deallocate (X);
   end;


   procedure Delete_Tree (X : in out Node_Access) is

      Y : Node_Access;

   begin

      while X /= null loop

         Y := X.Right;

         Delete_Tree (Y);

         Y := X.Left;

         Free (X);

         X := Y;

      end loop;

   end Delete_Tree;


   function Copy_Node (Source : Node_Access) return Node_Access is
      pragma Inline (Copy_Node);

      --NOTE:
      --We could make the decision to allow null elements to be
      --copied.  Here we assume that the source is non-null.
      --END NOTE.

      Element : Element_Access := new Element_Type'(Source.Element.all);
   begin
      return new Node_Type'(Parent  => null,
                            Left    => null,
                            Right   => null,
                            Color   => Source.Color,
                            Element => Element);
   exception
      when others =>
         Free (Element);
         raise;
   end;


   function Copy_Tree (Source_Root : Node_Access) return Node_Access is

      Target_Root : Node_Access := Copy_Node (Source_Root);

      P, X : Node_Access;

   begin

      if Source_Root.Right /= null then

         Target_Root.Right := Copy_Tree (Source_Root.Right);
         Target_Root.Right.Parent := Target_Root; --per Georg Bauhaus

      end if;

      P := Target_Root;
      X := Source_Root.Left;

      while X /= null loop

         declare
            Y : Node_Access := Copy_Node (X);
         begin

            P.Left := Y;
            Y.Parent := P;

            if X.Right /= null then

               Y.Right := Copy_Tree (X.Right);
               Y.Right.Parent := Y; --per Georg Bauhaus

            end if;

            P := Y;
            X := X.Left;

         end;

      end loop;

      return Target_Root;

   exception
      when others =>

         Delete_Tree (Target_Root);
         raise;

   end Copy_Tree;


   procedure Adjust (Container : in out Set) is

      Tree : Tree_Type renames Container.Tree;

      Length : constant Size_Type := Tree.Length;

      X : Node_Access := Root (Tree);
      Y : Node_Access;

   begin -- Adjust

      Tree.Back := null;
      Tree.Length := 0;

      Tree.Back := new Node_Type;
      -- Allocation failure here is the problem case,
      -- since it means we aren't satisfying our
      -- representation invariant.

      Tree.Back.Color := White;

      Initialize (Tree);

      if X = null then
         return;
      end if;

      Y := Copy_Tree (X);
      -- If allocation fails here, it's less of a problem
      -- since we already have a Back node, and hence our
      -- representation invariant is satisfied.

      Y.Parent := Tree.Back;  -- per Georg Bauhaus

      Set_Root (Tree, Y);
      Set_First (Tree, Min (Y));
      Set_Last (Tree, Max (Y));

      Tree.Length := Length;

   end Adjust;


   procedure Finalize (Container : in out Set) is

      Tree : Tree_Type renames Container.Tree;

      Back : Node_Access := Tree.Back;
      Root : Node_Access;

   begin

      if Back = null then
         return;
      end if;

      Root := Tree_Types.Root (Tree);

      Tree.Back := null;
      Tree.Length := 0;

      Delete_Tree (Root);
      Free (Back);

   end Finalize;



   function Is_Equal_Node_Node
     (L, R : Node_Access) return Boolean is

      pragma Inline (Is_Equal_Node_Node);
   begin
      --NOTE:
      --We could liberalize equality, to allow null-elements
      --to be compared.  Here we simply assume elements
      --are non-null.
      --END NOTE.

      return L.Element.all = R.Element.all;
   end;

   function Is_Equal is
      new Tree_Types.Generic_Equal (Is_Equal_Node_Node);

   function "=" (Left, Right : Set) return Boolean is
   begin
      if Left'Address = Right'Address then
         return True;
      end if;

      return Is_Equal (Left.Tree, Right.Tree);
   end;


   function Is_Less_Node_Node
     (Left, Right : Node_Access) return Boolean is

      pragma Inline (Is_Less_Node_Node);
   begin
      return Left.Element.all < Right.Element.all;
   end;


   function Is_Greater_Element_Node
     (Left  : Element_Type;
      Right : Node_Access) return Boolean is

      pragma Inline (Is_Greater_Element_Node);

      -- e > node same as node < e
   begin
      return Right.Element.all < Left;
   end;


   function Is_Less_Element_Node
     (Left  : Element_Type;
      Right : Node_Access) return Boolean is

      pragma Inline (Is_Less_Element_Node);
   begin
      return Left < Right.Element.all;
   end;



   function Empty_Set return Set is
   begin
      return (Controlled with Tree => (New_Back, 0));
   end;


   function Length (Container : Set) return Size_Type is
   begin
      return Container.Tree.Length;
   end;


   function Is_Empty (Container : Set) return Boolean is
   begin
      return Length (Container) = 0;
   end;


   procedure Clear (Container : in out Set) is

      X : Node_Access := Root (Container.Tree);

   begin

      Initialize (Container.Tree);

      Delete_Tree (X);

   end Clear;


   procedure Move (Target : in out Set;
                   Source : in out Set) is
   begin
      if Target'Address = Source'Address then
         return;
      end if;

      Move (Target => Target.Tree, Source => Source.Tree);
   end;


   package Element_Keys is
      new Tree_Types.Generic_Keys
        (Element_Type,
         Is_Less_Element_Node,
         Is_Greater_Element_Node);


   procedure Insert
     (Container : in out Set;
      New_Item  : in     Element_Type;
      Position  :    out Cursor;
      Success   :    out Boolean) is

      function New_Node return Node_Access is
         pragma Inline (New_Node);

         Element : Element_Access := new Element_Type'(New_Item);
      begin
         return new Node_Type'(Parent  => null,
                               Left    => null,
                               Right   => null,
                               Color   => Red,
                               Element => Element);
      exception
         when others =>
            Free (Element);
            raise;
      end;

      procedure Insert_Post is
         new Element_Keys.Generic_Insert_Post (New_Node);

      procedure Insert_Sans_Hint is
         new Element_Keys.Generic_Conditional_Insert (Insert_Post);

   begin -- Insert

      Insert_Sans_Hint
        (Container.Tree,
         New_Item,
         Node_Access (Position),
         Success);

      --pragma Debug (Check_Invariant (Container.Tree));

   end;


   procedure Delete (Container : in out Set;
                     Position  : in out Cursor) is

      X : Node_Access := Node_Access (Position);

   begin

      if Position = null then
         return;
      end if;

      pragma Assert (Position.Color /= White);

      Delete (Container.Tree, X);
      Free (X);

      Position := null;

      --pragma Debug (Check_Invariant (Container.Tree));

   end Delete;


   function Find (Container : Set;
                  Item      : Element_Type) return Cursor is

      Node : constant Node_Access :=
        Element_Keys.Find (Container.Tree, Item);
   begin
      return Cursor (Node);
   end;


   function Is_In (Item      : Element_Type;
                   Container : Set) return Boolean is
   begin
      return Find (Container, Item) /= null;
   end;


   function First (Container : Set) return Cursor is
      Node : constant Node_Access := First (Container.Tree);
   begin
      if Node = Container.Tree.Back then
         pragma Assert (Container.Tree.Length = 0);
         return null;
      end if;

      return Cursor (Node);
   end;


   function First_Element (Container : Set) return Element_Type is
      C : constant Cursor := First (Container);
   begin
      return C.Element.all;
   end;


   function Last (Container : Set) return Cursor is
      Node : constant Node_Access := Last (Container.Tree);
   begin
      if Node = Container.Tree.Back then
         pragma Assert (Container.Tree.Length = 0);
         return null;
      end if;

      return Cursor (Node);
   end;


   function Last_Element (Container : Set) return Element_Type is
      C : constant Cursor := Last (Container);
   begin
      return C.Element.all;
   end;


   function Next (Position : Cursor) return Cursor is

      Node : Node_Access;

   begin

      if Position = null then
         return null;
      end if;

      Node := Succ (Node_Access (Position));

      if Node.Color = White then  -- back
         return null;
      end if;

      return Cursor (Node);

   end Next;


   function Previous (Position : Cursor) return Cursor is

      Node : Node_Access;

   begin

      if Position = null then
         return null;
      end if;

      Node := Pred (Node_Access (Position));

      if Node.Color = White then  -- back
         return null;
      end if;

      return Cursor (Node);

   end Previous;


   procedure Next (Position : in out Cursor) is
   begin
      Position := Next (Position);
   end;


   procedure Previous (Position : in out Cursor) is
   begin
      Position := Previous (Position);
   end;


   function Element (Position : Cursor) return Element_Type is
   begin
      return Position.Element.all;  -- note that Back.Element = null
   end;


   procedure Generic_Update (Position : in Cursor) is
   begin
      pragma Assert (Position.Color /= White);
      Process (Position.Element.all);
   end;


   procedure Delete (Container : in out Set;
                     Item      : in     Element_Type) is

      C : Cursor := Find (Container, Item);
   begin
      Delete (Container, C);
   end;


   procedure Delete_First (Container : in out Set) is
      C : Cursor := First (Container);
   begin
      Delete (Container, C);
   end;


   procedure Delete_Last (Container : in out Set) is
      C : Cursor := Last (Container);
   begin
      Delete (Container, C);
   end;


--     function Lower_Bound (Container  : Set;
--                           Item : Element_Type) return Cursor is
--     begin
--        return (Node => Element_Keys.Lower_Bound (Container.Tree, Item));
--     end;


--     function Upper_Bound (Container  : Set;
--                           Item : Element_Type) return Cursor is
--     begin
--        return (Node => Element_Keys.Upper_Bound (Container.Tree, Item));
--     end;


   function "<" (Left, Right : Cursor) return Boolean is
   begin
      return Left.Element.all < Right.Element.all;
   end;


   function ">" (Left, Right : Cursor) return Boolean is
      -- L > R same as R < L
   begin
      return Right.Element.all < Left.Element.all;
   end;


   function "<" (Left : Cursor; Right : Element_Type)
      return Boolean is
   begin
      return Left.Element.all < Right;
   end;

   function ">" (Left : Cursor; Right : Element_Type)
      return Boolean is
   begin
      return Right < Left.Element.all;
   end;

   function "<" (Left : Element_Type; Right : Cursor)
      return Boolean is
   begin
      return Left < Right.Element.all;
   end;

   function ">" (Left : Element_Type; Right : Cursor)
      return Boolean is
   begin
      return Right.Element.all < Left;
   end;


   procedure Generic_Iteration (Container : in Set) is

      procedure Process (Node : Node_Access) is
         pragma Inline (Process);
      begin
         Process (Cursor (Node));
      end;

      procedure Iterate is
         new Tree_Types.Generic_Iteration (Process);
   begin
      Iterate (Container.Tree);
   end;


   procedure Generic_Reverse_Iteration (Container : in Set) is

      procedure Process (Node : Node_Access) is
         pragma Inline (Process);
      begin
         Process (Cursor (Node));
      end;

      procedure Iterate is
         new Tree_Types.Generic_Reverse_Iteration (Process);
   begin
      Iterate (Container.Tree);
   end;


   package body Generic_Keys is

      function Is_Less_Key_Node
        (Left  : Key_Type;
         Right : Node_Access) return Boolean is

         pragma Inline (Is_Less_Key_Node);
      begin
         return Left < Right.Element.all;
      end;

      function Is_Greater_Key_Node
        (Left  : Key_Type;
         Right : Node_Access) return Boolean is

         pragma Inline (Is_Greater_Key_Node);
      begin
         return Left > Right.Element.all;
      end;


      package Key_Keys is
         new Tree_Types.Generic_Keys
          (Key_Type,
           Is_Less_Key_Node,
           Is_Greater_Key_Node);


      function Find (Container : Set;
                     Key       : Key_Type) return Cursor is

         Node : constant Node_Access :=
           Key_Keys.Find (Container.Tree, Key);
      begin
         return Cursor (Node);
      end;


      function Is_In (Key       : Key_Type;
                      Container : Set)
         return Boolean is
      begin
         return Find (Container, Key) /= null;
      end;


      function Element (Container : Set;
                        Key       : Key_Type)
        return Element_Type is

         C : constant Cursor := Find (Container, Key);
      begin
         return C.Element.all;
      end;


--        function Lower_Bound (Container : Set;
--                              Key : Key_Type)
--          return Cursor is
--        begin
--           return (Node => Key_Keys.Lower_Bound (Container.Tree, Key));
--        end;


--        function Upper_Bound (Container : Set;
--                              Key : Key_Type)
--          return Cursor is
--        begin
--           return (Node => Key_Keys.Upper_Bound (Container.Tree, Key));
--        end;


      procedure Delete (Container : in out Set;
                        Key       : in     Key_Type) is

         C : Cursor := Find (Container, Key);
      begin
         Delete (Container, C);
      end;


      function "<" (Left : Key_Type; Right : Cursor)
         return Boolean is
      begin
         return Left < Right.Element.all;
      end;

      function ">" (Left : Key_Type; Right : Cursor)
         return Boolean is
      begin
         return Left > Right.Element.all;
      end;

      function "<" (Left : Cursor; Right : Key_Type)
        return Boolean is
      begin
         return Right > Left.Element.all;
      end;

      function ">" (Left : Cursor; Right : Key_Type)
        return Boolean is
      begin
         return Right < Left.Element.all;
      end;


      procedure Generic_Insertion
        (Container : in out Set;
         Key       : in     Key_Type;
         Position  :    out Cursor;
         Success   :    out Boolean) is

         function New_Node return Node_Access is
            pragma Inline (New_Node);

            Node : Node_Access := new Node_Type;
         begin
            --Set_Key (Node.Element, Key);
            Node.Element := new Element_Type'(New_Element (Key));

            Node.Color := Red;

            return Node;
         exception
            when others =>
               Free (Node);
               raise;
         end;

         procedure Insert_Post is
            new Key_Keys.Generic_Insert_Post (New_Node);

         procedure Insert_Sans_Hint is
            new Key_Keys.Generic_Conditional_Insert (Insert_Post);

      begin -- Generic_Insertion

         Insert_Sans_Hint
           (Container.Tree,
            Key,
            Node_Access (Position),
            Success);

      end Generic_Insertion;

   end Generic_Keys;


   procedure Write
     (Stream    : access Ada.Streams.Root_Stream_Type'Class;
      Container : in     Set) is

      procedure Process (Node : Node_Access) is
      begin
         Element_Type'Output (Stream, Node.Element.all);
      end;

      procedure Iterate is
         new Tree_Types.Generic_Iteration (Process);

   begin -- Write

      Size_Type'Base'Write (Stream, Container.Tree.Length);

      Iterate (Container.Tree);

   end Write;


   procedure Read
     (Stream    : access Ada.Streams.Root_Stream_Type'Class;
      Container :    out Set) is

      function New_Node return Node_Access is

         Node : Node_Access := new Node_Type;
      begin
         Node.Color := Red;
         Node.Element := new Element_Type'(Element_Type'Input (Stream));

         return Node;
      exception
         when others =>
            Free (Node);
            raise;
      end;

      procedure Read is
         new Tree_Types.Generic_Read (New_Node);

      N : Size_Type'Base;

   begin

      Clear (Container);

      Size_Type'Base'Read (Stream, N);
      pragma Assert (N >= 0);

      Read (Container.Tree, N);

   end Read;


   function Has_Element (Position : Cursor) return Boolean is
   begin
      if Position = null then
         return False;
      end if;

      pragma Assert (Position.Color /= White);

      return True;
   end;


   procedure Insert_With_Hint
     (Dst_Tree : in out Tree_Type;
      Dst_Hint : in     Node_Access;
      Src_Node : in     Node_Access;
      Dst_Node :    out Node_Access) is

      function New_Node return Node_Access is
         pragma Inline (New_Node);

         Element : Element_Access :=
           new Element_Type'(Src_Node.Element.all);

         Node : Node_Access;

      begin

         begin
            Node := new Node_Type;
         exception
            when others =>
               Free (Element);
               raise;
         end;

         Node.Color := Red;
         Node.Element := Element;

         return Node;

      end New_Node;

      procedure Insert_Post is
         new Element_Keys.Generic_Insert_Post (New_Node);

      procedure Insert_Sans_Hint is
         new Element_Keys.Generic_Conditional_Insert (Insert_Post);

      procedure Insert_With_Hint is
         new Element_Keys.Generic_Conditional_Insert_With_Hint
            (Insert_Post,
             Insert_Sans_Hint);

      Success  : Boolean;

   begin -- Insert_With_Hint

      Insert_With_Hint
        (Dst_Tree,
         Dst_Hint,
         Src_Node.Element.all,
         Dst_Node,
         Success);

      --pragma Debug (Check_Invariant (Set.Tree));

   end Insert_With_Hint;


   procedure Union
     (Target : in out Set;
      Source : in     Set) is

      Hint : Node_Access;

      procedure Process (Node : Node_Access) is
         pragma Inline (Process);
      begin
         Insert_With_Hint
           (Dst_Tree => Target.Tree,
            Dst_Hint => Hint,
            Src_Node => Node,
            Dst_Node => Hint);
      end;

      procedure Iterate is
         new Tree_Types.Generic_Iteration (Process);

   begin -- Union

      if Target'Address = Source'Address then
         return;
      end if;

      Iterate (Source.Tree);

   end Union;



   function Union (Left, Right : Set) return Set is
   begin

      if Left'Address = Right'Address then
         return Left;
      end if;

      declare

         Result : Set := Left;  --TODO: fix this here and elsewhere
         Hint   : Node_Access;

         procedure Process (Node : Node_Access) is
            pragma Inline (Process);
         begin
            Insert_With_Hint
              (Dst_Tree => Result.Tree,
               Dst_Hint => Hint,
               Src_Node => Node,
               Dst_Node => Hint);
         end;

         procedure Iterate is
            new Tree_Types.Generic_Iteration (Process);

      begin

         Iterate (Right.Tree);

         return Result;

      end;

   end Union;



   procedure Intersection
     (Target : in out Set;
      Source : in     Set) is

      Tgt : Node_Access := First (Target.Tree);
      Src : Node_Access := First (Source.Tree);

      Tgt_Back : constant Node_Access := Target.Tree.Back;
      Src_Back : constant Node_Access := Source.Tree.Back;

   begin

      if Target'Address = Source'Address then
         return;
      end if;

      while Tgt /= Tgt_Back
        and then Src /= Src_Back
      loop

         if Tgt.Element.all < Src.Element.all then

            declare
               X : Node_Access := Tgt;
            begin
               Tgt := Succ (Tgt);
               Delete (Target.Tree, X);
               Free (X);
            end;

         elsif Src.Element.all < Tgt.Element.all then

            Src := Succ (Src);

         else

            Tgt := Succ (Tgt);
            Src := Succ (Src);

         end if;

      end loop;

   end Intersection;



   function Intersection (Left, Right : Set) return Set is
   begin

      if Left'Address = Right'Address then
         return Left;
      end if;

      declare

         Result : Set;

         procedure Process (Node : Node_Access) is
            pragma Inline (Process);

            Dst_Node : Node_Access;
         begin
            Insert_With_Hint
              (Dst_Tree => Result.Tree,
               Dst_Hint => Result.Tree.Back,
               Src_Node => Node,
               Dst_Node => Dst_Node);
         end;

         procedure Intersection is
            new Charles.Algorithms.Generic_Set_Intersection
              (Node_Access,
               Is_Less => Is_Less_Node_Node);

      begin

         Intersection
           (First (Left.Tree), Left.Tree.Back,
            First (Right.Tree), Right.Tree.Back);

         return Result;

      end;

   end Intersection;



   procedure Difference
     (Target : in out Set;
      Source : in     Set) is

      Tgt : Node_Access := First (Target.Tree);
      Src : Node_Access := First (Source.Tree);

      Tgt_Back : constant Node_Access := Target.Tree.Back;
      Src_Back : constant Node_Access := Source.Tree.Back;

   begin

      if Target'Address = Source'Address then
         Clear (Target);
         return;
      end if;

      loop

         if Tgt = Tgt_Back then
            return;
         end if;

         if Src = Src_Back then
            return;
         end if;

         if Tgt.Element.all < Src.Element.all then

            Tgt := Succ (Tgt);

         elsif Src.Element.all < Tgt.Element.all then

            Src := Succ (Src);

         else

            declare
               X : Node_Access := Tgt;
            begin
               Tgt := Succ (Tgt);
               Delete (Target.Tree, X);
               Free (X);
            end;

            Src := Succ (Src);

         end if;

      end loop;

   end Difference;



   function Difference (Left, Right : Set) return Set is
   begin

      if Left'Address = Right'Address then
         return Empty_Set;
      end if;

      declare

         Result : Set;

         procedure Process (Node : Node_Access) is
            pragma Inline (Process);

            Dst_Node : Node_Access;
         begin
            Insert_With_Hint
              (Dst_Tree => Result.Tree,
               Dst_Hint => Result.Tree.Back,
               Src_Node => Node,
               Dst_Node => Dst_Node);
         end;

         procedure Difference is
            new Charles.Algorithms.Generic_Set_Difference
              (Node_Access,
               Is_Less => Is_Less_Node_Node);

      begin

         Difference
           (First (Left.Tree), Left.Tree.Back,
            First (Right.Tree), Right.Tree.Back);

         return Result;

      end;

   end Difference;



   procedure Symmetric_Difference
     (Target : in out Set;
      Source : in     Set) is

      Tgt : Node_Access := First (Target.Tree);
      Src : Node_Access := First (Source.Tree);

      Tgt_Back : constant Node_Access := Target.Tree.Back;
      Src_Back : constant Node_Access := Source.Tree.Back;

      New_Tgt_Node : Node_Access;

   begin

      if Target'Address = Source'Address then
         Clear (Target);
         return;
      end if;

      loop

         if Tgt = Tgt_Back then

            while Src /= Src_Back loop

               Insert_With_Hint
                 (Dst_Tree => Target.Tree,
                  Dst_Hint => Target.Tree.Back,
                  Src_Node => Src,
                  Dst_Node => New_Tgt_Node);

               Src := Succ (Src);

            end loop;

            return;

         end if;

         if Src = Src_Back then
            return;
         end if;

         if Tgt.Element.all < Src.Element.all then

            Tgt := Succ (Tgt);

         elsif Src.Element.all < Tgt.Element.all then

            Insert_With_Hint
              (Dst_Tree => Target.Tree,
               Dst_Hint => Tgt,
               Src_Node => Src,
               Dst_Node => New_Tgt_Node);

            Src := Succ (Src);

         else

            declare
               X : Node_Access := Tgt;
            begin
               Tgt := Succ (Tgt);
               Delete (Target.Tree, X);
               Free (X);
            end;

            Src := Succ (Src);

         end if;

      end loop;

   end Symmetric_Difference;



   function Symmetric_Difference (Left, Right : Set) return Set is
   begin

      if Left'Address = Right'Address then
         return Empty_Set;
      end if;

      declare

         Result : Set;

         procedure Process (Node : Node_Access) is
            pragma Inline (Process);

            Dst_Node : Node_Access;
         begin
            Insert_With_Hint
              (Dst_Tree => Result.Tree,
               Dst_Hint => Result.Tree.Back,
               Src_Node => Node,
               Dst_Node => Dst_Node);
         end;

         procedure Symmetric_Difference is
            new Charles.Algorithms.Generic_Set_Symmetric_Difference
              (Node_Access,
               Is_Less => Is_Less_Node_Node);

      begin

         Symmetric_Difference
           (First (Left.Tree), Left.Tree.Back,
            First (Right.Tree), Right.Tree.Back);

         return Result;

      end;

   end Symmetric_Difference;



   function Is_Subset
     (Item      : Set;
      Container : Set) return Boolean is

      function Is_Subset is
         new Charles.Algorithms.Generic_Includes
           (Node_Access,
            Is_Less => Is_Less_Node_Node);

   begin

      if Item'Address = Container'Address then
         return True;
      end if;

      if Item.Tree.Length > Container.Tree.Length then
         return False;
      end if;

      return Is_Subset (First (Container.Tree), Container.Tree.Back,
                        First (Item.Tree), Item.Tree.Back);

   end Is_Subset;


   function Is_Disjoint
     (Item      : Set;
      Container : Set) return Boolean is

      L_Node : Node_Access := First (Item.Tree);
      R_Node : Node_Access := First (Container.Tree);

      L_Back : constant Node_Access := Item.Tree.Back;
      R_Back : constant Node_Access := Container.Tree.Back;

   begin

      loop

         if L_Node = L_Back then
            return True;
         end if;

         if R_Node = R_Back then
            return True;
         end if;

         if L_Node.Element.all < R_Node.Element.all then

            L_Node := Succ (L_Node);

         elsif R_Node.Element.all < L_Node.Element.all then

            R_Node := Succ (R_Node);

         else

            return False;

         end if;

      end loop;

   end Is_Disjoint;



end AI302.Containers.Indefinite_Ordered_Sets;


