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
            Y : constant Node_Access := Copy_Node (X);
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
         Position.Node,
         Success);

      --pragma Debug (Check_Invariant (Container.Tree));

   end;


   procedure Delete (Container : in out Set;
                     Position  : in out Cursor) is
   begin

      if Position.Node = null
        or else Position.Node = Container.Tree.Back
      then
         return;
      end if;

      pragma Assert (Position.Node.Color /= White);

      Delete (Container.Tree, Position.Node);

      declare
         X : Node_Access := Position.Node;
      begin
         Free (X);
      end;

      Position.Node := null;

      --pragma Debug (Check_Invariant (Container.Tree));

   end Delete;


   function Find (Container : Set;
                  Item      : Element_Type) return Cursor is
   begin
      return (Node => Element_Keys.Find (Container.Tree, Item));
   end;


   function Is_In (Item      : Element_Type;
                   Container : Set) return Boolean is
   begin
      return Find (Container, Item) /= No_Element;
   end;


   function First (Container : Set) return Cursor is
   begin
      return (Node => First (Container.Tree));
   end;


   function First_Element (Container : Set) return Element_Type is
      C : constant Cursor := First (Container);
   begin
      return C.Node.Element.all;
   end;


   function Last (Container : Set) return Cursor is
   begin
      return (Node => Last (Container.Tree));
   end;


   function Last_Element (Container : Set) return Element_Type is
      C : constant Cursor := Last (Container);
   begin
      return C.Node.Element.all;
   end;


   function Back (Container : Set) return Cursor is
   begin
      return (Node => Container.Tree.Back);
   end;


   function Next (Position : Cursor) return Cursor is
   begin
      --TODO
      return (Node => Succ (Position.Node));
   end;


   function Previous (Position : Cursor) return Cursor is
   begin
      return (Node => Pred (Position.Node));
   end;


   procedure Next (Position : in out Cursor) is
   begin
      Position.Node := Succ (Position.Node);
   end;


   procedure Previous (Position : in out Cursor) is
   begin
      Position.Node := Pred (Position.Node);
   end;


   function Element (Position : Cursor) return Element_Type is
   begin
      return Position.Node.Element.all;  --works for Back node, too
   end;


   procedure Generic_Update (Position : in Cursor) is
   begin
      pragma Assert (Position.Node.Color /= White);
      Process (Position.Node.Element.all);
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
      return Left.Node.Element.all < Right.Node.Element.all;
   end;


   function ">" (Left, Right : Cursor) return Boolean is
      -- L > R same as R < L
   begin
      return Right.Node.Element.all < Left.Node.Element.all;
   end;


   function "<" (Left : Cursor; Right : Element_Type)
      return Boolean is
   begin
      return Left.Node.Element.all < Right;
   end;

   function ">" (Left : Cursor; Right : Element_Type)
      return Boolean is
   begin
      return Right < Left.Node.Element.all;
   end;

   function "<" (Left : Element_Type; Right : Cursor)
      return Boolean is
   begin
      return Left < Right.Node.Element.all;
   end;

   function ">" (Left : Element_Type; Right : Cursor)
      return Boolean is
   begin
      return Right.Node.Element.all < Left;
   end;


   procedure Generic_Iteration (Container : in Set) is

      procedure Process (Node : Node_Access) is
         pragma Inline (Process);
      begin
         Process (Cursor'(Node => Node));
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
         Process (Cursor'(Node => Node));
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
                     Key       : Key_Type)
        return Cursor is
      begin
         return (Node => Key_Keys.Find (Container.Tree, Key));
      end;


      function Is_In (Key       : Key_Type;
                      Container : Set)
         return Boolean is
      begin
         return Find (Container, Key) /= Back (Container);
      end;


      function Element (Container : Set;
                        Key       : Key_Type)
        return Element_Type is

         C : constant Cursor := Find (Container, Key);
      begin
         return C.Node.Element.all;
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
         return Left < Right.Node.Element.all;
      end;

      function ">" (Left : Key_Type; Right : Cursor)
         return Boolean is
      begin
         return Left > Right.Node.Element.all;
      end;

      function "<" (Left : Cursor; Right : Key_Type)
        return Boolean is
      begin
         return Right > Left.Node.Element.all;
      end;

      function ">" (Left : Cursor; Right : Key_Type)
        return Boolean is
      begin
         return Right < Left.Node.Element.all;
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
            Position.Node,
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


end AI302.Containers.Indefinite_Ordered_Sets;


