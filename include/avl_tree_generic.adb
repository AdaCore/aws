--  START SCCS INFORMATION

--  File Name:           avl_tree_generic_.a
--  SCCS Identification: 3.1
--  Date Of This Delta:  10/13/92 21:41:24
--  Retrieved On:        10/14/92 08:53:26

--  END SCCS INFORMATION

--  ++++++++++++++++++++++++ START PACKAGE SPEC PROLOGUE ++++++++++++++++++++++
--
--  COMPONENT NAME:
--  AVL_Tree_Generic
--
--  ABSTRACT:
--  This is a generic package that implements the AVL search tree for any
--  instantiated object. An AVL tree is a special form of a binary search
--  tree.
--
--  AUTHOR:
--  Sam Siewert
--
--  CREATION DATE:
--  March 1992
--
--  PROJECT/PROGRAM:
--  AASC/Ada Simulation Development System (ASDS)
--  McDonnell Douglas Space Systems Company - Houston Division,
--  Houston, Texas
--
--  REFERENCES:
--  Adelson-Velskii, G.M. and Landis, E.M., Dokl. Akad. Nauk SSSR 146 (1962),
--  263-266; English translation: Soviet Math. (Dokl.) 3 (1962), 1259-1263.
--
--  Siewert, S.B., "Common Lunar Lander Tool Improvement: AVL Tree Name List
--  Implementation," MDSSC-HD Internal Memo: CLL-92-001, 3/26/92.
--
--  KEYWORDS:
--  Trees
--  AVL
--
--  IMPLEMENTATION DEPENDENCIES:
--  none
--
--  MODIFICATION HISTORY:
--  5/92, Darren Davenport added the generic tree traversal code and took out
--  the list code.
--
--  +++++++++++++++++++++++ STOP PACKAGE SPEC PROLOGUE ++++++++++++++++++++++++
--

--  ************************ The Body ********************************

--  START SCCS INFORMATION

--  File Name:           avl_tree_generic.a
--  SCCS Identification: 3.1
--  Date Of This Delta:  10/13/92 21:41:22
--  Retrieved On:        10/14/92 08:53:26

--  END SCCS INFORMATION

--  ++++++++++++++++++++++++ START PACKAGE BODY PROLOGUE ++++++++++++++++++++++
--
--  COMPONENT NAME:
--  AVL_Tree_Generic
--
--  ABSTRACT:
--
--
--  AUTHOR:
--  Sam Siewert
--
--  CREATION DATE:
--  March 1992
--
--  IMPLEMENTATION DEPENDENCIES:
--  none
--
--  MODIFICATION HISTORY:
--  5/92, Darren Davenport added the generic tree traversal code and took out
--  the list code.
--
--  ++++++++++++++++++++++++ STOP PACKAGE BODY PROLOGUE +++++++++++++++++++++++
--

with Unchecked_Deallocation;

package body Avl_Tree_Generic is

procedure Free is new Unchecked_Deallocation (Avlt_Node, Avlt_Pointer);

procedure Avlt_Search
               (Root : in Avlt_Pointer; Ikey : in Key; The_Item : out Item) is
begin
   if Root /= null then
      if Ikey = Key_For (Root.Element) then
         The_Item := Root.Element;
      elsif Ikey < Key_For (Root.Element) then
         Avlt_Search (Root.Left, Ikey, The_Item);
      else
         Avlt_Search (Root.Right, Ikey, The_Item);
      end if;
   else
      raise Node_Not_Found;
   end if;
end Avlt_Search;

procedure Inquire (Inquiry_Key : in Key;
                   Tree : in Avl_Tree;
                   Info : out Item) is
begin
   Avlt_Search (Tree.Root, Inquiry_Key, Info);
end Inquire;

procedure Avlt_Update
              (Root : in Avlt_Pointer; Ikey : in Key; The_Item : in Item) is
begin
   if Root /= null then
      if Ikey = Key_For (Root.Element) then
         Root.Element := The_Item;
      elsif Ikey < Key_For (Root.Element) then
         Avlt_Update (Root.Left, Ikey, The_Item);
      else
         Avlt_Update (Root.Right, Ikey, The_Item);
      end if;
   else
      raise Node_Not_Found;
   end if;
end Avlt_Update;

procedure Update_Node (Update_Item : in Item;
                       Tree : in Avl_Tree) is
begin
   Avlt_Update (Tree.Root, Key_For (Update_Item), Update_Item);
end Update_Node;

procedure Rotate_Left (Node_Ptr : in out Avlt_Pointer) is
   Temp : Avlt_Pointer;
begin
   if Node_Ptr = null then
      raise Avl_Tree_Minimum_Exceeded; -- Attempt to rotate empty tree
   elsif Node_Ptr.Right = null then
      raise Avl_Tree_Minimum_Exceeded; -- Attempt to make empty tree the root
   else
      Temp := Node_Ptr.Right;
      Node_Ptr.Right := Temp.Left;
      Temp.Left := Node_Ptr;
      Node_Ptr := Temp;
   end if;
end Rotate_Left;

procedure Rotate_Right (Node_Ptr : in out Avlt_Pointer) is
   Temp : Avlt_Pointer;
begin
   if Node_Ptr = null then
      raise Avl_Tree_Minimum_Exceeded; -- Attempt to rotate empty tree
   elsif Node_Ptr.Left = null then
      raise Avl_Tree_Minimum_Exceeded; -- Attempt to make empty tree the root
   else
      Temp := Node_Ptr.Left;
      Node_Ptr.Left := Temp.Right;
      Temp.Right := Node_Ptr;
      Node_Ptr := Temp;
   end if;
end Rotate_Right;

procedure Delete_Node (Delete_Key : in Key; Tree : in out Avl_Tree) is
   Q : Avlt_Pointer;
   Ishorter : Boolean := True;

   procedure Delete (Root : in out Avlt_Pointer;
                     Ikey : Key;
                     Shorter : in out Boolean) is
   begin
      if Root /= null then
         if Ikey /= Key_For (Root.Element) then
            if Ikey < Key_For (Root.Element) then
               Delete (Root.Left, Ikey, Shorter);
               if Shorter then
                  case Root.Bf is
                     when Eh =>
                        Root.Bf := Rh;
                        Shorter := False;
                     when Lh =>
                        Root.Bf := Eh;
                        Shorter := True;
                     when Rh =>
                        case Root.Right.Bf is
                           when Eh =>
                              Rotate_Left (Root);
                              Root.Right.Bf := Lh;
                              Shorter := False;
                           when Rh =>
                              Rotate_Left (Root);
                              Root.Right.Bf := Eh;
                              Shorter := True;
                           when Lh =>
                              Rotate_Left (Root);
                              Rotate_Right (Root);
                              Root.Right.Bf := Eh;
                              Root.Bf := Eh;
                              Shorter := True;
                        end case;
                  end case;
               end if;
            else
               Delete (Root.Right, Ikey, Shorter);
               if Shorter then
                  case Root.Bf is
                     when Eh =>
                        Root.Bf := Lh;
                        Shorter := False;
                     when Lh =>
                        case Root.Left.Bf is
                           when Eh =>
                              Rotate_Right (Root);
                              Root.Left.Bf := Lh;
                              Shorter := False;
                           when Rh =>
                              Rotate_Right (Root);
                              Rotate_Left (Root);
                              Root.Left.Bf := Eh;
                              Root.Bf := Eh;
                              Shorter := True;
                           when Lh =>
                              Rotate_Right (Root);
                              Root.Left.Bf := Eh;
                              Shorter := True;
                        end case;
                     when Rh =>
                        Root.Bf := Eh;
                        Shorter := True;
                  end case;
               end if;
            end if;
         else     -- IKey = Key_For(Element)
            if Root.Right = null then
               Q := Root;
               Root := Root.Left;
               Free (Q);
               Tree.Node_Count := Tree.Node_Count - 1;
            elsif Root.Left = null then
               Q := Root;
               Root := Root.Right;
               Free (Q);
               Tree.Node_Count := Tree.Node_Count - 1;
            else
               Q := Root.Right;
               while Q.Left /= null loop
                  Q := Q.Left;
               end loop;
               Q.Left := Root.Left;
               Q := Root;
               Root := Root.Right;
               Free (Q);
               Tree.Node_Count := Tree.Node_Count - 1;
            end if; -- end right or left child null
         end if; -- end if IKey = Key_For(Element)
      end if; -- end if Root /= null
   end Delete;

begin
   Delete (Tree.Root, Delete_Key, Ishorter);
end Delete_Node;

procedure Delete_Tree (Tree : in out Avl_Tree) is

   procedure Delete (Root : in out Avlt_Pointer) is
   begin
      if Root /= null then
         if Root.Left = null and Root.Right = null then
            Free (Root);
            Root := null;
            Tree.Node_Count := Tree.Node_Count - 1;
         elsif Root.Left /= null and Root.Right = null then
            Delete (Root.Left);
            Free (Root);
            Root := null;
            Tree.Node_Count := Tree.Node_Count - 1;
         elsif Root.Right /= null and Root.Left = null then
            Delete (Root.Right);
            Free (Root);
            Root := null;
            Tree.Node_Count := Tree.Node_Count - 1;
         else
            Delete (Root.Left);
            Delete (Root.Right);
            Free (Root);
            Root := null;
            Tree.Node_Count := Tree.Node_Count - 1;
         end if;
      end if;
   end Delete;

begin
   Delete (Tree.Root);
end Delete_Tree;

procedure Left_Balance (Root : in out Avlt_Pointer;
                        Taller : in out Boolean) is
   X, W : Avlt_Pointer;
begin
   X := Root.Left;
   case X.Bf is
      when Lh =>
         Root.Bf := Eh;
         X.Bf := Eh;
         Rotate_Right (Root);
         Taller := False;
      when Eh =>
         raise Balancing_Error;
      when Rh =>
         W := X.Right;
         case W.Bf is
            when Eh =>
               Root.Bf := Eh;
               X.Bf := Eh;
            when Rh =>
               Root.Bf := Eh;
               X.Bf := Lh;
            when Lh =>
               Root.Bf := Rh;
               X.Bf := Eh;
         end case;
         W.Bf := Eh;
         Rotate_Left (X);
         Root.Left := X;
         Rotate_Right (Root);
         Taller := False;
   end case;
end Left_Balance;


procedure Right_Balance (Root : in out Avlt_Pointer;
                         Taller : in out Boolean) is
   X, W : Avlt_Pointer;
begin
   X := Root.Right;
   case X.Bf is
      when Rh =>
         Root.Bf := Eh;
         X.Bf := Eh;
         Rotate_Left (Root);
         Taller := False;
      when Eh =>
         raise Balancing_Error;
      when Lh =>
         W := X.Left;
         case W.Bf is
            when Eh =>
               Root.Bf := Eh;
               X.Bf := Eh;
            when Lh =>
               Root.Bf := Eh;
               X.Bf := Rh;
            when Rh =>
               Root.Bf := Lh;
               X.Bf := Eh;
         end case;
         W.Bf := Eh;
         Rotate_Right (X);
         Root.Right := X;
         Rotate_Left (Root);
         Taller := False;
   end case;
end Right_Balance;

procedure Insert_Node (New_Node : in Item; Tree : in out Avl_Tree) is
   Inew_Node_Ptr : Avlt_Pointer;
   Italler : Boolean := False;

   procedure Insert (Root : in out Avlt_Pointer;
                     New_Node_Ptr : in Avlt_Pointer;
                     Taller : in out Boolean) is
      Tallersubtree : Boolean := False;
   begin
      if Root = null then
         Root := New_Node_Ptr;
         Root.Left := null;
         Root.Right := null;
         Root.Bf := Eh;
         Taller := True;
      elsif Key_For (New_Node_Ptr.Element) = Key_For (Root.Element) then
         raise Duplicate_Key;
      elsif Key_For (New_Node_Ptr.Element) < Key_For (Root.Element) then
         Insert (Root.Left, New_Node_Ptr, Tallersubtree);
         if Tallersubtree then
            case Root.Bf is
               when Lh =>
                  Left_Balance (Root, Taller);
               when Eh =>
                  Root.Bf := Lh;
                  Taller := True;
               when Rh =>
                  Root.Bf := Eh;
                  Taller := False;
            end case;
         else
            Taller := False;
         end if;
      else
         Insert (Root.Right, New_Node_Ptr, Tallersubtree);
         if Tallersubtree then
            case Root.Bf is
               when Lh =>
                  Root.Bf := Eh;
                  Taller := False;
               when Eh =>
                  Root.Bf := Rh;
                  Taller := True;
               when Rh =>
                  Right_Balance (Root, Taller);
            end case;
         else
            Taller := False;
         end if;
      end if;
   end Insert;

begin
   Inew_Node_Ptr := new Avlt_Node'(New_Node, null, null, Eh);
   Insert (Tree.Root, Inew_Node_Ptr, Italler);
   Tree.Node_Count := Tree.Node_Count + 1;
exception
   when Storage_Error =>
      raise Avl_Tree_Maximum_Exceeded;
end Insert_Node;

function Number_Of_Nodes (Tree : Avl_Tree) return Natural is
begin
   return Tree.Node_Count;
end Number_Of_Nodes;

procedure Pre_Order_Tree_Traversal (Tree : in Avl_Tree) is

   procedure Traverse_Pre_Order (Root : in Avlt_Pointer) is
      Continue : Boolean;
   begin
      if Root /= null then
         Process (Root.Element, Continue);
         if Continue then
            Traverse_Pre_Order (Root.Left);
            Traverse_Pre_Order (Root.Right);
         end if;
      end if;
   end Traverse_Pre_Order;

begin
   Traverse_Pre_Order (Tree.Root);
end Pre_Order_Tree_Traversal;

procedure In_Order_Tree_Traversal (Tree : in Avl_Tree) is

   procedure Traverse_In_Order (Root : in Avlt_Pointer) is
      Continue : Boolean;
   begin
      if Root /= null then
         Traverse_In_Order (Root.Left);
         Process (Root.Element, Continue);
         if Continue then
            Traverse_In_Order (Root.Right);
         end if;
      end if;
   end Traverse_In_Order;

begin
   Traverse_In_Order (Tree.Root);
end In_Order_Tree_Traversal;

procedure Post_Order_Tree_Traversal (Tree : in Avl_Tree) is

   procedure Traverse_Post_Order (Root : in Avlt_Pointer) is
   begin
      if Root /= null then
         Traverse_Post_Order (Root.Left);
         Traverse_Post_Order (Root.Right);
         Process (Root.Element);
      end if;
   end Traverse_Post_Order;

begin
   Traverse_Post_Order (Tree.Root);
end Post_Order_Tree_Traversal;

end Avl_Tree_Generic;
