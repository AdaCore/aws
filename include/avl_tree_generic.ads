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
--  ++++++++++++++++++++++++ STOP PACKAGE SPEC PROLOGUE +++++++++++++++++++++++
--

generic
   type Key (<>) is private;
   type Item is private;
   with function Key_For (Node : Item) return Key;
   with function "<" (Left : Key; Right : Key) return Boolean is <>;
package Avl_Tree_Generic is

   type Avl_Tree is private;

   Avl_Tree_Maximum_Exceeded : exception;
   Avl_Tree_Minimum_Exceeded : exception;
   Node_Not_Found : exception;
   Duplicate_Key : exception;
   Balancing_Error : exception;

--  Raises Node_Not_Found if the key doesn't exist in tree
procedure Inquire (Inquiry_Key : in Key; Tree : in Avl_Tree; Info : out Item);

--  Raises Node_Not_Found if the key doesn't exist in tree
procedure Update_Node
               (Update_Item : in Item; Tree : in Avl_Tree);

--  Raises AVL_Tree_Minimum_Exceeded when you try to delete a node that
--  is already deleted
procedure Delete_Node (Delete_Key : in Key; Tree : in out Avl_Tree);

procedure Delete_Tree (Tree : in out Avl_Tree);

--  Raises Duplicate_Key if the key for New_Node already exists in the tree.
--  Raises AVL_Tree_Maximum_Exceeded if machine runs out of memory
procedure Insert_Node (New_Node : in Item; Tree : in out Avl_Tree);

function Number_Of_Nodes (Tree : Avl_Tree) return Natural;

generic
   with procedure Process (The_Item : in out Item; Continue : out Boolean);
procedure Pre_Order_Tree_Traversal (Tree : in Avl_Tree);

generic
   with procedure Process (The_Item : in out Item; Continue : out Boolean);
procedure In_Order_Tree_Traversal (Tree : in Avl_Tree);

generic
   with procedure Process (The_Item : in out Item);
procedure Post_Order_Tree_Traversal (Tree : in Avl_Tree);

private

type Balance is (Lh, Eh, Rh);

type Avlt_Node;

type Avlt_Pointer is access Avlt_Node;

type Avlt_Node is
   record
      Element : Item;
      Left : Avlt_Pointer := null;
      Right : Avlt_Pointer := null;
      Bf : Balance := Eh;
   end record;

type Avl_Tree is
   record
      Root : Avlt_Pointer := null;
      Node_Count : Integer := 0;
   end record;

end Avl_Tree_Generic;
