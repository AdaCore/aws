
with Lists;
generic
    type Label_Type is private;
                       --| This is used to identify nodes in the tree.

    type Value_Type is private; 
                       --| Information being contained in a node of tree


    with function "<" ( 
               X  :in    Label_Type;
               Y  :in    Label_Type
    ) return boolean is <> ;
                       --| Function which defines ordering of nodes
                       --| a < b -> not (b < a) and  (b /= a) for all a and b.
package Labeled_Trees  is 

--| Overview
--| This package creates an ordered binary tree.  This will allow for 
--| quick insertion, and search.  
--|
--| The tree is organized such that 
--|  
--|  label (leftchild) < label (root)    label (root) < label (rightchild)
--| 
--| This means that by doing a left to right search of the tree will 
--| produce the nodes of the tree in ascending order.





--                             Types
--                             -----

type Tree is  private;     --| This is the type exported to represent the
                           --| tree.


type Tree_Iter is private;  --| This is the type which is used to iterate
                            --| over the set.

--|                          Exceptions
--|                          ----------

Label_Already_Exists_In_Tree :exception;     
Label_Not_Present            :exception;
No_More                      :exception;
Tree_Is_Empty                :exception;    

--|                          Operations
--|                          ----------
--|
--| Create              Creates a tree.
--| Destroy_Tree        Destroys the given tree and returns the spaces.
--| Destroy_Deep_Tree   Destroys all space associated with a tree.  This
--|                     includes all nodes and the label and value associated
--|                     with each node.
--| Fetch_Value         Given a tree and or a label this returns the value
--|                     associated with the tree or label.
--| Get_Tree            Given a tree and a label this returns the tree
--|                     whose root is at the label.
--| Forward             This advances the iterator to the next node in the
--|                     iteration.
--| Insert_Node         This inserts a node n into a tree t.
--| Is_Empty            Returns true if the tree is empty false otherwise.
--| Iterator_Label      This returns the label of the node which corresponds
--|                     to the given iterator.
--| Iterator_Value      This returns the value of the node which corresponds
--|                     to the given iterator.
--| Make_Tree           This takes a label and a value and returns a tree.
--| Make_Tree_Iter_In   This returns an iterator to the user in order to start
--|                     an inorder iteration of the tree.  Inorder means
--|                     scan left child, scan node, scan right child.
--| Make_Tree_Iter_Pre  This returns an iterator to the use in order to 
--|                     start a preorder scan of the tree. Preorder is 
--|                     scan node, scan left child, scan right child.
--| Make_Tree_Iter_Post This returns an iterator to the user in order to
--|                     start a postorder scan of the tree. Postorder
--|                     means scan the left child, right child and then the 
--|                     node.  
--| More                This returns true if there are more elements to iterate
--|                     over in the tree.
--| Next                This returns the information associated with the 
--|                     current iterator and advances the iterator.
--| Store_Value         Replaces the given node's information with 
--|                     the given information.       

---------------------------------------------------------------------------

function Create             --| This function creates the tree.

return Tree;

--| Effects
--| This creates a tree containing no information and no children.  An 
--| emptytree.

-------------------------------------------------------------------------------

generic
  with procedure Dispose_Label (L :in out Label_Type);
  with procedure Dispose_Value (V :in out Value_Type);
procedure Destroy_Deep_Tree (     --| Procedure destroys all nodes in a tree 
                                  --| and the label and value assoiciated with
                                  --| each node.
  T :in out Tree
);

-------------------------------------------------------------------------------
    
procedure Destroy_Tree (         --| Destroys a tree.
          T  :in out Tree        --| Tree being destroyed.
);

--| Effects
--| Destroys a tree and returns the space which it is occupying.

--------------------------------------------------------------------------

function Fetch_Value (         --| Get the value of the node with the given 
                               --| label.
       T :in     Tree;         --| The tree which contains the node.
       L :in     Label_Type    --| The label of the node.
) return Value_Type;   

--| Effects 
--| If the label is not present Label_Not_Present is raised.

--------------------------------------------------------------------------

function Fetch_Value (  --| Return the value stored at the root node
                        --| of the given tree.
         T :in Tree
) return Value_Type;

--| Effects
--| Raises Label_Not_Present if the tree T is empty.  

--------------------------------------------------------------------------

function Get_Tree (         --| Get the subtree whose root is labelled L.
       T :in    Tree;       --| Tree which contains the label L.
       L :in    Label_Type  --| The label being searched for.
) return Tree;

--| Raises
--| Raises Label_Not_Present if the label L is not in T.

--------------------------------------------------------------------------

procedure Forward (        --| Advances the iterator to the next node in
                           --| the iteration. 
  I :in out Tree_Iter      --| Iterator being advance.
);

--| OVERVIEW
--| This is used to advance the iterator.  Typically this is used in
--| conjunction with Node_Value and Node_Label.

--------------------------------------------------------------------------

procedure Insert_Node(            --| This procedure inserts a node into the 
                                  --| specified tree.
       T      :in out Tree;       --| Tree being inserted into.
       L      :in     Label_Type; --| The label for the value being inserted.
       V      :in     Value_Type  --| The information to be contained in the 
                                  --| node being inserted.   

); 
--| EFFECTS
--| This adds the node with label L to the tree T.  Label_Already_Exists is 
--| raised if L already exists in T.

--| MODIFIES
--| This modifies the tree T by adding a node whose label is l and value is v.

------------------------------------------------------------------------------

function Is_Empty (        --| Returns true if the tree is empty false
                           --| otherwise.
         T :in     Tree
) return boolean;

------------------------------------------------------------------------------


function Is_Label_In_Tree (            --| Is the given label in the given
                                       --| tree.
         T :in    Tree;                --| The tree being searched.
         L :in    Label_Type           --| The label being searched for.
) return boolean;

------------------------------------------------------------------------------

procedure Is_Label_In_Tree (      --| Sets the variable Present to true if
                                  --| the given label is in the given tree.
           T       :in     Tree;        --| Tree being searched.
           L       :in     Label_Type;  --| Label being searched for.
           Subtree :   out Tree;        --| Subtree which is contains label.
           Present :   out boolean      --| True if label is in tree, false
                                        --| if not.
);

--| OVERVIEW
--| This operation can be used to see if a label is in the tree.
--| If it is the Subtree out parameter can then be used to
--| to update the value field of the label.  The sequence would be
--| 
--|  Is_Label_In_Tree (T, L, Subtree, Present);
--|  if Present then
--|     Store_Value (Subtree, SomeValue);
--|  end if;
--| 
--| If the label is not Present then Subtree is the root of the tree
--| where the label would be stored if it were present.  Thus the following
--| sequence would be useful.
--|
--| Is_Label_In_Tree (T, L, Subtree, Present);
--| if not Present then
--|    Insert_Node (Subtree, L, V);
--| end if;
--| 
--| The advantage to this routine is that the tree need only be searched 
--| once instead of twice once for the existence check and then once for
--| the insertion.

--| MODIFIES
--| The tree T, also sets the variables Present and Subtree.

------------------------------------------------------------------------------

function Iterator_Label (  --| Returns the label of the node corresponding
                           --| to the iterator.
  I :in      Tree_Iter     --| Iterator.
) return Label_Type;

-----------------------------------------------------------------------------

function Iterator_Value (  --| Returns the value of the node corresponding
                           --| to the iterator.
  I :in      Tree_Iter     --| Iterator.
) return Value_Type;

-----------------------------------------------------------------------------

function Make_Tree (          --| This creates a tree given a label and a  
                              --| value.
       L :in     Label_Type;  --| The label.
       V :in     Value_Type   --| The value.
) return Tree;

--| EFFECTS
--| Creates a tree whose root has the given label and value.

------------------------------------------------------------------------------

function Make_Tree_Iter_In  (  --| This sets up an iteration of the nodes
                               --| of the tree in inorder.  
        T :in     Tree         --| Tree being iterated over 
) return Tree_Iter;


--| EFFECTS
--| By using the Next operations the nodes of the tree are returned in
--| in post order. Inorder means return the left child then the node 
--| then the right child.

------------------------------------------------------------------------------

function Make_Tree_Iter_Post (  --| This sets up an iteration of the nodes
                                --| of the tree in postorder.  
        T :in     Tree          --| Tree being iterated over 
) return Tree_Iter;


--| EFFECTS
--| By using the Next operations the nodes of the tree are returned in
--| post order. Post order means return the node first then its left child 
--| and then its right child.

-----------------------------------------------------------------------------

function Make_Tree_Iter_Pre (   --| This sets up an iteration of the nodes
                                --| of the tree in preorder.  Then nodes
                                --| of the tree are returned in ascending 
                                --| order.  
        T :in     Tree          --| Tree being iterated over 
) return Tree_Iter;


--| EFFECTS
--| By using the Next operations the nodes of the tree are returned in
--| ascending order.

-----------------------------------------------------------------------------

function More (                 --| Returns true if there are more elements 
                                --| in the tree to iterate over.
          I :in Tree_Iter  
) return boolean;


-----------------------------------------------------------------------------

procedure Next (                --| This returns the next element in the 
                                --| iteration.
    I :in out Tree_Iter;        --| The Iter which marks the position in the 
                                --| Tree.
    V :   out Value_Type        --| Information being returned from a node.
);    
--| EFFECTS
--| No_More is raised when after the last element has been returned an attempt
--| is made to get another element.


---------------------------------------------------------------------------

procedure Next (                --| This is the iterator operation.  
    I :in out Tree_Iter;        --| The iterator which marks the position in
                                --| the Tree.
    V :   out Value_Type;       --| Information being returned from a node.
    L :   out Label_Type        --| The label of the node in the iteration.

);    

--| EFFECTS
--| This iteration operation returns the label of a node as well as the 
--| nodes value.  No_More is raised if Next is called after the last
--| element of the tree has been returned.


---------------------------------------------------------------------------

procedure Store_Value (             
        T :in out Tree;          --| The tree which contains the label
                                 --| whose value is being changed.
        L :in     Label_Type;    --| The label of the node where the 
                                 --| information is being stored.
        V :in     Value_Type     --| The value being stored.
);

--| MODIFIES
--| The tree T, and the node identified by the label L.  

--| EFFECTS
--| Label_Not_Present is raised if L is not in T.

---------------------------------------------------------------------------

procedure Store_Value (          --| This stores the value V in the root
                                 --| node of the tree T.   
        T :in out Tree;          --| Tree value being stored in the tree.
        V :in     Value_Type     --| The value being stored.
);

--| MODIFIES
--| The tree T, and the node identified by the label L.

--| EFFECTS
--| Raises Label_Not_Present if T is empty.

-------------------------------------------------------------------------------

private

   type Node;
   type Tree is access Node;

   type Node is 
        record
            Label           :Label_Type;
            Value           :Value_Type;
            Left_Child      :Tree;
            Right_Child     :Tree;
        end record;

   package Node_Order is new Lists (Tree);


   type Tree_Iter is
      record
          Node_List :Node_Order.List;
          State     :Node_Order.ListIter;
      end record;
 

end Labeled_Trees;
