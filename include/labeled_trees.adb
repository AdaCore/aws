with unchecked_deallocation;
package body Labeled_Trees is

----------------------------------------------------------------------------
--                   Local Subprograms
----------------------------------------------------------------------------

procedure Free is new unchecked_deallocation (Node, Tree);

function equal (
       X :in     Label_Type;
       Y :in     Label_Type
) return boolean is 

begin
    return (not (X < Y))  and  (not (Y < X));
end equal;

------------------------------------------------------------------------------

procedure Internal_Is_Label_In_Tree (
   T         :in      Tree;
   L         :in      Label_Type;
   Parent    :in out Tree;
   Present   :   out boolean;
   recursed  :in out boolean
) is
begin
    --| OVERVIEW
    --| This procedure is used so that
    --| Is_Label_In_Tree (T, L, Subtree, Present) returns more useful 
    --| information.  If the label L is not in the tree then Subtree is
    --| the root of the tree where L should be inserted.  If L is in 
    --| the tree then Subtree is the root of the tree where L is.
    --| This procedure is necessary because in Is_Label_In_Tree has Subtree
    --| as an out parameter not as in out.

    --| The variable Recursed is used to indicate whether we have called
    --| the procedure recursively.  It is used when T is null.  If T is
    --| null and we haven't called recursively then T's parent is null.
    --| If T is null and we have called the procedure recusively then
    --| T's parent is not null.

    if T = null then
        Present := false;
        if not Recursed then
            Parent := null;
        end if;
    elsif L < T.Label then
        Parent := T;
        recursed := true;
        Internal_Is_Label_In_Tree (T.Left_Child, L, Parent, Present, Recursed);
    elsif T.Label < L then
        Parent := T;
        Recursed := true;
        Internal_Is_Label_In_Tree (
          T.Right_Child , L, Parent, Present, Recursed
                                  );
    else
        Parent := T;
        Present := true;
    end if;
end Internal_Is_Label_In_Tree;

------------------------------------------------------------------------------

function Pre_Order_Generate (
          T :in Tree
) return  Node_Order.List is


--| This routine generates a list of pointers to nodes in the tree t.
--| The list of nodes is a pre order list of the nodes of the tree.

    L : Node_Order.List;
begin 
    L := Node_Order.Create;
    if T /= null then
        Node_Order.Attach (L, T);
        Node_Order.Attach (L, Pre_Order_Generate (T.Left_Child));
        Node_Order.Attach (L, Pre_Order_Generate (T.Right_Child));
    end if;
    return L;
end Pre_Order_Generate;

------------------------------------------------------------------------------

function Post_Order_Generate (
          T :in Tree
) return  Node_Order.List is


--| This routine generates a list of pointers to nodes in the tree t.
--| The list is a post ordered list of nodes of the tree.

    L : Node_Order.List;
begin 
    L := Node_Order.Create;
    if T /= null then
        L := Post_Order_Generate (T.Left_Child);
        Node_Order.Attach (L, Post_Order_Generate (T.Right_Child));
        Node_Order.Attach (L, T);
    end if;
    return L;
end Post_Order_Generate;

------------------------------------------------------------------------------

function In_Order_Generate (
          T :in Tree
) return  Node_Order.List is


--| This routine generates a list of pointers to nodes in the tree t.
--| The list is ordered with respect to the order of the nodes in the tree.
--| The nodes in the list are such the element 1 < element 2 < .... 
--| element (n - 1) < element (n).  Where < is passed in .

    L : Node_Order.List;
begin 
    L := Node_Order.Create;
    if T /= null then
        L := In_Order_Generate (T.Left_Child);
        Node_Order.Attach (L, T);
        Node_Order.Attach (L, In_Order_Generate (T.Right_Child));
    end if;
    return L;
end In_Order_Generate;

------------------------------------------------------------------------------



------------------------------------------------------------------------------
--                    Visible Subprograms
------------------------------------------------------------------------------

------------------------------------------------------------------------------

function Create  return Tree is

begin
    return null;
end;

------------------------------------------------------------------------------

procedure Destroy_Deep_Tree (
  T :in out Tree
) is

begin
    --| ALGORITHM
    --| Walk over the tree destroying the value, the label, and then the node
    --| itself.  Do this in post order.  This means destroy the left child
    --| destroy the right child and then destroy the node.

    if T /= null then
       Destroy_Deep_Tree (T.Left_Child);
       Destroy_Deep_Tree (T.Right_Child);
       Dispose_Label (T.Label);
       Dispose_Value (T.Value);
       Destroy_Tree (T);
    end if;
end;

------------------------------------------------------------------------------

procedure Destroy_Tree ( T :in out Tree) is


begin
    --| OVERVIEW
    --| This procedure recursively destroys the tree T.
    --|  1.  It destroy the Left_Child of T
    --|  2.  It then destroys the Right_Child of T.
    --|  3.  It then destroy the root T and set T to be null.

    if T /= null then
        Destroy_Tree (T.Left_Child);
        Destroy_Tree (T.Right_Child);
        Free (T);
    end if;
end Destroy_Tree;

------------------------------------------------------------------------------

function Fetch_Value (         --| Get the value of the node with the given 
                               --| value.
       T :in     Tree;         --| The tree which contains the node.
       L :in     Label_Type    --| The label of the node.
) return Value_Type is

begin
    if T = null then 
        raise Label_Not_Present;
    elsif L < T.Label then
        return Fetch_Value (T.Left_Child, L);
    elsif T.Label < L then
        return Fetch_Value (T.Right_Child, L);
    else
        return T.Value;
    end if;               
end Fetch_Value;
     
--------------------------------------------------------------------------

function Fetch_Value (  --| Return the value stored at the root node
                        --| of the given tree.
         T :in Tree
) return Value_Type is

begin
    if T = null then
       raise Tree_Is_Empty;	
    else
       return T.Value;
    end if;
end Fetch_Value;
  
--------------------------------------------------------------------------

procedure Forward (        --| Advances the iterator to the next node in
                           --| the iteration. 
  I :in out Tree_Iter      --| Iterator being advance.
) is
begin
    Node_Order.Forward (I.State);
end Forward;

------------------------------------------------------------------------------

function Get_Tree (         --| Get the tree whose root is labelled L.
       T :in    Tree;       --| Tree which contains the label L.
       L :in    Label_Type  --| The label being searched for.
) return Tree is

begin
    if T = null then
        raise Label_Not_Present;
    elsif L < T.Label then
        return Get_Tree (T.Left_Child, L);
    elsif T.Label < L then
        return Get_Tree (T.Right_Child, L);
    else
       return T;
    end if;
end Get_Tree;

------------------------------------------------------------------------------

procedure Insert_Node (       --| This procedure inserts a node into
                              --| the tree T with label and value V.
      T  :in out Tree;
      L  :in     Label_Type;
      V  :in     Value_Type
) is

begin
    if T = null then
       T := new Node ' 
            ( Value => V, Label => L, Left_Child => null, Right_Child => null);
    elsif L < T.Label then
       Insert_Node (T.Left_Child, L, V);
    elsif T.Label < L then
       Insert_Node (T.Right_Child, L, V);
    elsif T.Label = L then
       raise Label_Already_Exists_In_Tree;
    end if;
end Insert_Node; 
	
------------------------------------------------------------------------------

function Is_Empty (        --| Returns true if the tree is empty false
                           --| otherwise.
         T :in     Tree
) return boolean is
begin
    return T = null;
end Is_Empty;

------------------------------------------------------------------------------

function Is_Label_In_Tree (            --| Is the given label in the given
                                       --| tree.
         T :in    Tree;                --| The tree being searched.
         L :in    Label_Type           --| The label being searched for.
) return boolean is
begin
    if T = null then
         return false;
    elsif L < T.Label then
         return Is_Label_In_Tree (T.Left_Child, L);
    elsif T.Label < L then
         return Is_Label_In_Tree (T.Right_Child, L);
    else
        return true;
    end if;
end Is_Label_In_Tree;

------------------------------------------------------------------------------

procedure Is_Label_In_Tree (            --| Checks if the given label is 
                                        --| in the given tree.
           T       :in     Tree;        --| Tree being searched.
           L       :in     Label_Type;  --| Label being searched for.
           Subtree :   out Tree;        --| Subtree which is contains label.
           Present :   out boolean      --| True if label is in tree, false
                                        --| if not.
) is
    Recursed          :boolean := false;
    Internal_Subtree  :Tree;    -- This variable is needed because
                                -- in Internal_Is_Label subtree is an in out
                                -- parameter.                   
      
begin
     --| Sets the variable Present to true if the given label is in the given 
     --| tree. Also sets the variable Subtree to 
     --| the root of the subtree which contains the label.  If L isn't in the
     --| tree then Subtree is the root of the tree where label should be
     --| inserted.  This internal routine is called so that if L isn't in T
     --| then Subtree will be the root of the tree where L should be inserted.
     --| In order to do this we need the extra variable Recursed.

    Internal_Is_Label_In_Tree (T, L, Internal_Subtree, Present, Recursed);
    Subtree := Internal_Subtree;
end Is_Label_In_Tree;

----------------------------------------------------------------------------
 
function Iterator_Label (  --| Returns the label of the node corresponding
                           --| to the iterator.
  I :in      Tree_Iter     --| Iterator.
) return Label_Type is
    T :Tree;
begin
    T := Node_Order.CellValue (I.State);
    return T.Label;
end Iterator_Label;

-----------------------------------------------------------------------------

function Iterator_Value (  --| Returns the value of the node corresponding
                           --| to the iterator.
  I :in      Tree_Iter     --| Iterator.
) return Value_Type is
    T :Tree;
begin
    T := Node_Order.CellValue (I.State);
    return T.Value;
end;

-------------------------------------------------------------------------------

function Make_Tree (          --| This creates a tree given a label and a  
                              --| value.
       L :in     Label_Type;  --| The label.
       V :in     Value_Type   --| The value.
) return Tree is

begin
     return  new Node ' ( 
                   Value => V, 
                   Label => L, 
                   Left_Child => null,
                   Right_Child => null
                        );
end;

-------------------------------------------------------------------------------

function Make_Tree_Iter_In  (  --| This sets up an inoder iteration of the 
                               --| nodes of the tree.
        T :in     Tree         --| Tree being iterated over 
) return Tree_Iter is

--| This sets up the iterator for a tree T.
--| The NodeList keeps track of the order of the nodes of T.  The Node_List
--| is computed by first invoking In_Generate of the Left_Child then append
--| the root node to Node_List and then append the result of In_Generate
--| to Node_List.  Since the tree is ordered such that 
--|
--|    Left_Child < root    root < Right_Child 
--| 
--| Node_Order returns the nodes in ascending order.
--|
--| Thus Node_List keeps the list alive for the duration of the iteration
--| operation.  The variable State is the a pointer into the Node_List
--| which is the current place of the iteration.

    I :Tree_Iter;
begin
    I.Node_List := Node_Order.Create;
    if T /= null then
        Node_Order.Attach (I.Node_List, In_Order_Generate (T));	
    end if;
    I.State := Node_Order.MakeListIter (I.Node_List);
    return I;	
end Make_Tree_Iter_In;    

------------------------------------------------------------------------------

function Make_Tree_Iter_Post (  --| This sets up a postorder iteration of the
                                --| nodes of the tree.
        T :in     Tree          --| Tree being iterated over 
) return Tree_Iter is

--| A postorder iteration of the tree ( + a b)  where the root is + and 
--| the left child is a and the right child is b will return the nodes
--| in the order a b +.  
--| Node_List is a post_ordered list of the nodes of the tree generated 
--| by Post_Order Generate. Thus Node_List keeps the list alive for the 
--| duration of the iteration operation.  The variable State is the a pointer 
--| into the Node_List which is the current place of the iteration.

    I :Tree_Iter;
begin
    I.Node_List := Node_Order.Create;
    if T /= null then
        Node_Order.Attach (I.Node_List, Post_Order_Generate (T));	
    end if;
    I.State := Node_Order.MakeListIter (I.Node_List);
    return I;	
end Make_Tree_Iter_Post;    

-----------------------------------------------------------------------------

function Make_Tree_Iter_Pre (   --| This sets up an iteration of the nodes
                                --| of the tree in preorder.  Then nodes
                                --| of the tree are returned in ascending 
                                --| order.  
        T :in     Tree          --| Tree being iterated over 
) return Tree_Iter is


--| A preorder iteration of the tree ( + a b)  where the root is + and 
--| the left child is a and the right child is b will return the nodes
--| in the order + a b .  
--| Node_List is a pre_ordered list of the nodes of the tree generated 
--| by Pre_Order_Generate. Thus Node_List keeps the list alive for the 
--| duration of the iteration operation.  The variable State is the a pointer 
--| into the Node_List which is the current place of the iteration.

    I :Tree_Iter;
begin
    I.Node_List := Node_Order.Create;
    if T /= null then
        Node_Order.Attach (I.Node_List, Pre_Order_Generate (T));	
    end if;
    I.State := Node_Order.MakeListIter (I.Node_List);
    return I;	
end Make_Tree_Iter_Pre;    

------------------------------------------------------------------------------

function More (
     I :in Tree_Iter
) return boolean is
   
begin
    return Node_Order.More (I.State);
end More;

------------------------------------------------------------------------------

procedure Next (
          I     :in out Tree_Iter;
          V     :   out Value_Type       
) is


    T :Tree;
begin
    --| OVERVIEW	
    --| Next returns the information at the current position in the iterator
    --| and increments the iterator.  This is accomplished by using the iterater
    --| associated with the Node_Order list.  This returns a pointer into the Tree
    --| and then the information found at this node in T is returned.
    Node_Order.Next (I.State, T);
    V := T.Value ;
exception 
    when Node_Order.NoMore => 
      raise No_More;
    when others =>
      raise;
end Next;

-----------------------------------------------------------------------------

procedure Next ( 
          I :in out Tree_Iter;
          V :   out Value_Type;
          L :   out Label_Type
) is

    T :Tree;
begin
    --| OVERVIEW	
    --| Next returns the information at the current position in the iterator
    --| and increments the iterator.  This is accomplished by using the 
    --| iterater associated with the Node_Order list.  This returns a 
    --| pointer into the Tree and then the information found at this node in 
    --| T is returned.

    Node_Order.Next (I.State, T);
    V := T.Value ;
    L := T.Label;

exception 
    when Node_Order.NoMore => 
      raise No_More;
    when others =>
      raise;
end Next;

-----------------------------------------------------------------------------

procedure Store_Value (             
        T :in out Tree;          --| Tree value is being stored in.
        L :in     Label_Type;    --| The label of the node where the 
                                 --| information is being stored.
        V :in     Value_Type     --| The value being stored.
) is

begin
    if T = null then
        raise Label_Not_Present;
    elsif L < T.Label then
        Store_Value (T.Left_Child, L, V);
    elsif T.Label < L then
        Store_Value (T.Right_Child, L, V);
    else
        T.Value := V;
    end if;
end Store_Value; 

-------------------------------------------------------------------------------

procedure Store_Value (          --| This stores the value V in the root
                                 --| node of the tree T.   
        T :in out Tree;          --| Tree value being stored in the tree.
        V :in     Value_Type     --| The value being stored.
) is
begin
    if T /= null then 
        T.Value := V;
    else
        raise Label_Not_Present;
    end if;
end Store_Value;

-----------------------------------------------------------------------------
end Labeled_Trees;
