--------------------------------- COPYRIGHT ------------------------------------
-- (C) 1987 Swiss Federal Institute of Technology (EPFL).                     --
--     Represented by A. Strohmeier EPFL-DI-LGL CH-1015 Lausanne Switzerland. --
--     All Rights Reserved.                                                   --
--------------------------------------------------------------------------------

--+ TITLE:      GENERIC PACKAGE FOR ASSOCIATIVE TABLES.
--+ REVISION:   13-JUL-1992 Ph. Kipfer (PKR), File header format
--+ APPROVAL:   03-DEC-1987 C. Genillard.
--+ CREATION:   29-JUN-1987 A. Strohmeier.

generic
  type Key_Type is limited private;
  with function Less (Left, Right: Key_Type) return Boolean;
  -- Defines ordering of keys.
  with function Equals (Left, Right: Key_Type) return Boolean;
  -- Defines equality between keys.
  with procedure Assign (Destination: in out Key_Type; Source: in Key_Type);
  -- Assigns SOURCE to DESTINATION. If needed, DESTINATION has to be destroyed
  -- before assignement, since ASSIGN is called without a previous call to
  -- DESTROY in the implementation of the package.
  with procedure Destroy (Key: in out Key_Type);

  type Value_Type is private;
package Table_Of_Dynamic_Keys_And_Static_Values_G is

--+ OVERVIEW:
--+   This package provides associative tables of unlimited dynamic size with
--+ entries of type (KEY_TYPE, VALUE_TYPE), where KEY_TYPE and VALUE_TYPE are
--+ specified by generic parameters. Such a couple will also be called an item.
--+   The type TABLE_TYPE is implemented in such a way that every object has
--+ the implied initial value of an empty table.
--+   Two items (k1, v1) and (k2, v2) have same key if and only if
--+ EQUALS (k1, k2).
--+   A table may not contain duplicate items, that is having same key.
--+   The following consistency condition must be fullfilled by the relational
--+ operations LESS and EQUALS:
--+   (i)  EQUALS (k1, k2) implies not LESS (k1, k2) and not LESS (k2, k1)
--+   (ii) not LESS (k1, k2) and not EQUALS (k1, k2) implies LESS (k2, k1).
--+   In our terminology, a static type is a type which is neither a limited
--+ type nor an access type. When an actual generic access type is associated
--+ with a generic static type, objects would be shared, i.e. only the access
--+ value would be stored, without copying the accessed object.
--+   On the opposite, a dynamic type may be limited or an access type.
--+ However a dynamic type must have the feature that every object has an
--+ implied initial value.
--+   Depending on the very nature of the types KEY_TYPE and VALUE_TYPE, one
--+ of the provided packages has to be used:
--+ TABLE_OF_DYNAMIC_KEYS_AND_DYNAMIC_VALUES_G
--+ TABLE_OF_STATIC_KEYS_AND_DYNAMIC_VALUES_G
--+ TABLE_OF_DYNAMIC_KEYS_AND_STATIC_VALUES_G
--+ TABLE_OF_STATIC_KEYS_AND_STATIC_VALUES_G
--+
--+ CAUTION:
--+   Functions which return the value of an item (or part of it) of the
--+ structure share the item with the structure and do not return a copy of it.
--+ This may have consequences if the type of the item, (or some component of
--+ it) is an access type. For instance, when accessing an item by a function
--+ call, this item must not be destroyed or modified during the query.
--+
--+ PRIMITIVES:
--+   CONSTRUCTORS:
--+     ASSIGN
--+     INSERT (2)
--+     INSERT_OR_REPLACE_VALUE
--+     REPLACE_VALUE (2)
--+     REMOVE (3)
--+     REMOVE_MIN (3)
--+     REMOVE_MAX (3)
--+     UPDATE_VALUE_OR_EXCEPTION_G
--+     UPDATE_VALUE_OR_STATUS_G
--+   QUERIES:
--+     SIZE
--+     IS_EMPTY
--+     IS_PRESENT
--+     VALUE
--+     GET_VALUE
--+     GET_MIN_ITEM
--+     GET_MAX_ITEM
--+     MIN_KEY
--+     GET_MIN_KEY
--+     MAX_KEY
--+     GET_MAX_KEY
--+     GET_LESS_ITEM
--+     GET_LESS_OR_EQUAL_ITEM
--+     GET_GREATER_ITEM
--+     GET_GREATER_OR_EQUAL_ITEM
--+     LESS_KEY
--+     GET_LESS_KEY (2)
--+     LESS_OR_EQUAL_KEY
--+     GET_LESS_OR_EQUAL_KEY (2)
--+     GREATER_KEY
--+     GET_GREATER_KEY (2)
--+     GREATER_OR_EQUAL_KEY
--+     GET_GREATER_OR_EQUAL_KEY (2)
--+   SET_OPERATIONS:
--+     SET_OPERATIONS_G
--+       UNION
--+       INTERSECTION
--+       DIFFERENCE
--+       SYMMETRIC_DIFFERENCE
--+       "=" (set equality)
--+       "<" (strict set inclusion)
--+       "<=" (set inclusion)
--+       ">" (strict set inclusion)
--+       ">=" (set inclusion)
--+   ITERATORS:
--+     TRAVERSE_ASC_G
--+     TRAVERSE_DESC_G
--+     TRAVERSE_ASC_AND_UPDATE_VALUE_G
--+     TRAVERSE_DESC_AND_UPDATE_VALUE_G
--+     DISORDER_TRAVERSE_G
--+     DISORDER_TRAVERSE_AND_UPDATE_VALUE_G
--+   HEAP MANAGEMENT:
--+     DESTROY
--+     RELEASE_FREE_LIST
--+     SET_MAX_FREE_LIST_SIZE
--+     FREE_LIST_SIZE
--+
--+ ALGORITHM:
--+    A table is implemented as a balanced search binary tree (AVL-tree)
--+ using pointers. The items are sorted in the table by increasing keys values
--+ in conformance to inorder.
--+    An internal free list is used to avoid returning each free item (i.e.
--+ coming from REMOVE) to the system, so long as the length of this list does
--+ not exceed MAX_FREE_LIST_SIZE, in which case the free item is immediately
--+ returned to the system. When a new item has to be inserted (i.e. by a call
--+ to INSERT), an element is recovered from the free list if it is not empty.
--+ Otherwise, new space is taken from the system.


  type Table_Type is limited private;

  Duplicate_Item_Error,
  Missing_Item_Error,
  Empty_Structure_Error: exception;

--/ CONSTRUCTORS:

  procedure Assign (Destination: in out Table_Type; Source: in Table_Type);
  --+ OVERVIEW:
  --+   Begins by a call to DESTROY (DESTINATION) and then copies SOURCE into
  --+ DESTINATION. Note the "in out" mode of the formal parameter DESTINATION.

  procedure Insert (Table: in out Table_Type;
                    Key: in Key_Type;
                    Value: in Value_Type);
  --+ OVERVIEW:
  --+   Inserts the couple (KEY, VALUE) into TABLE.
  --+ ERROR:
  --+   If an entry with the given key is already in the table, then exception
  --+ DUPLICATE_ITEM_ERROR is raised.

  procedure Insert (Table: in out Table_Type;
                    Key: in Key_Type;
                    Value: in Value_Type;
                    Duplicate_Item: out Boolean);
  --+ OVERVIEW:
  --+   Inserts the couple (KEY, VALUE) into TABLE. No action is taken and no
  --+ error occurs if an entry with the given key is already in the table
  --+ except that DUPLICATE_ITEM is set to true.

  procedure Insert_Or_Replace_Value (Table: in out Table_Type;
                                     Key: in Key_Type;
                                     Value: in Value_Type);
  --+ OVERVIEW:
  --+   Inserts the couple (KEY, VALUE) into TABLE if there is no entry with
  --+ this key. Otherwise the given VALUE replaces the previous one.

  procedure Replace_Value (Table: in out Table_Type;
                           Key: in Key_Type;
                           Value: in Value_Type);
  --+ OVERVIEW:
  --+   An entry having key KEY is searched for in TABLE. The given VALUE then
  --+ replaces the previous one.
  --+ ERROR:
  --+   If there is no entry with the given key, the exception
  --+ MISSING_ITEM_ERROR is raised.

  procedure Replace_Value (Table: in out Table_Type;
                           Key: in Key_Type;
                           Value: in Value_Type;
                           Found: out Boolean);
  --+ OVERVIEW:
  --+   An entry having key KEY is searched for in TABLE. The given
  --+ VALUE then replaces the previous one. No action is taken and no error
  --+ occurs if there is no entry with the given key, except that FOUND
  --+ is set to false.

  procedure Remove (Table: in out Table_Type;
                    Key: in Key_Type);
  procedure Remove (Table: in out Table_Type;
                    Key: in Key_Type;
                    Value: out Value_Type);
  --+ OVERVIEW:
  --+   Removes the entry with key KEY from TABLE and returns in
  --+ parameter VALUE, if present, the associated VALUE.
  --+ ERROR:
  --+   If there is no entry with the given key, the exception
  --+ MISSING_ITEM_ERROR is raised. In this case the value of the actual
  --+ parameter VALUE is left unchanged.


  procedure Remove (Table: in out Table_Type;
                    Key: in Key_Type;
                    Found: out Boolean);
  --+ OVERVIEW:
  --+   Removes the entry with key KEY from TABLE. No action is taken
  --+ and no error occurs if there is no entry with the given key, except
  --+ that FOUND is set to false.

  procedure Remove_Min (Table: in out Table_Type);
  procedure Remove_Min (Table: in out Table_Type;
                        Key: in out Key_Type);
  procedure Remove_Min (Table: in out Table_Type;
                        Key: in out Key_Type;
                        Value: out Value_Type);
  --+ OVERVIEW:
  --+   Removes the entry with the smallest key from TABLE and returns,
  --+ if needed, the values of KEY and VALUE.
  --+ ERROR:
  --+   Raises EMPTY_STRUCTURE_ERROR if TABLE is empty. In this case the
  --+ values of the actual parameters KEY and VALUE are left unchanged.

  procedure Remove_Max (Table: in out Table_Type);
  procedure Remove_Max (Table: in out Table_Type;
                        Key: in out Key_Type);
  procedure Remove_Max (Table: in out Table_Type;
                        Key: in out Key_Type;
                        Value: out Value_Type);
  --+ OVERVIEW:
  --+   Removes the entry with the greatest key from TABLE and returns,
  --+ if needed, the values of KEY and VALUE.
  --+ ERROR:
  --+   Raises EMPTY_STRUCTURE_ERROR if TABLE is empty. In this case the
  --+ values of the actual parameters KEY and VALUE are left unchanged.

  generic
    with procedure Modify (Key: in Key_Type;
                           Value: in out Value_Type) is <>;
  procedure Update_Value_Or_Exception_G (Table: in out Table_Type;
                                         Key: in Key_Type);
  --+ OVERVIEW:
  --+   An entry with key KEY is searched for in TABLE. The associated item
  --+ is then passed to procedure MODIFY for modification of its value part.
  --+ ERROR:
  --+   Raises MISSING_ITEM_ERROR if KEY is not in TABLE.

  generic
    with procedure Modify (Key: in Key_Type;
                           Value: in out Value_Type) is <>;
  procedure Update_Value_Or_Status_G (Table: in out Table_Type;
                                      Key: in Key_Type;
                                      Found: out Boolean);
  --+ OVERVIEW:
  --+   An entry with key KEY is searched for in TABLE. The associated item
  --+ is then passed to procedure MODIFY for modification of its value part.
  --+   No action is taken and no error occurs if there is no entry with the
  --+ given key, except that FOUND is set to false.

--/ QUERIES:

  function Size (Table: in Table_Type) return Natural;
  --+ OVERVIEW:
  --+   Returns the number of entries currently in TABLE.

  function Is_Empty (Table: in Table_Type) return Boolean;
  --+ OVERVIEW:
  --+   Returns TRUE if and only if the TABLE is empty.

  function Is_Present (Table: in Table_Type; Key: in Key_Type) return Boolean;
  --+ OVERVIEW:
  --+   Returns TRUE if and only if an ITEM with key KEY is in TABLE.

  function Value (Table: in Table_Type; Key: in Key_Type) return Value_Type;

  procedure Get_Value(Table: in Table_Type;
                      Key: in Key_Type;
                      Value: out Value_Type);
  --+ OVERVIEW:
  --+   Gives the VALUE associated with KEY in TABLE.
  --+ ERROR:
  --+   Raises MISSING_ITEM_ERROR if KEY is not found in TABLE. In this case
  --+ the value of the actual parameter VALUE is left unchanged.

  procedure Get_Min_Item (Table: in Table_Type;
                           Key: in out Key_Type;
                           Value: out Value_Type);
  --+ OVERVIEW:
  --+   Gives the smallest KEY and the VALUE associated with it in TABLE.
  --+ ERROR:
  --+   Raises EMPTY_STRUCTURE_ERROR if TABLE is empty. In this case the values
  --+ of the actual parameters KEY and VALUE are left unchanged.

  procedure Get_Max_Item (Table: in Table_Type;
                           Key: in out Key_Type;
                           Value: out Value_Type);
  --+ OVERVIEW:
  --+   Gives the biggest KEY and the VALUE associated with it in TABLE.
  --+ ERROR:
  --+   Raises EMPTY_STRUCTURE_ERROR if TABLE is empty. In this case the values
  --+ of the actual parameters KEY and VALUE are left unchanged.

  function Min_Key (Table: in Table_Type) return Key_Type;

  procedure Get_Min_Key (Table: in Table_Type; Key: in out Key_Type);
  --+ OVERVIEW:
  --+   Gives the smallest KEY of TABLE.
  --+ ERROR:
  --+   Raises EMPTY_STRUCTURE_ERROR if TABLE is empty. In this case the value
  --+ of the actual parameter KEY is left unchanged.

  function Max_Key (Table: in Table_Type) return Key_Type;

  procedure Get_Max_Key (Table: in Table_Type; Key: in out Key_Type);
  --+ OVERVIEW:
  --+   Gives the biggest KEY of TABLE.
  --+ ERROR:
  --+   Raises EMPTY_STRUCTURE_ERROR if TABLE is empty. In this case the value
  --+ of the actual parameter KEY is left unchanged.

  procedure Get_Less_Item (Table: in Table_Type;
                            Key: in out Key_Type;
                            Value: out Value_Type);
  --+ OVERVIEW:
  --+   Returns the entry having the greatest key less than the value of
  --+ the actual parameter KEY. KEY is modified in accordance.
  --+ ERROR:
  --+   The exception MISSING_ITEM_ERROR is raised if there is not such an
  --+ entry in the table. In this case the values of the actual parameters KEY
  --+ and VALUE are left unchanged.

  procedure Get_Less_Or_Equal_Item (Table: in Table_Type;
                                     Key: in out Key_Type;
                                     Value: out Value_Type);
  --+ OVERVIEW:
  --+   Returns the entry having the greatest key less than or equal to
  --+ the value of the actual parameter KEY. KEY is modified in accordance.
  --+ ERROR:
  --+   The exception MISSING_ITEM_ERROR is raised if there is not such an
  --+ entry in the table. In this case the values of the actual parameters KEY
  --+ and VALUE are left unchanged.

  procedure Get_Greater_Item (Table: in Table_Type;
                               Key: in out Key_Type;
                               Value: out Value_Type);
  --+ OVERVIEW:
  --+   Returns the entry having the smallest key greater than the value
  --+ of the actual parameter KEY. KEY is modified in accordance.
  --+ ERROR:
  --+   The exception MISSING_ITEM_ERROR is raised if there is not such an
  --+ entry in the table. In this case the values of the actual parameters KEY
  --+ and VALUE are left unchanged.

  procedure Get_Greater_Or_Equal_Item (Table: in Table_Type;
                                        Key: in out Key_Type;
                                        Value: out Value_Type);
  --+ OVERVIEW:
  --+   Returns the entry having the smallest key greater than or equal
  --+ to the value of the actual parameter KEY. KEY is modified in accordance.
  --+ ERROR:
  --+   The exception MISSING_ITEM_ERROR is raised if there is not such an
  --+ entry in the table. In this case the values of the actual parameters KEY
  --+ and VALUE are left unchanged.

  function Less_Key (Table: in Table_Type; Key: in Key_Type) return Key_Type;

  procedure Get_Less_Key (Table: in Table_Type;
                          Key: in out Key_Type);
  --+ OVERVIEW:
  --+   Gives the greatest key less than the value of the parameter KEY.
  --+ ERROR:
  --+   The exception MISSING_ITEM_ERROR is raised if there is not such an
  --+ entry in the table. In this case the value of the actual parameter KEY
  --+ is left unchanged.

  procedure Get_Less_Key (Table: in Table_Type;
                          Key: in out Key_Type;
                          Found: out Boolean);
  --+ OVERVIEW:
  --+   Returns the greatest key less than the value of the actual
  --+ parameter KEY. KEY is modified in accordance. FOUND is set to TRUE or
  --+ FALSE depending on success of search. The value of the actual parameter
  --+ KEY is left unchanged if FOUND is set to FALSE.

  function Less_Or_Equal_Key (Table: in Table_Type; Key: in Key_Type) return Key_Type;

  procedure Get_Less_Or_Equal_Key (Table: in Table_Type;
                                   Key: in out Key_Type);
  --+ OVERVIEW:
  --+   Gives the greatest key less than or equal to the value of the
  --+ parameter KEY.
  --+ ERROR:
  --+   The exception MISSING_ITEM_ERROR is raised if there is not such an
  --+ entry in the table. In this case the value of the actual parameter KEY
  --+ is left unchanged.

  procedure Get_Less_Or_Equal_Key (Table: in Table_Type;
                                   Key: in out Key_Type;
                                   Found: out Boolean);
  --+ OVERVIEW:
  --+   Returns the greatest key less than or equal to the value of the
  --+ actual parameter KEY. KEY is modified in accordance. FOUND is set to
  --+ TRUE or FALSE depending on success of search. The value of the actual
  --+ parameter KEY is left unchanged if FOUND is set to FALSE.

  function Greater_Key (Table: in Table_Type; Key: in Key_Type) return Key_Type;

  procedure Get_Greater_Key (Table: in Table_Type;
                             Key: in out Key_Type);
  --+ OVERVIEW:
  --+   Gives the smallest key greater than the value of the parameter
  --+ KEY.
  --+ ERROR:
  --+   The exception MISSING_ITEM_ERROR is raised if there is not such an
  --+ entry in the table. In this case the value of the actual parameter KEY
  --+ is left unchanged.

  procedure Get_Greater_Key (Table: in Table_Type;
                             Key: in out Key_Type;
                             Found: out Boolean);
  --+ OVERVIEW:
  --+   Returns the smallest key greater than the value of the actual
  --+ parameter KEY. KEY is modified in accordance. FOUND is set to TRUE or
  --+ FALSE depending on success of search. The value of the actual parameter
  --+ KEY is left unchanged if FOUND is set to FALSE.

  function Greater_Or_Equal_Key (Table: in Table_Type;
                                 Key: in Key_Type) return Key_Type;

  procedure Get_Greater_Or_Equal_Key (Table: in Table_Type;
                                      Key: in out Key_Type);
  --+ OVERVIEW:
  --+   Returns the smallest key greater than or equal to the value of
  --+ the parameter KEY.
  --+ ERROR:
  --+   The exception MISSING_ITEM_ERROR is raised if there is not such an
  --+ entry in the table. In this case the value of the actual parameter KEY
  --+ is left unchanged.

  procedure Get_Greater_Or_Equal_Key (Table: in Table_Type;
                                      Key: in out Key_Type;
                                      Found: out Boolean);
  --+ OVERVIEW:
  --+   Returns the smallest key greater than or equal to the value of
  --+ the actual parameter KEY.  KEY is modified in accordance. FOUND is set
  --+ to TRUE or FALSE depending on success of search. The value of the actual
  --+ parameter KEY is left unchanged if FOUND is set to FALSE.

--/ SET_OPERATIONS:

  generic package Set_Operations_G is

    procedure Union (Destination: in out Table_Type;
                     Left,
                     Right: in Table_Type);
    --+ OVERVIEW:
    --+   Union of LEFT and RIGHT. If a key is both in LEFT and RIGHT, the value
    --+ is taken from LEFT.

    procedure Intersection (Destination: in out Table_Type;
                            Left,
                            Right: in Table_Type);
    --+ OVERVIEW:
    --+   Intersection of LEFT and RIGHT. The items are taken from LEFT.

    procedure Difference (Destination: in out Table_Type;
                          Left,
                          Right: in Table_Type);
    --+ OVERVIEW:
    --+   Set difference of LEFT and RIGHT. An item is in the resulting table
    --+ if it is in LEFT and if there is no item with same key in RIGHT.

    procedure Symmetric_Difference (Destination: in out Table_Type;
                                    Left,
                                    Right: in Table_Type);
    --+ OVERVIEW:
    --+   Symmetric set difference of LEFT and RIGHT. An item is in the
    --+ resulting table if it is in LEFT but there is no item with same key in
    --+ RIGHT or if it is in RIGHT but there is no item with same key in LEFT.

    function "=" (Left, Right: in Table_Type) return Boolean;
    --+ OVERVIEW:
    --+   Set equality; the LEFT and RIGHT tables contain entries with same
    --+ keys.

    function "<" (Left, Right: in Table_Type) return Boolean;
    --+ OVERVIEW:
    --+   Strict set inclusion; to each item in the LEFT table an item with
    --+ same key is associated in the RIGHT table, but the two sets are not
    --+ identical.

    function "<=" (Left, Right: in Table_Type) return Boolean;
    --+ OVERVIEW:
    --+   Set inclusion; to each entry in the LEFT table an entry with same
    --+ key is associated in the RIGHT table.

    function ">" (Left, Right: in Table_Type) return Boolean;
    --+ OVERVIEW:
    --+   Strict set inclusion; to each entry in the RIGHT table an entry with
    --+ same key is associated in the LEFT table, but the two sets are not
    --+ identical.

    function ">=" (Left, Right: in Table_Type) return Boolean;
    --+ OVERVIEW:
    --+   Set inclusion; to each entry in the RIGHT table an entry with same
    --+ key is associated in the LEFT table.

  end Set_Operations_G;


--/ ITERATORS:

  generic
    with procedure Action (Key: in Key_Type;
                                 Value: in Value_Type;
                                 Order_Number: in Positive;
                                 Continue: in out Boolean) is <>;
  procedure Traverse_Asc_G (Table: in Table_Type);
  --+ OVERVIEW:
  --+   The entries in TABLE are visited in ascending order of their key
  --+ values. Procedure ACTION is applied on each entry within TABLE.
  --+   ORDER_NUMBER gives the position of the visited entry in order of
  --+ traversal. The boolean CONTINUE specifies if you want to proceed to the
  --+ next entry or if you want to stop traversing. As long as you do not
  --+ modify its value within ACTION, its value remains TRUE.
  --+ REQUIREMENT:
  --+   For your actual procedure  ACTION, you must not use a procedure
  --+ which modifies the traversed table.


  generic
    with procedure Action (Key: in Key_Type;
                                 Value: in Value_Type;
                                 Order_Number: in Positive;
                                 Continue: in out Boolean) is <>;
  procedure Traverse_Desc_G (Table: in Table_Type);
  --+ OVERVIEW:
  --+   The entries in TABLE are visited in descending order of their key
  --+ values. Procedure ACTION is applied on each entry within TABLE.
  --+   ORDER_NUMBER gives the position of the visited entry in order of
  --+ traversal. The boolean CONTINUE specifies if you want to proceed to the
  --+ next entry or if you want to stop traversing. As long as you do not
  --+ modify its value within ACTION, its value remains TRUE.
  --+ REQUIREMENT:
  --+   For your actual procedure  ACTION, you must not use a procedure
  --+ which modifies the traversed table.

  generic
    with procedure Modify (Key: in Key_Type;
                           Value: in out Value_Type;
                           Order_Number: in Positive;
                           Continue: in out Boolean) is <>;
  procedure Traverse_Asc_And_Update_Value_G (Table: in out Table_Type);
  --+ OVERVIEW:
  --+   The entries in TABLE are visited in ascending order of their key
  --+ values. For each visited entry, procedure MODIFY is called. The value of
  --+ the current entry is then replaced by the new value.
  --+   ORDER_NUMBER gives the position of the visited entry in order of
  --+ traversal. The boolean CONTINUE specifies if you want to proceed to the
  --+ next entry or if you want to stop traversing. As long as you do not
  --+ modify its value within MODIFY, its value remains TRUE.
  --+ REQUIREMENT:
  --+   For your actual procedure MODIFY, you must not use a procedure which
  --+ modifies the traversed table.

  generic
    with procedure Modify (Key: in Key_Type;
                           Value: in out Value_Type;
                           Order_Number: in Positive;
                           Continue: in out Boolean) is <>;
  procedure Traverse_Desc_And_Update_Value_G (Table: in out Table_Type);
  --+ OVERVIEW:
  --+   The entries in TABLE are visited in descending order of their key
  --+ values. For each visited entry, procedure MODIFY is called. The item
  --+ value of the current entry is then replaced by the new value.
  --+   ORDER_NUMBER gives the position of the visited entry in order of
  --+ traversal. The boolean CONTINUE specifies if you want to proceed to the
  --+ next entry or if you want to stop traversing. As long as you do not
  --+ modify its value within MODIFY, its value remains TRUE.
  --+ REQUIREMENT:
  --+   For your actual procedure MODIFY, you must not use a procedure which
  --+ modifies the traversed table.

  generic
    with procedure Action (Key: in Key_Type;
                                Value: in Value_Type;
                                Order_Number: in Positive;
                                Continue: in out Boolean) is <>;
  procedure Disorder_Traverse_G (Table: in Table_Type);
  --+ OVERVIEW:
  --+   The entries in TABLE are visited in disorder of their key values.
  --+ procedure ACTION is applied on each entry within TABLE.
  --+   ORDER_NUMBER gives the position of the visited entry in order of
  --+ traversal. The boolean CONTINUE specifies if you want to proceed to the
  --+ next entry or if you want to stop traversing. As long as you do not
  --+ modify its value within ACTION, its value remains TRUE.
  --+   Traversal by DISORDER_TRAVERSE_G is faster than by TRAVERSE_ASC_G or
  --+ TRAVERSE_DESC_G. Moreover, use of the generic procedure
  --+ DISORDER_TRAVERSE_G is recommended for saving a table in a backstore
  --+ (file or linear list) because recovery will be efficient.
  --+ REQUIREMENT:
  --+   For your actual procedure  ACTION, you must not use a procedure
  --+ which modifies the traversed table.

  generic
    with procedure Modify (Key: in Key_Type;
                           Value: in out Value_Type;
                           Order_Number: in Positive;
                           Continue: in out Boolean) is <>;
  procedure Disorder_Traverse_And_Update_Value_G (Table: in out Table_Type);
  --+ OVERVIEW:
  --+   The entries in TABLE are visited in disorder of their key values.
  --+ procedure MODIFY is applied on each entry within TABLE. The item
  --+ value of the current entry is then replaced by the new value.
  --+   ORDER_NUMBER gives the position of the visited entry in order of
  --+ traversal. The boolean CONTINUE specifies if you want to proceed to the
  --+ next entry or if you want to stop traversing. As long as you do not
  --+ modify its value within MODIFY, its value remains TRUE.
  --+   Traversal by DISORDER_TRAVERSE_G is faster than by TRAVERSE_ASC_G or
  --+ TRAVERSE_DESC_G.
  --+ REQUIREMENT:
  --+   For your actual procedure  MODIFY, you must not use a procedure
  --+ which modifies the traversed table.

--/ HEAP MANAGEMENT:

  procedure Destroy (Table: in out Table_Type);
  --+ OVERVIEW:
  --+   Empties the TABLE and returns space to the free list.

  procedure Release_Free_List;
  --+ OVERVIEW:
  --+   Releases all items from the free list giving their space back to the
  --+ system.


  procedure Set_Max_Free_List_Size (Max_Free_List_Size: in Natural);
  --+ OVERVIEW:
  --+   Sets the maximum length of the internal free list which is 0 by default.
  --+ If parameter MAX_FREE_LIST_SIZE is smaller than the current size
  --+ of the list, the items in excess are returned to the system.


  function Free_List_Size return Natural;
  --+ OVERVIEW:
  --+   Returns the actual length of the internal free list.

private

  type Equilibrium_Type is range -1..1;

  type Cell_Type;
  type Link_Type is access Cell_Type;
  type Cell_Type is
    record
      Balance: Equilibrium_Type := 0;
                                  -- always equal to HEIGHT(RIGHT)-HEIGHT(LEFT)
      Left: Link_Type;
      Right: Link_Type;-- Is also used for linking within the free list
      Key: Key_Type;
      Value: Value_Type;
    end record;

  type Table_Type is
  record
    Root: Link_Type;
    Count: Natural := 0;
    Connect_Predecessor: Boolean := True;
    -- Used to connect alternatively the predecessor or successor when deleting
    -- a node. Is provided for optimization.
  end record;
end Table_Of_Dynamic_Keys_And_Static_Values_G;
