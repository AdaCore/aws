--------------------------------- COPYRIGHT ------------------------------------
-- (C) 1987 Swiss Federal Institute of Technology (EPFL).                     --
--     Represented by A. Strohmeier EPFL-DI-LGL CH-1015 Lausanne Switzerland. --
--     All Rights Reserved.                                                   --
--------------------------------------------------------------------------------

--+ TITLE:      GENERIC PACKAGE FOR ASSOCIATIVE TABLES.
--+ REVISION:   13-JUL-1992 Ph. Kipfer (PKR), File header format
--+ APPROVAL:   03-DEC-1987 C. Genillard.
--+ CREATION:   29-JUN-1987 A. Strohmeier.

with Unchecked_Deallocation;
package body Table_Of_Dynamic_Keys_And_Static_Values_G is

--/ LOCAL SUBPROGRAM:
  procedure Assign_Item (Destination: out Value_Type;
                         Source: in Value_Type);
  procedure Assign (Destination: out Value_Type;
                    Source: in Value_Type) renames Assign_Item;

--/ LOCAL SUBPROGRAM:
  procedure Destroy_Item  (Item: in Value_Type);
  procedure Destroy (Item: in Value_Type) renames Destroy_Item;


  type Link_List_Type is array (Positive range <>) of Link_Type;

  Max_Free_List_Size: Natural := 0;

  type Free_List_Type is
    record
      Ptr: Link_Type;
      Count: Natural := 0;
    end record;

--/ STATE VARIABLE:
  Free_List: Free_List_Type;

--/ LOCAL SUBPROGRAM:
  procedure Create_And_Assign_Cell (Link: in out Link_Type;
                                    Key: in Key_Type;
                                    Value: in Value_Type) is
  -- LINK has in (out) mode for allowing access to LINK.VALUE.
  begin
    if Free_List.Count = 0 then
      Link := new Cell_Type;
    else
      Link := Free_List.Ptr;
      Free_List.Ptr := Free_List.Ptr.Right;
      Free_List.Count := Free_List.Count - 1;
      Link.Balance := 0;
      Link.Left := null;
      Link.Right := null;
    end if;
    Assign (Link.Key, Key);
    Assign (Link.Value, Value);
  end Create_And_Assign_Cell;
  pragma Inline (Create_And_Assign_Cell);

--/ LOCAL SUBPROGRAM:
  procedure Dispose is new Unchecked_Deallocation (Object => Cell_Type,
                                                   Name => Link_Type);
  pragma Inline (Dispose);

--/ LOCAL SUBPROGRAM:
  procedure Release (Link: in out Link_Type) is
  -- Collect in the free list, or release to system.
  begin
    Destroy (Link.Key);
    Destroy (Link.Value);
    if Free_List.Count < Max_Free_List_Size then
      Link.Right := Free_List.Ptr;
      Free_List.Ptr := Link;
      Free_List.Count := Free_List.Count + 1;
    else
      Dispose (Link);
    end if;
  end Release;
  pragma Inline (Release);

--/ LOCAL SUBPROGRAM:
  function Search_A_Key ( Root: in Link_Type; Key: in Key_Type) return Link_Type is
  -- Result points to the cell with key value searched for; when search
  -- fails, null value is returned.
    Ptr: Link_Type := Root;
  begin -- SEARCH_A_KEY
    while Ptr /= null loop
      if Less (Ptr.Key, Key) then
        Ptr := Ptr.Right;
      elsif Equals (Ptr.Key, Key) then
        return Ptr;
      else
        Ptr := Ptr.Left;
      end if;
    end loop;
    return null;
  end Search_A_Key;
  pragma Inline (Search_A_Key);

--/ LOCAL SUBPROGRAM:
  function Search_Min (Root: in Link_Type) return Link_Type is
  -- Result points to the first (smallest) cell in the table.
    Ptr: Link_Type := Root;
  begin
    if Ptr = null then
      return null;
    end if;
    while Ptr.Left /= null loop
      Ptr := Ptr.Left;
    end loop;
    return Ptr;
  end Search_Min;
  pragma Inline (Search_Min);

--/ LOCAL SUBPROGRAM:
  function Search_Max (Root: in Link_Type) return Link_Type is
  -- Result points to the last (greatest) cell in the table.
    Ptr: Link_Type := Root;
  begin
    if Ptr = null then
      return null;
    end if;
    while Ptr.Right /= null loop
      Ptr := Ptr.Right;
    end loop;
    return Ptr;
  end Search_Max;
  pragma Inline (Search_Max);


--/ CONSTRUCTORS:

  procedure Assign (Destination: in out Table_Type; Source: in Table_Type) is
    procedure Copy_Subtree (Destination: in out Link_Type;
                            Source: in Link_Type) is
    begin
      if Source /= null then
        Create_And_Assign_Cell (Destination, Source.Key, Source.Value);
        Destination.Balance := Source.Balance;
        Copy_Subtree (Destination.Left, Source.Left);
        Copy_Subtree (Destination.Right, Source.Right);
      else
        Destination := null;
      end if;
    end Copy_Subtree;
  begin -- ASSIGN
    if Source.Root = Destination.Root then return; end if;
    -- Actual parameters are identical tables.
    Destroy (Destination);
    if Source.Count = 0 then return; end if;
    -- SOURCE is a null table.
    Copy_Subtree (Destination.Root, Source.Root);
    Destination.Count := Source.Count;
  end Assign;

  procedure Insert (Table: in out Table_Type;
                    Key: in Key_Type;
                    Value: in Value_Type) is
    Duplicate_Item: Boolean;
  begin -- INSERT
    Insert (Table, Key, Value, Duplicate_Item);
    if Duplicate_Item then raise Duplicate_Item_Error; end if;
  end Insert;

  procedure Insert (Table: in out Table_Type;
                    Key: in Key_Type;
                    Value: in Value_Type;
                    Duplicate_Item: out Boolean) is
    Depth_Increased: Boolean := False;

    procedure Insert_Node (Key: in Key_Type;
                           Value: in Value_Type;
                           Subtree: in out Link_Type;
                           Depth_Increased: in out Boolean) is

      procedure Check_And_Balance_Left (Root: in out Link_Type;
                                        Depth_Increased: in out Boolean) is
      begin -- CHECK_AND_BALANCE_LEFT
        case Root.Balance is
          when 1 =>
            Root.Balance := 0;
            Depth_Increased := False;
          when 0 =>
            Root.Balance := -1;
          when -1 =>
            -- rebalance
            declare
              Left_Son: constant Link_Type := Root.Left;
            begin
              if Left_Son.Balance = -1 then
                -- single LL rotation
                Root.Left := Left_Son.Right;
                Left_Son.Right := Root;
                Root.Balance := 0;
                Root := Left_Son;
              else
                -- double LR rotation
                declare
                  Left_Right_Son: constant Link_Type := Left_Son.Right;
                begin
                  Left_Son.Right := Left_Right_Son.Left;
                  Left_Right_Son.Left := Left_Son;
                  Root.Left := Left_Right_Son.Right;
                  Left_Right_Son.Right := Root;
                  if Left_Right_Son.Balance = -1 then
                    Root.Balance := 1;
                  else
                    Root.Balance := 0;
                  end if;
                  if Left_Right_Son.Balance = 1 then
                    Left_Son.Balance := -1;
                  else
                    Left_Son.Balance := 0;
                  end if;
                  Root := Left_Right_Son;
                end;
              end if;
              Root.Balance := 0;
              Depth_Increased := False;
            end;
        end case;
      end Check_And_Balance_Left;

      procedure Check_And_Balance_Right (Root: in out Link_Type;
                                         Depth_Increased: in out Boolean) is
      begin -- CHECK_AND_BALANCE_RIGHT
        case Root.Balance is
          when -1 =>
            Root.Balance := 0;
            Depth_Increased := False;
          when 0 =>
            Root.Balance := 1;
          when 1 =>
            -- rebalance
            declare
              Right_Son: constant Link_Type := Root.Right;
            begin
              if Right_Son.Balance = 1 then
                -- single RR rotation
                Root.Right := Right_Son.Left;
                Right_Son.Left := Root;
                Root.Balance := 0;
                Root := Right_Son;
              else
                -- double RL rotation
                declare
                  Right_Left_Son: constant Link_Type := Right_Son.Left;
                begin
                  Right_Son.Left := Right_Left_Son.Right;
                  Right_Left_Son.Right := Right_Son;
                  Root.Right := Right_Left_Son.Left;
                  Right_Left_Son.Left := Root;
                  if Right_Left_Son.Balance = 1 then
                    Root.Balance := -1;
                  else
                    Root.Balance := 0;
                  end if;
                  if Right_Left_Son.Balance = -1 then
                    Right_Son.Balance := +1;
                  else
                    Right_Son.Balance := 0;
                  end if;
                  Root := Right_Left_Son;
                end;
              end if;
              Root.Balance := 0;
              Depth_Increased := False;
            end;
        end case;
      end Check_And_Balance_Right;

    begin -- INSERT_NODE
      if Subtree = null then
        Create_And_Assign_Cell (Subtree, Key, Value);
        Table.Count := Table.Count + 1;
        Depth_Increased := True;
        Duplicate_Item := False;
      else
        if Less (Key, Subtree.Key) then
          -- insert into left subtable
          Insert_Node (Key, Value, Subtree.Left, Depth_Increased);
          if Depth_Increased then
            Check_And_Balance_Left (Subtree, Depth_Increased);
          end if;
        elsif Equals (Key, Subtree.Key) then
          Depth_Increased := False;
          Duplicate_Item := True;
        else
          -- insert into right subtable
          Insert_Node (Key, Value, Subtree.Right, Depth_Increased);
          if Depth_Increased then
            Check_And_Balance_Right (Subtree, Depth_Increased);
          end if;
        end if;
      end if;
    end Insert_Node;

  begin -- INSERT
    Insert_Node (Key, Value, Table.Root, Depth_Increased);
  end Insert;

  procedure Insert_Or_Replace_Value (Table: in out Table_Type;
                                     Key: in Key_Type;
                                     Value: in Value_Type) is
    Ptr: constant Link_Type := Search_A_Key (Table.Root, Key);
    Junk: Boolean;
  begin
    if Ptr /= null then
      Assign (Ptr.Value, Value);
    else
      Insert (Table, Key, Value, Junk);
    end if;
  end Insert_Or_Replace_Value;

  procedure Replace_Value (Table: in out Table_Type;
                           Key: in Key_Type;
                           Value: in Value_Type;
                           Found: out Boolean) is
    Ptr: constant Link_Type := Search_A_Key (Table.Root, Key);
  begin
    if Ptr /= null then
      Assign (Ptr.Value, Value);
      Found := True;
    else
      Found := False;
    end if;
  end Replace_Value;

  procedure Replace_Value (Table: in out Table_Type;
                           Key: in Key_Type;
                           Value: in Value_Type) is
    Ptr: constant Link_Type := Search_A_Key (Table.Root, Key);
  begin
    if Ptr /= null then
      Assign (Ptr.Value, Value);
    else
      raise Missing_Item_Error;
    end if;
  end Replace_Value;

--/ LOCAL SUBPROGRAM:
--/ Primitives for balancing used by procedures that delete cells
  procedure Balance_Left (Root: in out Link_Type;
                          Depth_Reduced: in out Boolean) is
  begin -- BALANCE_LEFT
    case Root.Balance is
      when -1 =>
        Root.Balance := 0;
      when 0 =>
        Root.Balance := 1;
        Depth_Reduced := False;
      when 1 =>
        -- rebalance
        declare
          Right_Son: constant Link_Type := Root.Right;
          Right_Son_Balance: constant Equilibrium_Type := Right_Son.Balance;
        begin
          if Right_Son_Balance >= 0 then
            -- single RR rotation
            Root.Right := Right_Son.Left;
            Right_Son.Left := Root;
            if Right_Son_Balance = 0 then
              Root.Balance := 1;
              Right_Son.Balance := -1;
              Depth_Reduced := False;
            else
              Root.Balance := 0;
              Right_Son.Balance := 0;
            end if;
            Root := Right_Son;
          else
            -- double RL rotation
            declare
              Right_Left_Son: constant Link_Type := Right_Son.Left;
              Right_Left_Son_Balance: constant Equilibrium_Type
                                                     := Right_Left_Son.Balance;
            begin
              Right_Son.Left := Right_Left_Son.Right;
              Right_Left_Son.Right := Right_Son;
              Root.Right := Right_Left_Son.Left;
              Right_Left_Son.Left := Root;
              if Right_Left_Son_Balance = 1 then
                Root.Balance := -1;
              else
                Root.Balance := 0;
              end if;
              if Right_Left_Son_Balance = -1 then
                Right_Son.Balance := 1;
              else
                Right_Son.Balance := 0;
              end if;
              Root := Right_Left_Son;
              Right_Left_Son.Balance := 0;
            end;
          end if;
        end;
    end case;
  end Balance_Left;

--/ LOCAL SUBPROGRAM:
  procedure Balance_Right (Root: in out Link_Type;
                           Depth_Reduced: in out Boolean) is
  begin -- BALANCE_RIGHT
    case Root.Balance is
      when 1 =>
        Root.Balance := 0;
      when 0 =>
        Root.Balance := -1;
        Depth_Reduced := False;
      when -1 =>
        -- rebalance
        declare
          Left_Son: constant Link_Type := Root.Left;
          Left_Son_Balance: constant Equilibrium_Type := Left_Son.Balance;
        begin
          if Left_Son_Balance <= 0 then
            -- single LL rotation
            Root.Left := Left_Son.Right;
            Left_Son.Right := Root;
            if Left_Son_Balance = 0 then
              Root.Balance := -1;
              Left_Son.Balance := 1;
              Depth_Reduced := False;
            else
              Root.Balance := 0;
              Left_Son.Balance := 0;
            end if;
            Root := Left_Son;
          else
            -- double LR rotation
            declare
              Left_Right_Son: constant Link_Type := Left_Son.Right;
              Left_Right_Son_Balance: constant Equilibrium_Type
                                                     := Left_Right_Son.Balance;
            begin
              Left_Son.Right := Left_Right_Son.Left;
              Left_Right_Son.Left := Left_Son;
              Root.Left := Left_Right_Son.Right;
              Left_Right_Son.Right := Root;
              if Left_Right_Son_Balance = -1 then
                Root.Balance := 1;
              else
                Root.Balance := 0;
              end if;
              if Left_Right_Son_Balance = 1 then
                Left_Son.Balance := -1;
              else
                Left_Son.Balance := 0;
              end if;
              Root := Left_Right_Son;
              Left_Right_Son.Balance := 0;
            end;
          end if;
        end;
    end case;
  end Balance_Right;


  procedure Remove (Table: in out Table_Type; Key: in Key_Type) is
    Found: Boolean;
  begin -- REMOVE
    Remove (Table, Key, Found);
    if not Found then raise Missing_Item_Error; end if;
  end Remove;

  procedure Remove (Table: in out Table_Type;
                    Key: in Key_Type;
                    Value: out Value_Type) is
    Ptr: constant Link_Type := Search_A_Key (Table.Root, Key);
    Found: Boolean;
  begin -- REMOVE
    if Ptr = null then raise Missing_Item_Error; end if;
    Assign (Value, Ptr.Value);
    Remove (Table, Key, Found);
  end Remove;

  procedure Remove (Table: in out Table_Type;
                    Key: in Key_Type;
                    Found: out Boolean) is
    Depth_Decreased: Boolean := False;
    Temp: Link_Type;

    procedure Remove_Node (Root: in out Link_Type;
                           Depth_Reduced: in out Boolean) is

      procedure Delete_Biggest (Node: in out Link_Type;
                                Depth_Reduced: in out Boolean) is
      begin
        if Node.Right = null then
          -- NODE already points to the biggest key value.
          Assign (Root.Key, Node.Key);
          Assign (Root.Value, Node.Value);
          Temp := Node; -- in order to dispose the right cell
          Node := Node.Left;
          Depth_Reduced := True;
        else
          Delete_Biggest (Node.Right, Depth_Reduced);
          if Depth_Reduced then
            Balance_Right (Node, Depth_Reduced);
          end if;
        end if;
      end Delete_Biggest;

      procedure Delete_Smallest (Node: in out Link_Type;
                                 Depth_Reduced: in out Boolean) is
      begin
        if Node.Left = null then
          -- NODE already points to the smallest key value.
          Assign (Root.Key, Node.Key);
          Assign (Root.Value, Node.Value);
          Temp := Node; -- in order to dispose the right cell
          Node := Node.Right;
          Depth_Reduced := True;
        else
          Delete_Smallest (Node.Left, Depth_Reduced);
          if Depth_Reduced then
            Balance_Left (Node, Depth_Reduced);
          end if;
        end if;
      end Delete_Smallest;

    begin -- REMOVE_NODE
      if Root = null then
        return;
      end if;
      if Less (Key, Root.Key) then
        -- delete in left subtable
        Remove_Node (Root.Left, Depth_Reduced);
        if Depth_Reduced then
          Balance_Left (Root, Depth_Reduced);
        end if;
      elsif Less (Root.Key, Key) then
        -- delete in right subtable
        Remove_Node (Root.Right, Depth_Reduced);
        if Depth_Reduced then
          Balance_Right (Root, Depth_Reduced);
        end if;
      elsif Equals (Key, Root.Key) then
        if Root.Right = null then
          Temp := Root;
          Root := Root.Left;
          Depth_Reduced := True;
        elsif Root.Left = null then
          Temp := Root;
          Root := Root.Right;
          Depth_Reduced := True;
        else
          if Table.Connect_Predecessor then
            Table.Connect_Predecessor := False;
            Delete_Biggest (Root.Left, Depth_Reduced);
            if Depth_Reduced then
              Balance_Left (Root, Depth_Reduced);
            end if;
          else -- CONNECT_SUCCESSOR
            Table.Connect_Predecessor := True;
            Delete_Smallest (Root.Right, Depth_Reduced);
            if Depth_Reduced then
              Balance_Right (Root, Depth_Reduced);
            end if;
          end if;
        end if;
        Table.Count := Table.Count - 1;
        Found := True;
        Release (Temp);
      end if;
    end Remove_Node;

  begin -- REMOVE
    Found := False;
    Remove_Node (Table.Root, Depth_Decreased);
  end Remove;

  procedure Remove_Min (Table: in out Table_Type) is
    Found: Boolean;
    Ptr: constant Link_Type := Search_Min (Table.Root);
  begin
    if Table.Root = null then
      raise Empty_Structure_Error;
    end if;
    Remove (Table, Ptr.Key, Found);
  end Remove_Min;

  procedure Remove_Min (Table: in out Table_Type; Key: in out Key_Type) is
    Found: Boolean;
    Ptr: constant Link_Type := Search_Min (Table.Root);
  begin
    if Table.Root = null then
      raise Empty_Structure_Error;
    end if;
    Assign (Key, Ptr.Key);
    Remove (Table, Ptr.Key, Found);
  end Remove_Min;

  procedure Remove_Min (Table: in out Table_Type;
                        Key: in out Key_Type;
                        Value: out Value_Type) is
    Found: Boolean;
    Ptr: constant Link_Type := Search_Min (Table.Root);
  begin
    if Table.Root = null then
      raise Empty_Structure_Error;
    end if;
    Assign (Key, Ptr.Key);
    Assign (Value, Ptr.Value);
    Remove (Table, Ptr.Key, Found);
  end Remove_Min;

  procedure Remove_Max (Table: in out Table_Type) is
    Found: Boolean;
    Ptr: constant Link_Type := Search_Max (Table.Root);
  begin
    if Table.Root = null then
      raise Empty_Structure_Error;
    end if;
    Remove (Table, Ptr.Key, Found);
  end Remove_Max;

  procedure Remove_Max (Table: in out Table_Type; Key: in out Key_Type) is
    Found: Boolean;
    Ptr: constant Link_Type := Search_Max (Table.Root);
  begin
    if Table.Root = null then
      raise Empty_Structure_Error;
    end if;
    Assign (Key, Ptr.Key);
    Remove (Table, Ptr.Key, Found);
  end Remove_Max;

  procedure Remove_Max (Table: in out Table_Type;
                        Key: in out Key_Type;
                        Value: out Value_Type) is
    Found: Boolean;
    Ptr: constant Link_Type := Search_Max (Table.Root);
  begin
    if Table.Root = null then
      raise Empty_Structure_Error;
    end if;
    Assign (Key, Ptr.Key);
    Assign (Value, Ptr.Value);
    Remove (Table, Ptr.Key, Found);
  end Remove_Max;

  procedure Update_Value_Or_Exception_G (Table: in out Table_Type;
                                   Key: in Key_Type) is
     Link: constant Link_Type := Search_A_Key (Table.Root, Key);
  begin
    if Link = null then
      raise Missing_Item_Error;
    end if;
    Modify (Link.Key, Link.Value);
  end Update_Value_Or_Exception_G;

  procedure Update_Value_Or_Status_G (Table: in out Table_Type;
                                      Key: in Key_Type;
                                      Found: out Boolean) is
     Link: constant Link_Type := Search_A_Key (Table.Root, Key);
  begin
    if Link = null then
      Found := False;
      return;
    end if;
    Found := True;
    Modify (Link.Key, Link.Value);
  end Update_Value_Or_Status_G;

--/ QUERIES:

  function Size (Table: in Table_Type) return Natural is
  begin -- SIZE
    return Table.Count;
  end Size;

  function Is_Empty (Table: in Table_Type) return Boolean is
  begin -- IS_EMPTY
    return Table.Count = 0;
  end Is_Empty;

  function Is_Present (Table: in Table_Type; Key: in Key_Type) return Boolean is
  begin -- IS_PRESENT
    return Search_A_Key (Table.Root, Key) /= null;
  end Is_Present;

  function Value (Table: in Table_Type; Key: in Key_Type) return Value_Type is
    Ptr: constant Link_Type := Search_A_Key (Table.Root, Key);
  begin -- GET_VALUE
    if Ptr = null then
      raise Missing_Item_Error;
    end if;
    return Ptr.Value;
  end Value;

  procedure Get_Value (Table: in Table_Type;
                       Key: in Key_Type;
                       Value: out Value_Type) is
    Ptr: constant Link_Type := Search_A_Key (Table.Root, Key);
  begin -- GET_VALUE
    if Ptr = null then
      raise Missing_Item_Error;
    end if;
    Assign (Value, Ptr.Value);
  end Get_Value;

  procedure Get_Min_Item (Table: in Table_Type;
                           Key: in out Key_Type;
                           Value: out Value_Type) is
    Current: Link_Type := Table.Root;
  begin -- GET_MIN_ITEM
    if Current = null then
      raise Empty_Structure_Error;
    end if;
    while Current.Left /= null loop
      Current := Current.Left;
    end loop;
    Assign (Key, Current.Key);
    Assign (Value, Current.Value);
  end Get_Min_Item;

  procedure Get_Max_Item (Table: in Table_Type;
                           Key: in out Key_Type;
                           Value: out Value_Type) is
    Current: Link_Type := Table.Root;
  begin -- GET_MAX_ITEM
    if Current = null then
      raise Empty_Structure_Error;
    end if;
    while Current.Right /= null loop
      Current := Current.Right;
    end loop;
    Assign (Key, Current.Key);
    Assign (Value, Current.Value);
  end Get_Max_Item;

  function Min_Key (Table: in Table_Type) return Key_Type is
    Current: Link_Type := Table.Root;
  begin -- GET_MIN_KEY
    if Current = null then
      raise Empty_Structure_Error;
    end if;
    while Current.Left /= null loop
      Current := Current.Left;
    end loop;
    return Current.Key;
  end Min_Key;

  procedure Get_Min_Key (Table: in Table_Type;
                         Key: in out Key_Type) is
    Current: Link_Type := Table.Root;
  begin -- GET_MIN_KEY
    if Current = null then
      raise Empty_Structure_Error;
    end if;
    while Current.Left /= null loop
      Current := Current.Left;
    end loop;
    Assign (Key, Current.Key);
  end Get_Min_Key;

  function Max_Key (Table: in Table_Type) return Key_Type is
    Current: Link_Type := Table.Root;
  begin -- GET_MAX_KEY
    if Current = null then
      raise Empty_Structure_Error;
    end if;
    while Current.Right /= null loop
      Current := Current.Right;
    end loop;
    return Current.Key;
  end Max_Key;

  procedure Get_Max_Key (Table: in Table_Type;
                         Key: in out Key_Type) is
    Current: Link_Type := Table.Root;
  begin -- GET_MAX_KEY
    if Current = null then
      raise Empty_Structure_Error;
    end if;
    while Current.Right /= null loop
      Current := Current.Right;
    end loop;
    Assign (Key, Current.Key);
  end Get_Max_Key;

--/ LOCAL SUBPROGRAM:
  function Search_Less_Or_Equal (Root: in Link_Type;
                                 Key: in Key_Type) return Link_Type is
  -- Result points to the cell with key value less than or equal to KEY;
  -- when search fails, null value is returned.
    Ptr: Link_Type := Root;
    Best: Link_Type;
  begin
    if Ptr = null then
      return null;
    end if;
    loop
      if Less (Key, Ptr.Key) then
        if Ptr.Left = null then
          return Best;
        end if;
        Ptr := Ptr.Left;
      elsif Equals (Key, Ptr.Key) then
        return Ptr;
      else -- LESS (PTR.KEY, KEY)
        if Ptr.Right = null then
          return Ptr;
        end if;
        Best := Ptr;
        Ptr := Ptr.Right;
      end if;
    end loop;
  end Search_Less_Or_Equal;
  pragma Inline (Search_Less_Or_Equal);

--/ LOCAL SUBPROGRAM:
  function Search_Less (Root: in Link_Type;
                        Key: in Key_Type) return Link_Type is
  -- Result points to the cell with key value less than KEY; when search fails,
  -- null value is returned.
    Ptr: Link_Type := Root;
    Best: Link_Type;
  begin
    if Ptr = null then
      return null;
    end if;
    loop
      if Less (Key, Ptr.Key ) or Equals (Key, Ptr.Key) then
        if Ptr.Left = null then
          return Best;
        end if;
        Ptr := Ptr.Left;
      else -- LESS (PTR.KEY, KEY)
        if Ptr.Right = null then
          return Ptr;
        end if;
        Best := Ptr;
        Ptr := Ptr.Right;
      end if;
    end loop;
  end Search_Less;
  pragma Inline (Search_Less);

--/ LOCAL SUBPROGRAM:
  function Search_Greater_Or_Equal (Root: in Link_Type;
                                    Key: in Key_Type) return Link_Type is
  -- Result points to the cell with key value greater than or equal to KEY;
  -- when search fails, null value is returned.
    Ptr: Link_Type := Root;
    Best: Link_Type;
  begin
    if Ptr = null then
      return null;
    end if;
    loop
      if Less (Ptr.Key, Key) then
        if Ptr.Right = null then
          return Best;
        end if;
        Ptr := Ptr.Right;
      elsif Equals (Key, Ptr.Key) then
        return Ptr;
      else -- LESS (KEY, PTR.KEY)
        if Ptr.Left = null then
          return Ptr;
        end if;
        Best := Ptr;
        Ptr := Ptr.Left;
      end if;
    end loop;
  end Search_Greater_Or_Equal;
  pragma Inline (Search_Greater_Or_Equal);

--/ LOCAL SUBPROGRAM:
  function Search_Greater (Root: in Link_Type;
                           Key: in Key_Type) return Link_Type is
  -- Result points to the cell with key value greater than KEY; when search
  -- fails, null value is returned.
    Ptr: Link_Type := Root;
    Best: Link_Type;
  begin
    if Ptr = null then
      return null;
    end if;
    loop
      if Less (Ptr.Key, Key ) or Equals (Ptr.Key, Key) then
        if Ptr.Right = null then
          return Best;
        end if;
        Ptr := Ptr.Right;
      else -- LESS (KEY, PTR.KEY)
        if Ptr.Left = null then
          return Ptr;
        end if;
        Best := Ptr;
        Ptr := Ptr.Left;
      end if;
    end loop;
  end Search_Greater;
  pragma Inline (Search_Greater);


  procedure Get_Less_Item (Table: in Table_Type;
                            Key: in out Key_Type;
                            Value: out Value_Type) is
    Ptr: constant Link_Type := Search_Less (Table.Root, Key);
  begin
    if Ptr = null then
      raise Missing_Item_Error;
    end if;
    Assign (Key, Ptr.Key);
    Assign (Value, Ptr.Value);
  end Get_Less_Item;

  procedure Get_Less_Or_Equal_Item (Table: in Table_Type;
                                     Key: in out Key_Type;
                                     Value: out Value_Type) is
    Ptr: constant Link_Type := Search_Less_Or_Equal (Table.Root, Key);
  begin
    if Ptr = null then
      raise Missing_Item_Error;
    end if;
    Assign (Key, Ptr.Key);
    Assign (Value, Ptr.Value);
  end Get_Less_Or_Equal_Item;

  procedure Get_Greater_Item (Table: in Table_Type;
                               Key: in out Key_Type;
                               Value: out Value_Type) is
    Ptr: constant Link_Type := Search_Greater (Table.Root, Key);
  begin
    if Ptr = null then
      raise Missing_Item_Error;
    end if;
    Assign (Key, Ptr.Key);
    Assign (Value, Ptr.Value);
  end Get_Greater_Item;

  procedure Get_Greater_Or_Equal_Item (Table: in Table_Type;
                                        Key: in out Key_Type;
                                        Value: out Value_Type) is
    Ptr: constant Link_Type := Search_Greater_Or_Equal (Table.Root, Key);
  begin
    if Ptr = null then
      raise Missing_Item_Error;
    end if;
    Assign (Key, Ptr.Key);
    Assign (Value, Ptr.Value);
  end Get_Greater_Or_Equal_Item;

  function Less_Key (Table: in Table_Type; Key: in Key_Type) return Key_Type is
    Ptr: constant Link_Type := Search_Less (Table.Root, Key);
  begin
    if Ptr = null then
      raise Missing_Item_Error;
    end if;
    return Ptr.Key;
  end Less_Key;

  procedure Get_Less_Key (Table: in Table_Type;
                          Key: in out Key_Type) is
    Ptr: constant Link_Type := Search_Less (Table.Root, Key);
  begin
    if Ptr = null then
      raise Missing_Item_Error;
    end if;
    Assign (Key, Ptr.Key);
  end Get_Less_Key;

  procedure Get_Less_Key (Table: in Table_Type;
                          Key: in out Key_Type;
                          Found: out Boolean) is
    Ptr: constant Link_Type := Search_Less (Table.Root, Key);
  begin
    if Ptr = null then
      Found := False;
    else
      Found := True;
      Assign (Key, Ptr.Key);
    end if;
  end Get_Less_Key;

  function Less_Or_Equal_Key (Table: in Table_Type;
                              Key: in Key_Type) return Key_Type is
    Ptr: constant Link_Type := Search_Less_Or_Equal (Table.Root, Key);
  begin
    if Ptr = null then
      raise Missing_Item_Error;
    end if;
    return Ptr.Key;
  end Less_Or_Equal_Key;

  procedure Get_Less_Or_Equal_Key (Table: in Table_Type;
                                   Key: in out Key_Type) is
    Ptr: constant Link_Type := Search_Less_Or_Equal (Table.Root, Key);
  begin
    if Ptr = null then
      raise Missing_Item_Error;
    end if;
    Assign (Key, Ptr.Key);
  end Get_Less_Or_Equal_Key;

  procedure Get_Less_Or_Equal_Key (Table: in Table_Type;
                                   Key: in out Key_Type;
                                   Found: out Boolean) is
    Ptr: constant Link_Type := Search_Less_Or_Equal (Table.Root, Key);
  begin
    if Ptr = null then
      Found := False;
    else
      Found := True;
      Assign (Key, Ptr.Key);
    end if;
  end Get_Less_Or_Equal_Key;

  function Greater_Key (Table: in Table_Type; Key: in Key_Type) return Key_Type is
    Ptr: constant Link_Type := Search_Greater (Table.Root, Key);
  begin
    if Ptr = null then
      raise Missing_Item_Error;
    end if;
    return Ptr.Key;
  end Greater_Key;

  procedure Get_Greater_Key (Table: in Table_Type;
                             Key: in out Key_Type) is
    Ptr: constant Link_Type := Search_Greater (Table.Root, Key);
  begin
    if Ptr = null then
      raise Missing_Item_Error;
    end if;
    Assign (Key, Ptr.Key);
  end Get_Greater_Key;

  procedure Get_Greater_Key (Table: in Table_Type;
                             Key: in out Key_Type;
                             Found: out Boolean) is
    Ptr: constant Link_Type := Search_Greater (Table.Root, Key);
  begin
    if Ptr = null then
      Found := False;
    else
      Found :=  True;
      Assign (Key, Ptr.Key);
    end if;
  end Get_Greater_Key;

  function Greater_Or_Equal_Key (Table: in Table_Type;
                                 Key: in Key_Type) return  Key_Type is
    Ptr: constant Link_Type := Search_Greater_Or_Equal (Table.Root, Key);
  begin
    if Ptr = null then
      raise Missing_Item_Error;
    end if;
    return Ptr.Key;
  end Greater_Or_Equal_Key;

  procedure Get_Greater_Or_Equal_Key (Table: in Table_Type;
                                      Key: in out Key_Type) is
    Ptr: constant Link_Type := Search_Greater_Or_Equal (Table.Root, Key);
  begin
    if Ptr = null then
      raise Missing_Item_Error;
    end if;
    Assign (Key, Ptr.Key);
  end Get_Greater_Or_Equal_Key;

  procedure Get_Greater_Or_Equal_Key (Table: in Table_Type;
                                      Key: in out Key_Type;
                                      Found: out Boolean) is
    Ptr: constant Link_Type := Search_Greater_Or_Equal (Table.Root, Key);
  begin
    if Ptr = null then
      Found := False;
    else
      Found := True;
      Assign (Key, Ptr.Key);
    end if;
  end Get_Greater_Or_Equal_Key;

--/ SET OPERATIONS:

  package body Set_Operations_G is

  --/ LOCAL SUBPROGRAM:
  procedure Conditional_Union (Destination: in out Table_Type;
                               Source: in Table_Type) is
  -- All entries which are in SOURCE but not in DESTINATION are inserted into
  -- DESTINATION. DESTINATION and SOURCE must not access the same table.
     procedure Action (Key: in Key_Type;
                       Value: in Value_Type;
                       Order_Number: in Positive;
                       Continue: in out Boolean) is
       Dummy: Boolean;
     begin
       Insert (Destination, Key, Value, Dummy);
     end Action;
     procedure Traversal is new Disorder_Traverse_G (Action);
   begin
     Traversal (Source);
   end Conditional_Union;

   --/ LOCAL SUBPROGRAM:
   procedure Unconditional_Union (Destination: in out Table_Type;
                                  Source: in Table_Type) is
   -- All entries which are in SOURCE are inserted into DESTINATION or replace
   -- previous entries.
     procedure Action (Key: in Key_Type;
                       Value: in Value_Type;
                       Order_Number: in Positive;
                       Continue: in out Boolean) is
     begin
       Insert_Or_Replace_Value (Destination, Key, Value);
     end Action;
     procedure Traversal is new Disorder_Traverse_G (Action);
   begin
     Traversal (Source);
   end Unconditional_Union;

   procedure Union (Destination: in out Table_Type;
                    Left,
                    Right: in Table_Type) is
   begin
     if Left.Root = Right.Root then
       if Destination.Root = Left.Root then
         null;
       else
         Assign (Destination, Left);
       end if;
     elsif Destination.Root = Left.Root then
       Conditional_Union (Destination, Right);
     elsif Destination.Root = Right.Root then
       Unconditional_Union (Destination, Left);
     else
       Assign(Destination, Left);
       Conditional_Union (Destination, Right);
     end if;
   end Union;

   --/ LOCAL SUBPROGRAM:
   procedure Local_Intersection (Destination: in out Table_Type;
                                 Left,
                                 Right: in Table_Type) is
   -- DESTINATION must be an empty table. LEFT is traversed and each entry
   -- which is also in RIGHT is inserted into DESTINATION.

     procedure Action (Key: in Key_Type;
                       Value: in Value_Type;
                       Order_Number: in Positive;
                       Continue: in out Boolean) is
     begin
       if Is_Present (Right, Key) then
         Insert (Destination, Key, Value);
       end if;
     end Action;
     procedure Traversal is new Disorder_Traverse_G (Action);
   begin
     Traversal (Left);
   end Local_Intersection;

   procedure Intersection (Destination: in out Table_Type;
                           Left,
                           Right: in Table_Type) is
     Local_Table: Table_Type;
   begin
     if Left.Root = Right.Root then
       if Destination.Root = Left.Root then
         null;
       else
         Assign (Destination, Left);
       end if;
     elsif Destination.Root = Left.Root or Destination.Root = Right.Root then
       Local_Intersection (Local_Table, Left, Right);
       Assign (Destination, Local_Table);
       Destroy (Local_Table);
     else
       Destroy (Destination);
       Local_Intersection (Destination, Left, Right);
     end if;
   end Intersection;

   --/ LOCAL SUBPROGRAM:
   procedure Local_Difference (Destination: in out Table_Type;
                               Left,
                               Right: in Table_Type) is
   -- DESTINATION must be an empty table. LEFT is traversed and each entry
   -- which is not in RIGHT is inserted into DESTINATION.
     procedure Action (Key: in Key_Type;
                       Value: in Value_Type;
                       Order_Number: in Positive;
                       Continue: in out Boolean) is
     begin
       if not Is_Present (Right, Key) then
         Insert (Destination, Key, Value);
       end if;
     end Action;
     procedure Traversal is new Disorder_Traverse_G (Action);
   begin
     Traversal (Left);
   end Local_Difference;

   procedure Difference (Destination: in out Table_Type;
                         Left,
                         Right: in Table_Type) is
     Local_Table: Table_Type;
   begin
     if Left.Root = Right.Root then
       Destroy (Destination);
     elsif Destination.Root = Left.Root or Destination.Root = Right.Root then
       Local_Difference (Local_Table, Left, Right);
       Assign (Destination, Local_Table);
       Destroy (Local_Table);
     else
       Destroy (Destination);
       Local_Difference (Destination, Left, Right);
     end if;
   end Difference;

   --/ LOCAL SUBPROGRAM:
   procedure Local_Symmetric_Difference (Destination: in out Table_Type;
                                         Left,
                                         Right: in Table_Type) is
   -- DESTINATION must be an empty table. LEFT is traversed and each entry
   -- which is not in RIGHT is inserted into DESTINATION. Then RIGHT is
   -- traversed and each entry which is not in LEFT is inserted into
   -- DESTINATION.
     procedure Action_For_Left (Key: in Key_Type;
                                Value: in Value_Type;
                                Order_Number: in Positive;
                                Continue: in out Boolean) is
     begin
       if not Is_Present (Right, Key) then
         Insert (Destination, Key, Value);
       end if;
     end Action_For_Left;
     procedure Action_For_Right (Key: in Key_Type;
                                 Value: in Value_Type;
                                 Order_Number: in Positive;
                                 Continue: in out Boolean) is
     begin
       if not Is_Present (Left, Key) then
         Insert (Destination, Key, Value);
       end if;
     end Action_For_Right;
    procedure Traverse_Left is new Disorder_Traverse_G (Action_For_Left);
    procedure Traverse_Right is new Disorder_Traverse_G (Action_For_Right);
  begin
    Traverse_Left (Left);
    Traverse_Right (Right);
  end Local_Symmetric_Difference;

  procedure Symmetric_Difference (Destination: in out Table_Type;
                                  Left,
                                  Right: in Table_Type) is
    Local_Table: Table_Type;
  begin
    if Left.Root = Right.Root then
      Destroy (Destination);
    elsif Destination.Root = Left.Root or Destination.Root = Right.Root then
      Local_Symmetric_Difference (Local_Table, Left, Right);
      Assign (Destination, Local_Table);
      Destroy (Local_Table);
    else
      Destroy (Destination);
      Local_Symmetric_Difference (Destination, Left, Right);
    end if;
  end Symmetric_Difference;

  --/ LOCAL SUBPROGRAM:
    procedure Fill_List (Table: in Table_Type;
                         Link_List: in out Link_List_Type) is
    -- Fills LINK_LIST with pointers to the items of TABLE according to order
    -- defined on them.
    -- Condition: LINK_LIST'LAST = TABLE.COUNT
      Index: Natural := 0;
      procedure Traverse_Subtree (Link: in Link_Type) is
      -- LINK points to root of subtree.
      begin -- TRAVERSE_SUBTREE
        if Link /= null then
          Traverse_Subtree (Link.Left);
          Index := Index + 1;
          Link_List (Index) := Link;
          Traverse_Subtree (Link.Right);
        end if;
      end Traverse_Subtree;
    begin -- FILL_LIST
    -- if TABLE.COUNT /= LINK_LIST'LAST then
    --   raise CONSTRAINT_ERROR;
    -- end if;
    -- Statements provided for debugging.
       Traverse_Subtree (Table.Root);
    end Fill_List;

    function "=" (Left, Right: in Table_Type) return Boolean is
    -- Set equality; the LEFT and RIGHT tables contain entries with same values
      Left_Op_Link_List: Link_List_Type(1..Left.Count);
      Right_Op_Link_List: Link_List_Type(1..Right.Count);
    begin -- "="
      if Left.Root = Right.Root then
      -- LEFT and RIGHT points to the same table.
        return True;
      end if;
      if Left.Count /= Right.Count then
        return False;
      end if;
      if Left.Count = 0 then
      -- two empty tables
        return True;
      end if;
      Fill_List (Left, Left_Op_Link_List);
      Fill_List (Right, Right_Op_Link_List);
      for Index in 1..Left.Count loop
        if not Equals (Left_Op_Link_List(Index).Key,
                         Right_Op_Link_List(Index).Key) then
          return False;
        end if;
      end loop;
      return True;
    end "=";

    function "<" (Left, Right: in Table_Type) return Boolean is
    -- Strict set inclusion; to each entry in the LEFT table an entry with same
    -- value is associated in the RIGHT table, but the two sets are not
    -- identical.
      Left_Op_Link_List: Link_List_Type(1..Left.Count);
      Right_Op_Link_List: Link_List_Type(1..Right.Count);
      Found: Boolean;
      Right_Index: Positive := 1;
    begin -- "<"
      if Left.Count >= Right.Count then
      -- The case of identical sets is processed here.
        return False;
      end if;
      if Left.Count = 0 then
        return True;
      end if;
      Fill_List (Left, Left_Op_Link_List);
      Fill_List (Right, Right_Op_Link_List);
      for Left_Index in 1..Left.Count loop
        Found := False;
        while Right_Index <= Right.Count loop
          if Equals (Left_Op_Link_List (Left_Index).Key,
                       Right_Op_Link_List(Right_Index).Key) then
            Found := True;
            Right_Index := Right_Index + 1;
            exit; -- inner loop
          end if;
          Right_Index := Right_Index + 1;
        end loop;
        if not Found then
        -- item associated with LEFT_INDEX has not been found in RIGHT table.
          return False;
        end if;
      end loop;
      return True;
    end "<";

    function "<=" (Left, Right: in Table_Type) return Boolean is
    -- Strict set inclusion; to each entry in the LEFT table an entry with same
    -- key is associated in the RIGHT table.
      Left_Op_Link_List: Link_List_Type(1..Left.Count);
      Right_Op_Link_List: Link_List_Type(1..Right.Count);
      Found: Boolean;
      Right_Index: Positive := 1;
    begin -- "<="
      if Left.Root = Right.Root then
      -- LEFT and RIGHT points to the same table.
        return True;
      end if;
      if Left.Count > Right.Count then
        return False;
      end if;
      if Left.Count = 0 then
        return True;
      end if;
      Fill_List (Left, Left_Op_Link_List);
      Fill_List (Right, Right_Op_Link_List);
      for Left_Index in 1..Left.Count loop
        Found := False;
        while Right_Index <= Right.Count loop
          if Equals (Left_Op_Link_List (Left_Index).Key,
                       Right_Op_Link_List(Right_Index).Key) then
            Found := True;
            Right_Index := Right_Index + 1;
            exit; -- inner loop
          end if;
          Right_Index := Right_Index + 1;
        end loop;
        if not Found then
        -- item associated with LEFT_INDEX has not been found in RIGHT table.
          return False;
        end if;
      end loop;
      return True;
    end "<=";

    function ">" (Left, Right: in Table_Type) return Boolean is
    begin -- ">"
      return Right < Left;
    end ">";


    function ">=" (Left, Right: in Table_Type) return Boolean is
    begin -- ">="
      return Right <= Left;
    end ">=";

  end Set_Operations_G;

--/ ITERATORS:

  procedure Traverse_Asc_G (Table: in Table_Type) is
    Order_Number: Positive := 1;
    Continue: Boolean := True;
    procedure Traverse_Subtree (Link: in Link_Type) is
    begin
      if Link.Left /= null then
        Traverse_Subtree (Link.Left);
      end if;
      if Continue then
        Action (Link.Key, Link.Value, Order_Number, Continue);
        Order_Number := Order_Number + 1;
      end if;
      if Continue and then Link.Right /= null then
        Traverse_Subtree (Link.Right);
      end if;
    end Traverse_Subtree;
  begin -- TRAVERSE_ASC_G
    if Table.Root /= null then
      Traverse_Subtree (Table.Root);
    end if;
  end Traverse_Asc_G;

  procedure Traverse_Desc_G (Table: in Table_Type) is
    Order_Number: Positive := 1;
    Continue: Boolean := True;
    procedure Traverse_Subtree (Link: in Link_Type) is
    begin
      if Link.Right /= null then
        Traverse_Subtree (Link.Right );
      end if;
      if Continue then
        Action (Link.Key, Link.Value, Order_Number, Continue);
        Order_Number := Order_Number + 1;
      end if;
      if Continue and then Link.Left /= null then
        Traverse_Subtree (Link.Left);
      end if;
    end Traverse_Subtree;
  begin -- TRAVERSE_DESC_G
    if Table.Root /= null then
      Traverse_Subtree (Table.Root);
    end if;
  end Traverse_Desc_G;

  procedure Traverse_Asc_And_Update_Value_G (Table: in out Table_Type) is
    Order_Number: Positive := 1;
    Continue: Boolean := True;
    procedure Traverse_Subtree (Link: in Link_Type) is
    begin
      if Link.Left /= null then
        Traverse_Subtree (Link.Left);
      end if;
      if Continue then
        Modify (Link.Key, Link.Value, Order_Number, Continue);
        Order_Number := Order_Number + 1;
      end if;
      if Continue and then Link.Right /= null then
        Traverse_Subtree (Link.Right);
      end if;
    end Traverse_Subtree;
  begin -- TRAVERSE_ASC_AND_UPDATE_VALUE_G
    if Table.Root /= null then
      Traverse_Subtree (Table.Root);
    end if;
  end Traverse_Asc_And_Update_Value_G;

  procedure Traverse_Desc_And_Update_Value_G (Table: in out Table_Type) is
    Order_Number: Positive := 1;
    Continue: Boolean := True;
    procedure Traverse_Subtree (Link: in Link_Type) is
    begin
      if Link.Right /= null then
        Traverse_Subtree (Link.Right );
      end if;
      if Continue then
        Modify (Link.Key, Link.Value, Order_Number, Continue);
        Order_Number := Order_Number + 1;
      end if;
      if Continue and then Link.Left /= null then
        Traverse_Subtree (Link.Left );
      end if;
    end Traverse_Subtree;
  begin -- TRAVERSE_DESC_AND_UPDATE_VALUE_G
    if Table.Root /= null then
      Traverse_Subtree (Table.Root);
    end if;
  end Traverse_Desc_And_Update_Value_G;

  procedure Disorder_Traverse_G (Table: in Table_Type) is
    Current: Link_Type;
    Insert_Position: Positive;
    Link_List: Link_List_Type(1..Table.Count);
    Continue: Boolean := True;
  begin -- DISORDER_TRAVERSE_G
    if Table.Count = 0 then
      return;
    end if;
    Link_List(1) := Table.Root;
    Insert_Position := 2;
    for Order_Number in 1..Table.Count loop
      Current := Link_List(Order_Number);
      Action (Current.Key, Current.Value, Order_Number, Continue);
      if not Continue then
        exit;
      end if;
      if Current.Left /= null then
        Link_List (Insert_Position) := Current.Left;
        Insert_Position := Insert_Position + 1;
      end if;
      if Current.Right /= null then
        Link_List(Insert_Position) := Current.Right;
        Insert_Position := Insert_Position + 1;
      end if;
    end loop;
  end Disorder_Traverse_G;

  procedure Disorder_Traverse_And_Update_Value_G (Table: in out Table_Type) is
    Current: Link_Type;
    Insert_Position: Positive;
    Link_List: Link_List_Type(1..Table.Count);
    Continue: Boolean := True;
  begin -- DISORDER_TRAVERSE_AND_UPDATE_VALUE_G
    if Table.Count = 0 then
      return;
    end if;
    Link_List(1) := Table.Root;
    Insert_Position := 2;
    for Order_Number in 1..Table.Count loop
      Current := Link_List(Order_Number);
      Modify (Current.Key, Current.Value, Order_Number, Continue);
      if not Continue then
        exit;
      end if;
      if Current.Left /= null then
        Link_List (Insert_Position) := Current.Left;
        Insert_Position := Insert_Position + 1;
      end if;
      if Current.Right /= null then
        Link_List(Insert_Position) := Current.Right;
        Insert_Position := Insert_Position + 1;
      end if;
    end loop;
  end Disorder_Traverse_And_Update_Value_G;

--/ HEAP MANAGEMENT:

  procedure Destroy (Table: in out Table_Type) is
    Current: Link_Type;
    Insert_Position: Positive;
    Link_List: Link_List_Type(1..Table.Count);
  begin -- DESTROY
    if Table.Count = 0 then return; end if;
    -- May optimize.
    Link_List(1) := Table.Root;
    Insert_Position := 2;
    for Fetch_Position in 1..Table.Count loop
      Current := Link_List(Fetch_Position);
      if Current.Left /= null then
        Link_List(Insert_Position) := Current.Left;
        Insert_Position := Insert_Position + 1;
      end if;
      if Current.Right /= null then
        Link_List(Insert_Position) := Current.Right;
        Insert_Position := Insert_Position + 1;
      end if;
      Release (Current);
    end loop;
    Table := (null, 0, True);
  end Destroy;

  procedure Release_Free_List is
    Temp: Link_Type;
  begin
    while Free_List.Ptr /= null loop
      Temp := Free_List.Ptr;
      Free_List.Ptr := Free_List.Ptr.Right;
      Dispose (Temp);
    end loop;
    Free_List.Count := 0;
  end Release_Free_List;

  procedure Set_Max_Free_List_Size (Max_Free_List_Size: in Natural) is
    Nb_Of_Cells_For_System: Integer := Free_List.Count - Max_Free_List_Size;
    Temp: Link_Type;
  begin
    if Nb_Of_Cells_For_System > 0 then
      for I in 1..Nb_Of_Cells_For_System loop
        Temp := Free_List.Ptr;
        Free_List.Ptr := Free_List.Ptr.Right;
        Dispose (Temp);
      end loop;
      Free_List.Count := Free_List.Count - Nb_Of_Cells_For_System;
    end if;
    Table_Of_Dynamic_Keys_And_Static_Values_G.Max_Free_List_Size :=
                                                            Max_Free_List_Size;
  end Set_Max_Free_List_Size;

  function Free_List_Size return Natural is
  begin
    return Free_List.Count;
  end Free_List_Size;

  procedure Assign_Item (Destination: out Value_Type;
                         Source: in Value_Type) is
  begin
    Destination := Source;
  end Assign_Item ;

 procedure Destroy_Item (Item: in Value_Type) is
 -- Mode of the parameter is artificial, but mode 'out' could raise
 -- CONSTRAINT_ERROR !
 begin
   null;
 end Destroy_Item ;
  pragma Inline (Assign_Item );
 pragma Inline (Destroy_Item);


end Table_Of_Dynamic_Keys_And_Static_Values_G;
