
with unchecked_deallocation;

package body Lists is

    procedure Free is new unchecked_deallocation (Cell, List);

--------------------------------------------------------------------------

   function Last (L: in     List) return List is

       Place_In_L:        List;
       Temp_Place_In_L:   List;

   --|  Link down the list L and return the pointer to the last element
   --| of L.  If L is null raise the EmptyList exception.

   begin
       if L = null then
           raise EmptyList;
       else

           --|  Link down L saving the pointer to the previous element in
           --|  Temp_Place_In_L.  After the last iteration Temp_Place_In_L
           --|  points to the last element in the list.

           Place_In_L := L;
           while Place_In_L /= null loop
               Temp_Place_In_L := Place_In_L;
               Place_In_L := Place_In_L.Next;
           end loop;
           return Temp_Place_In_L;
       end if;
    end Last;


--------------------------------------------------------------------------

    procedure Attach (List1: in out List;
                      List2: in     List ) is
        EndOfList1: List;

    --| Attach List2 to List1.
    --| If List1 is null return List2
    --| If List1 equals List2 then raise CircularList
    --| Otherwise get the pointer to the last element of List1 and change
    --| its Next field to be List2.

    begin
        if List1 = null then
            List1 := List2;
            return;
        elsif List1 = List2 then
            raise CircularList;
        else
            EndOfList1 := Last (List1);
            EndOfList1.Next := List2;
        end if;
    end Attach;

--------------------------------------------------------------------------

   procedure Attach (L:       in out List;
                     Element: in     ItemType ) is

       NewEnd:    List;

   --| Create a list containing Element and attach it to the end of L

   begin
       NewEnd := new Cell'(Info => Element, Next => null);
       Attach (L, NewEnd);
   end;

--------------------------------------------------------------------------

   function Attach (Element1: in   ItemType;
                    Element2: in   ItemType ) return List is
       NewList: List;

   --| Create a new list containing the information in Element1 and
   --| attach Element2 to that list.

   begin
       NewList := new Cell'(Info => Element1, Next => null);
       Attach (NewList, Element2);
       return NewList;
   end;

--------------------------------------------------------------------------

   procedure Attach (Element: in     ItemType;
                     L:       in out List      ) is

   --|  Create a new cell whose information is Element and whose Next
   --|  field is the list L.  This prepends Element to the List L.

   begin
       L := new Cell'(Info => Element, Next => L);
   end;

--------------------------------------------------------------------------

   function Attach ( List1: in    List;
                     List2: in    List   ) return List is

   Last_Of_List1: List;

   begin
       if List1 = null then
           return List2;
       elsif List1 = List2 then
           raise CircularList;
       else
           Last_Of_List1 := Last (List1);
           Last_Of_List1.Next := List2;
           return List1;
       end if;
   end  Attach;

-------------------------------------------------------------------------

   function Attach( L:       in     List;
                    Element: in     ItemType ) return List is

   NewEnd: List;
   Last_Of_L: List;

   --| Create a list called NewEnd and attach it to the end of L.
   --| If L is null return NewEnd
   --| Otherwise get the last element in L and make its Next field
   --| NewEnd.

   begin
       NewEnd := new Cell'(Info => Element, Next => null);
       if L = null then
           return NewEnd;
       else
           Last_Of_L := Last (L);
           Last_Of_L.Next := NewEnd;
           return L;
       end if;
   end Attach;

--------------------------------------------------------------------------

   function Attach (Element: in     ItemType;
                    L:       in     List        ) return List is

   begin
       return (new Cell'(Info => Element, Next => L));
   end Attach;

---------------------------------------------------------------------------


   function Copy (L: in     List) return List is

   --| If L is null return null
   --| Otherwise recursively copy the list by first copying the information
   --| at the head of the list and then making the Next field point to
   --| a copy of the tail of the list.

   begin
       if L = null then
           return null;
       else
           return new Cell'(Info => L.Info, Next => Copy (L.Next));
       end if;
   end Copy;


--------------------------------------------------------------------------

   function CopyDeep (L: in List) return List is

   --|  If L is null then return null.
   --|  Otherwise copy the first element of the list into the head of the
   --|  new list and copy the tail of the list recursively using CopyDeep.

   begin
       if L = null then
           return null;
       else
           return new Cell'( Info => Copy (L.Info), Next => CopyDeep(L.Next));
       end if;
   end CopyDeep;

--------------------------------------------------------------------------

    function Create return List is

    --| Return the empty list.

    begin
        return null;
    end Create;

--------------------------------------------------------------------------
   procedure DeleteHead (L: in out List) is

       TempList: List;

   --| Remove the element of the head of the list and return it to the heap.
   --| If L is null EmptyList.
   --| Otherwise save the Next field of the first element, remove the first
   --| element and then assign to L the Next field of the first element.

   begin
       if L = null then
           raise EmptyList;
       else
           TempList := L.Next;
           Free (L);
           L := TempList;
       end if;
   end DeleteHead;

--------------------------------------------------------------------------

function DeleteItem(            --| remove the first occurrence of Element
                                --| from L
      L:       in     List;     --| list element is being  removed from
      Element: in     ItemType  --| element being removed
) return List is
    I       :List;
    Result  :List;
    Found   :boolean := false;
begin
    --| ALGORITHM
    --| Attach all elements of L to Result except the first element in L
    --| whose value is Element.  If the current element pointed to by I
    --| is not equal to element or the element being skipped was found
    --| then attach the current element to Result.

    I := L;
    while (I /= null) loop
        if (not Equal (I.Info, Element)) or (Found) then
            Attach (Result, I.Info);
        else
           Found := true;
        end if;
        I := I.Next;
    end loop;
    return Result;
end DeleteItem;

------------------------------------------------------------------------------

function DeleteItems (          --| remove all occurrences of Element
                                --| from  L.
      L:       in     List;     --| The List element is being removed from
      Element: in     ItemType  --| element being removed
) return List is
    I       :List;
    Result  :List;
begin
    --| ALGORITHM
    --| Walk over the list L and if the current element does not equal
    --| Element then attach it to the list to be returned.

    I := L;
    while I /= null loop
        if not Equal (I.Info, Element) then
            Attach (Result, I.Info);
        end if;
        I := I.Next;
    end loop;
    return Result;
end DeleteItems;

-------------------------------------------------------------------------------

   procedure DeleteItem (L:       in out List;
                         Element: in     ItemType ) is

       Temp_L  :List;

   --| Remove the first element in the list with the value Element.
   --| If the first element of the list is equal to element then
   --| remove it.  Otherwise, recurse on the tail of the list.

   begin
       if Equal(L.Info, Element) then
           DeleteHead(L);
       else
           DeleteItem(L.Next, Element);
       end if;
   end DeleteItem;

--------------------------------------------------------------------------

   procedure DeleteItems (L:       in out List;
                          Element: in     ItemType ) is

       Place_In_L       :List;     --| Current place in L.
       Last_Place_In_L  :List;     --| Last place in L.
       Temp_Place_In_L  :List;     --| Holds a place in L to be removed.

   --| Walk over the list removing all elements with the value Element.

   begin
       Place_In_L := L;
       Last_Place_In_L := null;
       while (Place_In_L /= null) loop
           --| Found an element equal to Element
           if Equal(Place_In_L.Info, Element) then
                --| If Last_Place_In_L is null then we are at first element
                --| in L.
                if Last_Place_In_L = null then
                     Temp_Place_In_L := Place_In_L;
                     L := Place_In_L.Next;
                else
                     Temp_Place_In_L := Place_In_L;

                     --| Relink the list Last's Next gets Place's Next

                     Last_Place_In_L.Next := Place_In_L.Next;
                end if;

                --| Move Place_In_L to the next position in the list.
                --| Free the element.
                --| Do not update the last element in the list it remains the
                --| same.

                Place_In_L := Place_In_L.Next;
                Free (Temp_Place_In_L);
           else
                --| Update the last place in L and the place in L.

                Last_Place_In_L := Place_In_L;
                Place_In_L := Place_In_L.Next;
           end if;
       end loop;

   --| If we have not found an element raise an exception.

   end DeleteItems;
------------------------------------------------------------------------------

   procedure Destroy (L: in out List) is

       Place_In_L:  List;
       HoldPlace:   List;

   --| Walk down the list removing all the elements and set the list to
   --| the empty list.

   begin
       Place_In_L := L;
       while Place_In_L /= null loop
           HoldPlace := Place_In_L;
           Place_In_L := Place_In_L.Next;
           Free (HoldPlace);
       end loop;
       L := null;
   end Destroy;

--------------------------------------------------------------------------

   procedure DestroyDeep (L: in out List) is

       Place_In_L:  List;
       HoldPlace:   List;

   --| Walk down the list removing all the elements and set the list to
   --| the empty list.

   begin
       Place_In_L := L;
       while Place_In_L /= null loop
           HoldPlace := Place_In_L;
           Place_In_L := Place_In_L.Next;
           Dispose (HoldPlace.Info);
           Free (HoldPlace);
       end loop;
       L := null;
   end DestroyDeep;

--------------------------------------------------------------------------

   function FirstValue (L: in    List) return ItemType is

   --| Return the first value in the list.

   begin
       if L = null then
           raise EmptyList;
       else
           return (L.Info);
       end if;
   end FirstValue;

--------------------------------------------------------------------------

   procedure Forward (I: in out ListIter) is

   --| Return the pointer to the next member of the list.

   begin
       if I = null then
           raise NoMore;
       else
           I := ListIter (I.Next);
       end if;
   end Forward;

--------------------------------------------------------------------------

   function IsInList (L:       in    List;
                      Element: in    ItemType  ) return boolean is

   Place_In_L: List;

   --| Check if Element is in L.  If it is return true otherwise return false.

   begin
       Place_In_L := L;
       while Place_In_L /= null loop
           if Equal(Place_In_L.Info, Element) then
               return true;
           end if;
           Place_In_L := Place_In_L.Next;
        end loop;
        return false;
   end IsInList;

--------------------------------------------------------------------------

    function IsEmpty (L: in     List) return boolean is

    --| Is the list L empty.

    begin
        return (L = null);
    end IsEmpty;

--------------------------------------------------------------------------

   function LastValue (L: in     List) return ItemType is

       LastElement: List;

   --| Return the value of the last element of the list. Get the pointer
   --| to the last element of L and then return its information.

   begin
       LastElement := Last (L);
       return LastElement.Info;
   end LastValue;

--------------------------------------------------------------------------

   function Length (L: in     List) return integer is

   --| Recursively compute the length of L.  The length of a list is
   --| 0 if it is null or  1 + the length of the tail.

   begin
       if L = null then
           return (0);
       else
           return (1 + Length (Tail (L)));
       end if;
   end Length;

--------------------------------------------------------------------------

   function MakeList (
          E :in     ItemType
   ) return List is

   begin
       return new Cell ' (Info => E, Next => null);
   end;

--------------------------------------------------------------------------
   function MakeListIter (L: in     List) return ListIter is

   --| Start an iteration operation on the list L.  Do a type conversion
   --| from List to ListIter.

   begin
       return ListIter (L);
   end MakeListIter;

--------------------------------------------------------------------------

   function More (L: in     ListIter) return boolean is

   --| This is a test to see whether an iteration is complete.

   begin
       return L /= null;
   end;

--------------------------------------------------------------------------

   procedure Next (Place:   in out ListIter;
                   Info:       out ItemType ) is
       PlaceInList: List;

   --| This procedure gets the information at the current place in the List
   --| and moves the ListIter to the next postion in the list.
   --| If we are at the end of a list then exception NoMore is raised.

   begin
       if Place = null then
          raise NoMore;
       else
          PlaceInList := List(Place);
          Info := PlaceInList.Info;
          Place := ListIter(PlaceInList.Next);
       end if;
   end Next;

--------------------------------------------------------------------------

   procedure ReplaceHead (L:    in out  List;
                          Info: in      ItemType ) is

   --| This procedure replaces the information at the head of a list
   --| with the given information. If the list is empty the exception
   --| EmptyList is raised.

   begin
       if L = null then
           raise EmptyList;
       else
           L.Info := Info;
       end if;
   end ReplaceHead;

--------------------------------------------------------------------------

   procedure ReplaceTail (L:        in out List;
                          NewTail:  in     List  ) is
       Temp_L: List;

   --| This destroys the tail of a list and replaces the tail with
   --| NewTail.  If L is empty EmptyList is raised.

   begin
       Destroy(L.Next);
       L.Next := NewTail;
   exception
       when constraint_error =>
           raise EmptyList;
   end ReplaceTail;

--------------------------------------------------------------------------

    function Tail (L: in    List) return List is

    --| This returns the list which is the tail of L.  If L is null
    --| EmptyList is raised.

    begin
        if L = null then
            raise EmptyList;
        else
            return L.Next;
        end if;
    end Tail;

--------------------------------------------------------------------------

    function CellValue (
           I :in ListIter
    ) return ItemType is
        L :List;
    begin
          -- Convert I to a List type and then return the value it points to.
        L := List(I);
        return L.Info;
    end CellValue;

--------------------------------------------------------------------------
    function Equal (List1: in    List;
                    List2: in    List ) return boolean is

        PlaceInList1: List;
        PlaceInList2: LIst;

    --| This function tests to see if two lists are equal.  Two lists
    --| are equal if for all the elements of List1 the corresponding
    --| element of List2 has the same value.  Thus if the 1st elements
    --| are equal and the second elements are equal and so up to n.
    --|  Thus a necessary condition for two lists to be equal is that
    --| they have the same number of elements.

    --| This function walks over the two list and checks that the
    --| corresponding elements are equal.  As soon as we reach
    --| the end of a list (PlaceInList = null) we fall out of the loop.
    --| If both PlaceInList1 and PlaceInList2 are null after exiting the loop
    --| then the lists are equal.  If they both are not null the lists aren't
    --| equal.  Note that equality on elements is based on a user supplied
    --| function Equal which is used to test for item equality.

    begin
        PlaceInList1 := List1;
        PlaceInList2 := List2;
        while   (PlaceInList1 /= null) and (PlaceInList2 /= null) loop
            if not Equal (PlaceInList1.Info, PlaceInList2.Info) then
                return false;
            end if;
            PlaceInList1 := PlaceInList1.Next;
            PlaceInList2 := PlaceInList2.Next;
        end loop;
        return ((PlaceInList1 = null) and (PlaceInList2 = null) );
    end Equal;
end Lists;
