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

with System;  use type System.Address;
with Ada.Unchecked_Deallocation;

package body AI302.Containers.Indefinite_Hashed_Maps is

   type Key_Access is access Key_Type;
   type Element_Access is access Element_Type;

   type Node_Type is
      record  -- make this limited in Ada 0Y
         Key     : Key_Access;
         Element : Element_Access;
         Next    : Node_Access;
      end record;


   function Hash_Node
     (Node : Node_Access) return Hash_Type is
   begin
      return Hash (Node.Key.all);
   end;

   function Next
     (Node : Node_Access) return Node_Access is
   begin
      return Node.Next;
   end;

   procedure Set_Next
     (Node : Node_Access;
      Next : Node_Access) is
   begin
      Node.Next := Next;
   end;

   function Is_Equal_Key_Node
     (Key  : Key_Type;
      Node : Node_Access) return Boolean is
   begin
      return Is_Equal_Key (Key, Node.Key.all);
   end;


   procedure Free_Key is
      new Ada.Unchecked_Deallocation (Key_Type, Key_Access);

   procedure Free_Element is
      new Ada.Unchecked_Deallocation (Element_Type, Element_Access);

   procedure Free (X : in out Node_Access) is

      procedure Deallocate is
         new Ada.Unchecked_Deallocation (Node_Type, Node_Access);

   begin

      --I don't think we have to check for null here, since it's
      --only called by the hash table when the node is non-null.

      Free_Key (X.Key);
      Free_Element (X.Element);

      Deallocate (X);

   end Free;



   function New_Node (Node : Node_Access) return Node_Access is

      K : Key_Access := new Key_Type'(Node.Key.all);
      E : Element_Access;

   begin

      E := new Element_Type'(Node.Element.all);

      return new Node_Type'(K, E, null);

   exception
      when others =>
         Free_Key (K);
         Free_Element (E);
         raise;

   end;


   procedure Adjust (Container : in out Map) is
   begin
      Adjust (Container.HT);
   end;


   procedure Finalize (Container : in out Map) is
   begin
      Finalize (Container.HT);
   end;


   function Find_Equal_Key
     (HT : Hash_Table_Type;
      N  : Node_Access) return Node_Access is

      pragma Inline (Find_Equal_Key);
   begin
      return Find (HT, Key => N.Key.all);
   end;

   function Is_Equal_Element_Node_Node
     (L, R : Node_Access) return Boolean is

      pragma Inline (Is_Equal_Element_Node_Node);

   begin

      --Let's liberalize the rules for holes here.
      --(Note that technically there can't be holes.)

      if L.Element = null then
         return R.Element = null;
      end if;

      if R.Element = null then
         return False;
      end if;

      return L.Element.all = R.Element.all;

   end Is_Equal_Element_Node_Node;

   function Is_Equal is
      new Hash_Table_Types.Generic_Equal
        (Find_Equal_Key   => Find_Equal_Key,
         Is_Equal_Element => Is_Equal_Element_Node_Node);

   function "=" (Left, Right : Map) return Boolean is
   begin

      if Left'Address = Right'Address then
         return True;
      end if;

      return Is_Equal (Left.HT, Right.HT);

   end "=";


   function Length (Container : Map) return Count_Type is
   begin
      return Container.HT.Length;
   end;


   function Is_Empty (Container : Map) return Boolean is
   begin
      return Length (Container) = 0;
   end;


   procedure Clear (Container : in out Map) is
   begin
      Clear (Container.HT);
   end;


   procedure Move
     (Target : in out Map;
      Source : in out Map) is
   begin
      Move (Target.HT, Source.HT);
   end;


   function Capacity (Container : Map) return Count_Type is
   begin
      if Container.HT.Buckets = null then
         return 0;
      end if;

      return Container.HT.Buckets'Length;
   end;


   procedure Set_Capacity (Container : in out Map;
                           Capacity  : in     Count_Type) is
   begin
      Resize (Container.HT, Capacity);
   end;


   procedure Insert (Container : in out Map;
                     Key       : in     Key_Type;
                     New_Item  : in     Element_Type;
                     Position  :    out Cursor;
                     Success   :    out Boolean) is

      function New_Node (Next : Node_Access) return Node_Access is

         K  : Key_Access := new Key_Type'(Key);
         E  : Element_Access;

      begin

         E := new Element_Type'(New_Item);

         return new Node_Type'(K, E, Next);

      exception
         when others =>
            Free_Key (K);
            Free_Element (E);

            raise;

      end New_Node;

      procedure Insert is
        new Hash_Table_Types.Generic_Conditional_Insert (New_Node);

      HT : Hash_Table_Type renames Container.HT;

   begin -- Insert

      Resize (HT, HT.Length + 1);
      Insert (HT, Key, Position.Node, Success);
      Position.Container := Container'Unchecked_Access;

   end Insert;


   procedure Replace (Container : in out Map;
                      Key       : in     Key_Type;
                      New_Item  : in     Element_Type) is

      Position : Cursor;
      Success : Boolean;

      X : Element_Access;

   begin

      Insert (Container, Key, New_Item, Position, Success);

      if not Success then

         X := Position.Node.Element;

         Position.Node.Element := new Element_Type'(New_Item);

         Free_Element (X);

      end if;

   end Replace;


   procedure Delete (Container : in out Map;
                     Key       : in     Key_Type) is

      HT : Hash_Table_Type renames Container.HT;

   begin

      if HT.Length = 0 then
         return;
      end if;

      Delete (HT, Key);

   end Delete;


   procedure Delete (Container : in out Map;
                     Position  : in out Cursor) is
   begin

      if Position.Container = null then
         return;
      end if;

      if Position.Node = null then
         return;
      end if;

      pragma Assert (Position.Container =
                       Map_Access'(Container'Unchecked_Access));

      Delete (Container.HT, Position.Node);

      Position := No_Element;

   end Delete;


   function Find (Container : Map;
                  Key       : Key_Type) return Cursor is

      HT : Hash_Table_Type renames Container.HT;

   begin

      if HT.Length = 0 then
         return No_Element;
      end if;

      declare
         Node : constant Node_Access := Find (HT, Key);
      begin
         if Node = null then
            return No_Element;
         end if;

         return (Container'Unchecked_Access, Node);
      end;

   end Find;


   function Is_In (Container : Map;
                   Key       : Key_Type) return Boolean is
   begin
      return Find (Container, Key) /= No_Element;
   end;


   function Element (Container : Map;
                     Key       : Key_Type)
     return Element_Type is

      C : constant Cursor := Find (Container, Key);
   begin
      return C.Node.Element.all;
   end;


   function First (Container : Map) return Cursor is
      Node : constant Node_Access := First (Container.HT);
   begin
      if Node = null then
         return No_Element;
      end if;

      return (Container'Unchecked_Access, Node);
   end;



   function Next (Position : Cursor) return Cursor is
   begin
      if Position.Container = null then
         return No_Element;
      end if;

      if Position.Node = null then
         return No_Element;
      end if;

      declare
         HT   : Hash_Table_Type renames Position.Container.HT;
         Node : constant Node_Access := Succ (HT, Position.Node);
      begin
         if Node = null then
            return No_Element;
         end if;

         return (Position.Container, Node);
      end;
   end Next;


   procedure Next (Position : in out Cursor) is
   begin
      Position := Next (Position);
   end;


   function Key (Position : Cursor) return Key_Type is
   begin
      return Position.Node.Key.all;
   end;


   function Is_Equal_Key (Left, Right : Cursor)
     return Boolean is
   begin
      return Is_Equal_Key (Left.Node.Key.all, Right.Node.Key.all);
   end;


   function Is_Equal_Key (Left  : Cursor;
                          Right : Key_Type)
     return Boolean is
   begin
      return Is_Equal_Key (Left.Node.Key.all, Right);
   end;


   function Is_Equal_Key (Left  : Key_Type;
                          Right : Cursor)
      return Boolean is
   begin
      return Is_Equal_Key (Left, Right.Node.Key.all);
   end;



   function Element (Position : Cursor)
      return Element_Type is
   begin
      return Position.Node.Element.all;
   end;


   procedure Generic_Update_Element (Position : in Cursor) is
   begin
      Process (Position.Node.Element.all);
   end;


   procedure Replace_Element (Position : in Cursor;
                              By       : in Element_Type) is

      X : Element_Access := Position.Node.Element;
   begin
      Position.Node.Element := new Element_Type'(By);
      Free_Element (X);
   end;


   procedure Generic_Iteration (Container : in Map) is

      procedure Process (Node : in Node_Access) is
         pragma Inline (Process);
      begin
         Process (Cursor'(Container'Unchecked_Access, Node));
      end;

      procedure Iterate is
         new Hash_Table_Types.Generic_Iteration (Process);

   begin -- Generic_Iteration

      Iterate (Container.HT);

   end Generic_Iteration;


   procedure Write
     (Stream : access Root_Stream_Type'Class;
      Node   : in     Node_Access) is
   begin
      Key_Type'Output (Stream, Node.Key.all);
      Element_Type'Output (Stream, Node.Element.all);
   end;

   procedure Write is
      new Hash_Table_Types.Generic_Write (Write);

   procedure Write
     (Stream    : access Root_Stream_Type'Class;
      Container : in     Map) is
   begin
      Write (Stream, Container.HT);
   end;


   function New_Node (Stream : access Root_Stream_Type'Class)
     return Node_Access is

      Node : Node_Access := new Node_Type;

   begin

      begin
         Node.Key := new Key_Type'(Key_Type'Input (Stream));
      exception
         when others =>
            Free (Node);
            raise;
      end;

      begin
         Node.Element := new Element_Type'(Element_Type'Input (Stream));
      exception
         when others =>
            Free_Key (Node.Key);
            Free (Node);
            raise;
      end;

      return Node;

   end New_Node;


   procedure Read is
      new Hash_Table_Types.Generic_Read (New_Node);

   procedure Read
     (Stream    : access Root_Stream_Type'Class;
      Container :    out Map) is
   begin
      Read (Stream, Container.HT);
   end;


   function Empty_Map return Map is
   begin
      return (Controlled with HT => (null, 0));
   end;


   function Has_Element (Position : Cursor) return Boolean is
   begin
      if Position.Container = null then
         return False;
      end if;

      if Position.Node = null then
         return False;
      end if;

      return True;
   end Has_Element;


end AI302.Containers.Indefinite_Hashed_Maps;

