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

with AI302.Containers.Red_Black_Trees;
pragma Elaborate_All (AI302.Containers.Red_Black_Trees);

with Ada.Finalization;
with Ada.Streams;

generic

   type Element_Type (<>) is private;

   with function "<" (Left, Right : Element_Type)
      return Boolean is <>;

   with function "=" (Left, Right : Element_Type)
      return Boolean is <>;

package AI302.Containers.Indefinite_Ordered_Sets is
   pragma Preelaborate (Indefinite_Ordered_Sets);

   type Set is private;

   type Cursor is private;

   function Empty_Set return Set;
   --NOTE: In the API, this is a deferred constant.  But that's
   --not possible until we have an Ada 0Y compiler.

   No_Element : constant Cursor;

   function "=" (Left, Right : Set) return Boolean;

   function Length (Container : Set) return Size_Type;

   function Is_Empty (Container : Set) return Boolean;

   procedure Clear (Container : in out Set);

   function Element (Position : Cursor) return Element_Type;

   generic
      with procedure Process (Element : in out Element_Type) is <>;
   procedure Generic_Update (Position : in Cursor);

   procedure Move (Target : in out Set;
                   Source : in out Set);

   procedure Insert (Container : in out Set;
                     New_Item  : in     Element_Type;
                     Position  :    out Cursor;
                     Success   :    out Boolean);

   --NOTE:
   --A nice function might be:
   --procedure Insert (Container : in out Set;
   --                  New_Item  : in     Element_Type);
   --This is a convenience function that omits the last two params.
   --END NOTE.


   procedure Delete (Container : in out Set;
                     Item      : in     Element_Type);

   procedure Delete (Container : in out Set;
                     Position  : in out Cursor);

   procedure Delete_First (Container : in out Set);

   procedure Delete_Last (Container : in out Set);


   procedure Union (Target : in out Set;
                    Source : in     Set);

   function Union (Left, Right : Set) return Set;

   function "or" (Left, Right : Set) return Set renames Union;

   procedure Intersection (Target : in out Set;
                           Source : in     Set);

   function Intersection (Left, Right : Set) return Set;

   function "and" (Left, Right : Set) return Set renames Intersection;

   procedure Difference (Target : in out Set;
                         Source : in     Set);

   function Difference (Left, Right : Set) return Set;

   function "-" (Left, Right : Set) return Set renames Difference;

   procedure Symmetric_Difference (Target : in out Set;
                                   Source : in     Set);

   function Symmetric_Difference (Left, Right : Set) return Set;

   function "xor" (Left, Right : Set) return Set renames Symmetric_Difference;

   function Is_Subset (Item      : Set;
                       Container : Set)
      return Boolean;

   function Is_Disjoint (Item      : Set;
                         Container : Set)
      return Boolean;

   function Is_In (Item      : Element_Type;
                   Container : Set) return Boolean;

   function Find (Container : Set;
                  Item      : Element_Type)
      return Cursor;

--     function Lower_Bound (Container  : Set;
--                           Item : Element_Type)
--        return Cursor;

--     function Upper_Bound (Container  : Set;
--                           Item : Element_Type) return Cursor;

   function First (Container : Set) return Cursor;

   function First_Element (Container : Set) return Element_Type;

   function Last (Container : Set) return Cursor;

   function Last_Element (Container : Set) return Element_Type;

   function Next (Position : Cursor) return Cursor;

   function Previous (Position : Cursor) return Cursor;

   procedure Next (Position : in out Cursor);

   procedure Previous (Position : in out Cursor);

   function Has_Element (Position : Cursor) return Boolean;

   function "<" (Left, Right : Cursor) return Boolean;

   function ">" (Left, Right : Cursor) return Boolean;

   function "<" (Left : Cursor; Right : Element_Type)
      return Boolean;

   function ">" (Left : Cursor; Right : Element_Type)
      return Boolean;

   function "<" (Left : Element_Type; Right : Cursor)
      return Boolean;

   function ">" (Left : Element_Type; Right : Cursor)
      return Boolean;

   generic
      with procedure Process (Position : in Cursor) is <>;
   procedure Generic_Iteration (Container : in Set);

   generic
      with procedure Process (Position : in Cursor) is <>;
   procedure Generic_Reverse_Iteration (Container : in Set);


   generic

      type Key_Type (<>) is limited private;

      with function "<" (Left : Key_Type; Right : Element_Type)
          return Boolean is <>;

      with function ">" (Left : Key_Type; Right : Element_Type)
          return Boolean is <>;

   package Generic_Keys is

      function Is_In (Key       : Key_Type;
                      Container : Set)
         return Boolean;

      function Find (Container : Set;
                     Key       : Key_Type)
        return Cursor;

      function Element (Container : Set;
                        Key       : Key_Type)
        return Element_Type;

--        function Lower_Bound (Container : Set;
--                              Key : Key_Type)
--          return Cursor;

--        function Upper_Bound (Container : Set;
--                              Key : Key_Type)
--          return Cursor;

      procedure Delete (Container : in out Set;
                        Key       : in     Key_Type);

      function "<" (Left : Cursor; Right : Key_Type)
        return Boolean;

      function ">" (Left : Cursor; Right : Key_Type)
        return Boolean;

      function "<" (Left : Key_Type; Right : Cursor)
        return Boolean;

      function ">" (Left : Key_Type; Right : Cursor)
        return Boolean;

      generic

         with function New_Element (Key : Key_Type)
            return Element_Type is <>;

      procedure Generic_Insertion (Container : in out Set;
                                   Key       : in     Key_Type;
                                   Position  :    out Cursor;
                                   Success   :    out Boolean);

   end Generic_Keys;

private

   type Color_Type is (Red, Black, White);

   type Node_Type;
   type Node_Access is access Node_Type;

   function Parent (Node : Node_Access)
      return Node_Access;
   pragma Inline (Parent);

   function Left (Node : Node_Access)
      return Node_Access;
   pragma Inline (Left);

   function Right (Node : Node_Access)
      return Node_Access;
   pragma Inline (Right);

   function Color (Node : Node_Access)
      return Color_Type;
   pragma Inline (Color);

   procedure Set_Parent
     (Node   : Node_Access;
      Parent : Node_Access);
   pragma Inline (Set_Parent);

   procedure Set_Left
     (Node : Node_Access;
      Left : Node_Access);
   pragma Inline (Set_Left);

   procedure Set_Right
     (Node  : Node_Access;
      Right : Node_Access);
   pragma Inline (Set_Right);

   procedure Set_Color
     (Node  : Node_Access;
      Color : Color_Type);
   pragma Inline (Set_Color);


   package Tree_Types is
      new Red_Black_Trees
        (Node_Access => Node_Access,
         Color_Type  => Color_Type,
         Null_Node   => null,
         Red         => Red,
         Black       => Black,
         White       => White);

   use Tree_Types;


   function New_Back return Node_Access;

   use Ada.Finalization;

   type Set is new Controlled with record
      Tree : Tree_Type := (Back => New_Back, Length => 0);
   end record;


   procedure Adjust (Container : in out Set);

   procedure Finalize (Container : in out Set);


   type Cursor is new Node_Access;

   No_Element : constant Cursor := null;


   use Ada.Streams;

   procedure Write
     (Stream    : access Root_Stream_Type'Class;
      Container : in     Set);

   for Set'Write use Write;


   procedure Read
     (Stream    : access Root_Stream_Type'Class;
      Container :    out Set);

   for Set'Read use Read;


end AI302.Containers.Indefinite_Ordered_Sets;


