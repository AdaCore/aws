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

with AI302.Containers.Hash_Tables;
pragma Elaborate_All (AI302.Containers.Hash_Tables);

with Ada.Streams;
with Ada.Finalization;

generic

   type Key_Type (<>) is private;

   type Element_Type (<>) is private;

   with function Hash (Key : Key_Type)
      return Hash_Type is <>;

   with function Is_Equal_Key (Left, Right : Key_Type)
      return Boolean is "=";

   with function "=" (Left, Right : Element_Type)
      return Boolean is <>;

package AI302.Containers.Indefinite_Hashed_Maps is
   pragma Preelaborate (Indefinite_Hashed_Maps);

   subtype Key_Subtype is Key_Type;
   subtype Element_Subtype is Element_Type;

   type Map_Type is private;

   type Cursor_Type is private;

   Null_Cursor : constant Cursor_Type;

   function "=" (Left, Right : Map_Type) return Boolean;

   function Length (Map : Map_Type) return Size_Type;

   function Is_Empty (Map : Map_Type) return Boolean;

   procedure Clear (Map : in out Map_Type);

   procedure Swap (Left, Right : in out Map_Type);

   procedure Insert (Map      : in out Map_Type;
                     Key      : in     Key_Type;
                     New_Item : in     Element_Type;
                     Cursor   :    out Cursor_Type;
                     Success  :    out Boolean);

   procedure Replace (Map      : in out Map_Type;
                      Key      : in     Key_Type;
                      New_Item : in     Element_Type);

   procedure Insert (Map      : in out Map_Type;
                     Key      : in     Key_Type;
                     Cursor   :    out Cursor_Type;
                     Success  :    out Boolean);

   procedure Delete (Map : in out Map_Type;
                     Key : in     Key_Type);

   procedure Delete (Map    : in out Map_Type;
                     Cursor : in out Cursor_Type);

   function Is_In (Key : Key_Type;
                   Map : Map_Type)
      return Boolean;

   function Find (Map : Map_Type;
                  Key : Key_Type)
      return Cursor_Type;

   function Element (Map : Map_Type;
                     Key : Key_Type)
     return Element_Type;

   function Size (Map : Map_Type)
     return Size_Type;

   procedure Resize (Map  : in out Map_Type;
                     Size : in     Size_Type);

   function First (Map : Map_Type) return Cursor_Type;

   function Back (Map : Map_Type) return Cursor_Type;

   function Succ
     (Map    : Map_Type;
      Cursor : Cursor_Type) return Cursor_Type;

   procedure Increment
     (Map    : in     Map_Type;
      Cursor : in out Cursor_Type);

   function Key (Cursor : Cursor_Type) return Key_Type;

   generic
      type Key_Access is access constant Key_Type;
   function Generic_Key (Cursor : Cursor_Type)
      return Key_Access;

   function Is_Equal_Key (Left, Right : Cursor_Type)
     return Boolean;

   function Is_Equal_Key (Left  : Cursor_Type;
                          Right : Key_Type)
     return Boolean;

   function Is_Equal_Key (Left  : Key_Type;
                          Right : Cursor_Type)
     return Boolean;

   function Element (Cursor : Cursor_Type)
     return Element_Type;

   generic
      type Element_Access is access all Element_Type;
   function Generic_Element (Cursor : Cursor_Type)
      return Element_Access;

   procedure Replace_Element (Cursor : in Cursor_Type;
                              By     : in Element_Type);

   generic
      with procedure Process (Cursor : in Cursor_Type) is <>;
   procedure Generic_Iteration (Map : in Map_Type);

private

   type Node_Type;
   type Node_Access is access Node_Type;

   function Hash_Node
     (Node : Node_Access) return Hash_Type;
   pragma Inline (Hash_Node);

   function Next
     (Node : Node_Access) return Node_Access;
   pragma Inline (Next);

   procedure Set_Next
     (Node : Node_Access;
      Next : Node_Access);
   pragma Inline (Set_Next);

   function Is_Equal_Key_Node
     (Key  : Key_Type;
      Node : Node_Access) return Boolean;
   pragma Inline (Is_Equal_Key_Node);

   function New_Node
     (Node : Node_Access) return Node_Access;
   pragma Inline (New_Node);

   procedure Free (X : in out Node_Access);
   pragma Inline (Free);

   package Hash_Table_Types is
      new AI302.Containers.Hash_Tables
        (Key_Type,
         Node_Access,
         null,
         Hash_Key => Hash,
         Hash_Node => Hash_Node,
         Next => Next,
         Set_Next => Set_Next,
         Is_Equal_Key_Node => Is_Equal_Key_Node,
         New_Node => New_Node,
         Free => Free);

   use Hash_Table_Types;

   type Map_Type is new Ada.Finalization.Controlled with record
      HT : Hash_Table_Type;
   end record;

   procedure Adjust (Map : in out Map_Type);

   procedure Finalize (Map : in out Map_Type);

   type Cursor_Type is
      record
         Node : Node_Access;
      end record;

   Null_Cursor : constant Cursor_Type := (Node => null);


   use Ada.Streams;

   procedure Write
     (Stream : access Root_Stream_Type'Class;
      Map    : in     Map_Type);

   for Map_Type'Write use Write;


   procedure Read
     (Stream : access Root_Stream_Type'Class;
      Map    :    out Map_Type);

   for Map_Type'Read use Read;


end AI302.Containers.Indefinite_Hashed_Maps;



