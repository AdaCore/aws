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

   type Map is tagged private;

   type Cursor is private;

   function Empty_Map return Map;

   No_Element : constant Cursor;

   function "=" (Left, Right : Map) return Boolean;

   function Length (Container : Map) return Count_Type;

   function Is_Empty (Container : Map) return Boolean;

   procedure Clear (Container : in out Map);

   function Element (Position : Cursor)
     return Element_Type;

   generic
      with procedure Process (Element : in out Element_Type);
   procedure Generic_Update_Element (Position : in Cursor);

   procedure Replace_Element (Position : in Cursor;
                              By       : in Element_Type);

   procedure Move (Target : in out Map;
                   Source : in out Map);

   procedure Insert (Container : in out Map;
                     Key       : in     Key_Type;
                     New_Item  : in     Element_Type;
                     Position  :    out Cursor;
                     Success   :    out Boolean);

   procedure Replace (Container : in out Map;
                      Key       : in     Key_Type;
                      New_Item  : in     Element_Type);

   procedure Delete (Container : in out Map;
                     Key       : in     Key_Type);

   procedure Delete (Container : in out Map;
                     Position  : in out Cursor);

   function Is_In (Container : Map;
                   Key       : Key_Type)
      return Boolean;

   function Find (Container : Map;
                  Key       : Key_Type)
      return Cursor;

   function Element (Container : Map;
                     Key       : Key_Type)
     return Element_Type;

   function Capacity (Container : Map) return Count_Type;

   procedure Set_Capacity (Container : in out Map;
                           Capacity  : in     Count_Type);

   function First (Container : Map) return Cursor;

   function Next (Position : Cursor) return Cursor;

   procedure Next (Position : in out Cursor);

   function Has_Element (Position : Cursor) return Boolean;

   function Key (Position : Cursor) return Key_Type;

   function Is_Equal_Key (Left, Right : Cursor)
     return Boolean;

   function Is_Equal_Key (Left  : Cursor;
                          Right : Key_Type)
     return Boolean;

   function Is_Equal_Key (Left  : Key_Type;
                          Right : Cursor)
     return Boolean;

   generic
      with procedure Process (Position : in Cursor);
   procedure Generic_Iteration (Container : in Map);

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
   use Ada.Finalization;

   type Map is new Controlled with record
      HT : Hash_Table_Type;
   end record;

   procedure Adjust (Container : in out Map);

   procedure Finalize (Container : in out Map);

   type Map_Access is access constant Map;
   for Map_Access'Storage_Size use 0;

   type Cursor is
      record
         Container : Map_Access;
         Node      : Node_Access;
      end record;

   No_Element : constant Cursor :=
     (Container => null,
      Node      => null);


   use Ada.Streams;

   procedure Write
     (Stream    : access Root_Stream_Type'Class;
      Container : in     Map);

   for Map'Write use Write;


   procedure Read
     (Stream    : access Root_Stream_Type'Class;
      Container :    out Map);

   for Map'Read use Read;


end AI302.Containers.Indefinite_Hashed_Maps;



