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

with Ada.Streams;

generic

   type Key_Type (<>) is limited private;

   type Node_Access is private;

   Null_Node : in Node_Access;

   with function Hash_Key
     (Key : Key_Type) return Hash_Type is <>;

   with function Hash_Node
     (Node : Node_Access) return Hash_Type is <>;

   with function Next
     (Node : Node_Access) return Node_Access is <>;

   with procedure Set_Next
     (Node : Node_Access;
      Next : Node_Access) is <>;

   with function Is_Equal_Key_Node
     (Key  : Key_Type;
      Node : Node_Access) return Boolean is <>;

    with function New_Node
       (Node : Node_Access) return Node_Access is <>;

   with procedure Free
     (X : in out Node_Access) is <>;

package AI302.Containers.Hash_Tables is

   pragma Preelaborate;

   type Buckets_Type is array (Hash_Type range <>) of Node_Access;

   type Buckets_Access is access Buckets_Type;

   type Hash_Table_Type is
      record
         Buckets : Buckets_Access;
         Length  : Size_Type := 0;
      end record;

   pragma Volatile (Hash_Table_Type);  -- want by-ref: pragma ok?

   function "=" (L, R : Hash_Table_Type) return Boolean is abstract;

   procedure Adjust (HT : in out Hash_Table_Type);

   procedure Finalize (HT : in out Hash_Table_Type);


   generic

      with function Is_Equal
        (L, R : Node_Access) return Boolean is <>;

   function Generic_Equal
     (L, R : Hash_Table_Type) return Boolean;


   procedure Clear (HT : in out Hash_Table_Type);


   procedure Move (Target, Source : in out Hash_Table_Type);


   procedure Resize
     (HT : in out Hash_Table_Type;
      N  : in     Size_Type);


   generic

      with function New_Node
        (Next : Node_Access) return Node_Access is <>;

   procedure Generic_Conditional_Insert
     (HT      : in out Hash_Table_Type;
      Key     : in     Key_Type;
      Node    :    out Node_Access;
      Success :    out Boolean);


   procedure Delete
     (HT  : in out Hash_Table_Type;
      Key : in     Key_Type);


   procedure Delete
     (HT : in out Hash_Table_Type;
      X  : in out Node_Access);


   function Find (HT  : Hash_Table_Type;
                  Key : Key_Type) return Node_Access;

   function First (HT : Hash_Table_Type)
     return Node_Access;


   function Succ (HT   : Hash_Table_Type;
                  Node : Node_Access)
     return Node_Access;


   generic

      with procedure Process (Node : in Node_Access) is <>;

   procedure Generic_Iteration (HT : in Hash_Table_Type);


   generic

      with procedure Write
        (Stream : access Ada.Streams.Root_Stream_Type'Class;
         Node   : in     Node_Access) is <>;

   procedure Generic_Write
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      HT     : in     Hash_Table_Type);



   generic

      use Ada.Streams;

      with function New_Node (Stream : access Root_Stream_Type'Class)
         return Node_Access is <>;

   procedure Generic_Read
     (Stream : access Root_Stream_Type'Class;
      HT     :    out Hash_Table_Type);



end AI302.Containers.Hash_Tables;

