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

with AI302.Containers.Prime_Numbers;
with Ada.Unchecked_Deallocation;

package body AI302.Containers.Hash_Tables is

   procedure Free is
      new Ada.Unchecked_Deallocation (Buckets_Type, Buckets_Access);


   function Index
     (Buckets : Buckets_Type;
      Node    : Node_Access) return Hash_Type is

      pragma Inline (Index);
   begin
      return Hash_Node (Node) mod Buckets'Length;
   end;


   function Index
     (Hash_Table : Hash_Table_Type;
      Node       : Node_Access) return Hash_Type is

      pragma Inline (Index);
   begin
      return Index (Hash_Table.Buckets.all, Node);
   end;


   function Index
     (Hash_Table : Hash_Table_Type;
      Key        : Key_Type) return Hash_Type is

      pragma Inline (Index);
   begin
      return Hash_Key (Key) mod Hash_Table.Buckets'Length;
   end;


   procedure Adjust (HT : in out Hash_Table_Type) is

      Src_Buckets : constant Buckets_Access := HT.Buckets;

      N : constant Size_Type := HT.Length;

      Src_Node, Dst_Prev : Node_Access;

   begin

      HT.Buckets := null;
      HT.Length := 0;

      if N = 0 then
         return;
      end if;

      HT.Buckets := new Buckets_Type (Src_Buckets'Range);
      --
      --NOTE:
      --I suppose we have to duplicate the Size (Src), too, in order
      --to guarantee that
      --    Dst := Src;
      --    Dst = Src is true
      --The only quirk is that we depend on the hash value of a dst key
      --to be the same as the src key from which it was copied.
      --If we relax the requirement that the hash value must be the
      --same, then of course we can't guarantee that following
      --assignment that Dst = Src is true.
      --END NOTE.

      for Src_Index in Src_Buckets'Range loop

         Src_Node := Src_Buckets (Src_Index);

         if Src_Node /= Null_Node then  -- I wish Ada had a continue ...

            declare
               Dst_Node : constant Node_Access := New_Node (Src_Node);

               -- See note above.
               pragma Assert (Index (HT, Dst_Node) = Src_Index);
            begin
               HT.Buckets (Src_Index) := Dst_Node;
               HT.Length := HT.Length + 1;

               Dst_Prev := Dst_Node;
            end;

            Src_Node := Next (Src_Node);

            while Src_Node /= Null_Node loop

               declare
                  Dst_Node : constant Node_Access := New_Node (Src_Node);

                  -- See note above.
                  pragma Assert (Index (HT, Dst_Node) = Src_Index);
               begin
                  Set_Next (Node => Dst_Prev, Next => Dst_Node);
                  HT.Length := HT.Length + 1;

                  Dst_Prev := Dst_Node;
               end;

               Src_Node := Next (Src_Node);

            end loop;

         end if;

      end loop;

      pragma Assert (HT.Length = N);

   end Adjust;


   procedure Finalize (HT : in out Hash_Table_Type) is

      X : Node_Access;

   begin

      if HT.Buckets = null then
         return;
      end if;

      for I in HT.Buckets'Range loop

         declare
            B : Node_Access renames HT.Buckets (I);
         begin

            while B /= Null_Node loop

               X := B;
               B := Next (B);

               pragma Assert (HT.Length > 0);
               HT.Length := HT.Length - 1;

               Free (X);

            end loop;

         end;

      end loop;

      Free (HT.Buckets);

   end Finalize;


   function Generic_Equal
     (L, R : Hash_Table_Type) return Boolean is

      L_Index, R_Index : Hash_Type;
      L_Node, R_Node   : Node_Access;

      I : Size_Type;

   begin

      if L.Length /= R.Length then
         return False;
      end if;

      if L.Length = 0 then
         return True;
      end if;

      L_Index := 0;

      loop
         L_Node := L.Buckets (L_Index);
         exit when L_Node /= Null_Node;
         L_Index := L_Index + 1;
      end loop;

      R_Index := 0;

      loop
         R_Node := R.Buckets (R_Index);
         exit when R_Node /= Null_Node;
         R_Index := R_Index + 1;
      end loop;

      I := L.Length;

      loop

         if not Is_Equal (L_Node, R_Node) then
            return False;
         end if;

         I := I - 1;

         if I = 0 then
            return True;
         end if;

         L_Node := Next (L_Node);
         R_Node := Next (R_Node);

         while L_Node = Null_Node loop
            L_Index := L_Index + 1;
            L_Node := L.Buckets (L_Index);
         end loop;

         while R_Node = Null_Node loop
            R_Index := R_Index + 1;
            R_Node := R.Buckets (R_Index);
         end loop;

      end loop;

   end Generic_Equal;


   procedure Clear (HT : in out Hash_Table_Type) is

      Index : Hash_Type := 0;
      Node  : Node_Access;

   begin

      while HT.Length > 0 loop

         while HT.Buckets (Index) = Null_Node loop
            Index := Index + 1;
         end loop;

         declare
            Bucket : Node_Access renames HT.Buckets (Index);
         begin
            loop

               Node := Bucket;

               Bucket := Next (Bucket);

               HT.Length := HT.Length - 1;

               Free (Node);

               exit when Bucket = Null_Node;

            end loop;
         end;

      end loop;

   end Clear;


   procedure Swap (L, R : in out Hash_Table_Type) is

      LB : constant Buckets_Access := L.Buckets;
      LL : constant Size_Type := L.Length;

   begin

      L.Buckets := R.Buckets;
      L.Length := R.Length;

      R.Buckets := LB;
      R.Length := LL;

   end Swap;



   procedure Rehash
     (HT   : in out Hash_Table_Type;
      Size : in     Hash_Type) is

      subtype Buckets_Range is
        Hash_Type range 0 .. Size - 1;

      Dst_Buckets : Buckets_Access :=
        new Buckets_Type (Buckets_Range);

      Src_Buckets : Buckets_Access := HT.Buckets;

      L : Size_Type renames HT.Length;
      LL : constant Size_Type := L;

   begin

      if Src_Buckets = null then
         pragma Assert (L = 0);
         HT.Buckets := Dst_Buckets;
         return;
      end if;

      if L = 0 then
         HT.Buckets := Dst_Buckets;
         Free (Src_Buckets);
         return;
      end if;

      --we might want to change this to iter from 1 .. L instead
      --
      for Src_Index in Src_Buckets'Range loop

         declare
            Src_Bucket : Node_Access renames Src_Buckets (Src_Index);
         begin

            while Src_Bucket /= Null_Node loop

               declare
                  Src_Node : constant Node_Access := Src_Bucket;

                  Dst_Index : constant Hash_Type :=
                    Index (Dst_Buckets.all, Src_Node);

                  Dst_Bucket : Node_Access renames Dst_Buckets (Dst_Index);
               begin
                  Src_Bucket := Next (Src_Node);

                  Set_Next (Src_Node, Dst_Bucket);

                  Dst_Bucket := Src_Node;
               end;

               pragma Assert (L > 0);
               L := L - 1;

            end loop;

         exception
            when others =>

-- I don't think we can deallocate the nodes,
-- because they may be designated by outstanding
-- iterators.  Which means they're now lost...
--
--                 for J in NB'Range loop
--
--                    declare
--                       Dst : Node_Access renames NB (J);
--                       X   : Node_Access;
--                    begin
--                       while Dst /= Null_Node loop
--                          X := Dst;
--                          Dst := Succ (Dst);
--                          Free (X);
--                       end loop;
--                    end;
--
--                 end loop;


               Free (Dst_Buckets);
               raise;

         end;

         --exit when L = 0;
         --need to bother?

      end loop;

      pragma Assert (L = 0);

      HT.Buckets := Dst_Buckets;
      HT.Length := LL;

      Free (Src_Buckets);

   end Rehash;


   procedure Resize
     (HT : in out Hash_Table_Type;
      N  : in     Size_Type) is

      NN : Hash_Type;

   begin

      if N = 0 then
         return;
      end if;

      if HT.Buckets /= null
        and then HT.Buckets'Length >= N
      then
         return;
      end if;

      NN := Prime_Numbers.To_Prime (N);

      if NN <= Hash_Type (N) then  --?
         return;
      end if;

      Rehash (HT, NN);

   end Resize;


   procedure Generic_Conditional_Insert
     (HT      : in out Hash_Table_Type;
      Key     : in     Key_Type;
      Node    :    out Node_Access;
      Success :    out Boolean) is

      I : constant Hash_Type := Index (HT, Key);

      B : Node_Access renames HT.Buckets (I);

      subtype Length_Subtype is Size_Type
        range 0 .. Size_Type'Last - 1;

   begin

      if B = Null_Node then

         declare
            Length : constant Length_Subtype := HT.Length;
         begin
            Node := New_Node (Next => Null_Node);
            Success := True;

            B := Node;
            HT.Length := Length + 1;
         end;

         return;

      end if;

      Node := B;

      loop

         if Is_Equal_Key_Node (Key, Node) then
            Success := False;
            return;
         end if;

         Node := Next (Node);

         exit when Node = Null_Node;

      end loop;

      declare
         Length : constant Length_Subtype := HT.Length;
      begin
         Node := New_Node (Next => B);
         Success := True;

         B := Node;
         HT.Length := Length + 1;
      end;

   end Generic_Conditional_Insert;


   procedure Delete
     (HT  : in out Hash_Table_Type;
      Key : in     Key_Type) is

      L : Size_Type renames HT.Length;
      pragma Assert (L > 0); --NOTE: precondition for caller

      I : constant Hash_Type := Index (HT, Key);

      B : Node_Access renames HT.Buckets (I);

      Prev, X : Node_Access;

   begin

      if B = Null_Node then
         return;
      end if;

      if Is_Equal_Key_Node (Key, B) then

         --handle matching first node in bucket as special case

         X := B;

         B := Next (B);

         --pragma Assert (L > 0);
         L := L - 1;

         Free (X);

         return;

      end if;

      Prev := B;

      Find_Match :
      loop

         X := Next (Prev);

         if X = Null_Node then
            return;
         end if;

         exit when Is_Equal_Key_Node (Key, X);

         Prev := X;

      end loop Find_Match;

      Set_Next (Node => Prev, Next => Next (X));

      --pragma Assert (L > 0);
      L := L - 1;

      Free (X);

   end Delete;



   procedure Delete
     (HT : in out Hash_Table_Type;
      X  : in out Node_Access) is

      pragma Assert (X /= Null_Node);

      L : Size_Type renames HT.Length;
      pragma Assert (L > 0);

      I : constant Hash_Type := Index (HT, X);

      B : Node_Access renames HT.Buckets (I);
      pragma Assert (B /= Null_Node);

      Prev, Curr : Node_Access;

   begin

      if B = X then

         --handle matching first node as special case

         B := Next (B);

         L := L - 1;

         Free (X);

         return;

      end if;

      pragma Assert (L > 1);

      Prev := B;

      loop

         Curr := Next (Prev);
         pragma Assert (Curr /= Null_Node);

         exit when Curr = X;

         Prev := Curr;

      end loop;

      Set_Next (Node => Prev, Next => Next (X));

      L := L - 1;

      Free (X);

   end Delete;


   function Find
     (HT  : Hash_Table_Type;
      Key : Key_Type) return Node_Access is

      I : constant Hash_Type := Index (HT, Key);

      Node : Node_Access := HT.Buckets (I);

   begin

      while Node /= Null_Node loop

         if Is_Equal_Key_Node (Key, Node) then
            return Node;
         end if;

         Node := Next (Node);

      end loop;

      return Null_Node;

   end Find;



   function First (HT : Hash_Table_Type)
     return Node_Access is

   begin

      if HT.Length = 0 then
         return Null_Node;
      end if;

      for I in HT.Buckets'Range loop

         if HT.Buckets (I) /= Null_Node then
            return HT.Buckets (I);
         end if;

      end loop;

      pragma Assert (False);
      return Null_Node;  --dummy stmt

   end First;


   function Succ (HT   : Hash_Table_Type;
                  Node : Node_Access)
     return Node_Access is

      Result : Node_Access := Next (Node);

   begin

      if Result /= Null_Node then
         return Result;
      end if;

      for I in Index (HT, Node) + 1 .. HT.Buckets'Last loop

         Result := HT.Buckets (I);

         if Result /= Null_Node then
            return Result;
         end if;

      end loop;

      return Null_Node;

   end Succ;


   procedure Generic_Iteration (HT : in Hash_Table_Type) is

      Node : Node_Access;

   begin

      if HT.Buckets = null
        or else HT.Length = 0
      then
         return;
      end if;

      for I in HT.Buckets'Range loop

         Node := HT.Buckets (I);

         while Node /= Null_Node loop
            Process (Node);
            Node := Next (Node);
         end loop;

      end loop;

   end Generic_Iteration;


   procedure Generic_Write
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      HT     : in     Hash_Table_Type) is

      M : Size_Type'Base;
      X : Node_Access;

   begin

      if HT.Buckets = null then
         Hash_Type'Write (Stream, 0);
      else
         Hash_Type'Write (Stream, HT.Buckets'Last);
      end if;

      Size_Type'Base'Write (Stream, HT.Length);

      if HT.Length = 0 then
         return;
      end if;

      for I in HT.Buckets'Range loop

         X := HT.Buckets (I);

         if X /= Null_Node then

            M := 1;

            loop

               X := Next (X);

               exit when X = Null_Node;

               M := M + 1;

            end loop;

            Hash_Type'Write (Stream, I);
            Size_Type'Base'Write (Stream, M);

            X := HT.Buckets (I);

            for J in Size_Type range 1 .. M loop

               Write (Stream, X);
               X := Next (X);

            end loop;

            pragma Assert (X = Null_Node);

         end if;

      end loop;

   end Generic_Write;



   procedure Generic_Read
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      HT     :    out Hash_Table_Type) is

      X, Y : Node_Access;

      Last, I : Hash_Type;
      N, M    : Size_Type'Base;

   begin

      --NOTE:
      --As with the sorted set, it's not clear whether read
      --is allowed to have side-effect if it fails.  For now
      --I'm going to assume side-effect is allowed since it
      --simplifies the algorithm.
      --END NOTE.
      --
      Clear (HT);

      declare
         B : Buckets_Access := HT.Buckets;
      begin
         HT.Buckets := null;
         HT.Length := 0;

         Free (B); -- can this fail?
      end;

      Hash_Type'Read (Stream, Last);

      if Last /= 0 then
         HT.Buckets := new Buckets_Type (0 .. Last);
      end if;

      Size_Type'Base'Read (Stream, N);
      pragma Assert (N >= 0);

      while N > 0 loop

         Hash_Type'Read (Stream, I);
         pragma Assert (I in HT.Buckets'Range);
         pragma Assert (HT.Buckets (I) = Null_Node);

         Size_Type'Base'Read (Stream, M);
         pragma Assert (M >= 1);
         pragma Assert (M <= N);

         HT.Buckets (I) := New_Node (Stream);
         pragma Assert (HT.Buckets (I) /= Null_Node);
         pragma Assert (Next (HT.Buckets (I)) = Null_Node);

         Y := HT.Buckets (I);

         HT.Length := HT.Length + 1;

         for J in Size_Type range 2 .. M loop

            X := New_Node (Stream);
            pragma Assert (X /= Null_Node);
            pragma Assert (Next (X) = Null_Node);

            Set_Next (Node => Y, Next => X);
            Y := X;

            HT.Length := HT.Length + 1;

         end loop;

         N := N - M;

      end loop;

   end Generic_Read;



end AI302.Containers.Hash_Tables;

