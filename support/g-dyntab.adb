------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                   G N A T . D Y N A M I C _ T A B L E S                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--           Copyright (C) 2000-2003 Ada Core Technologies, Inc.            --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with System; use System;

with Unchecked_Deallocation;

package body GNAT.Dynamic_Tables is

   Min : constant Integer := Integer (Table_Low_Bound);
   --  Subscript of the minimum entry in the currently allocated table

   procedure Free is new Unchecked_Deallocation (Table_Type, Table_Ptr);

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Reallocate (T : in out Instance);
   --  Reallocate the existing table according to the current value stored
   --  in Max. Works correctly to do an initial allocation if the table
   --  is currently null.

   --------------
   -- Allocate --
   --------------

   procedure Allocate
     (T   : in out Instance;
      Num : Integer := 1)
   is
   begin
      T.P.Last_Val := T.P.Last_Val + Num;

      if T.P.Last_Val > T.P.Max then
         Reallocate (T);
      end if;
   end Allocate;

   ------------
   -- Append --
   ------------

   procedure Append (T : in out Instance; New_Val : Table_Component_Type) is
   begin
      Increment_Last (T);
      T.Table (Table_Index_Type (T.P.Last_Val)) := New_Val;
   end Append;

   --------------------
   -- Decrement_Last --
   --------------------

   procedure Decrement_Last (T : in out Instance) is
   begin
      T.P.Last_Val := T.P.Last_Val - 1;
   end Decrement_Last;

   --------------
   -- For_Each --
   --------------

   procedure For_Each (Table : Instance) is
      Quit : Boolean := False;
   begin
      for Index in Table_Low_Bound .. Table_Index_Type (Table.P.Last_Val) loop
         Action (Index, Table.Table (Index), Quit);
         exit when Quit;
      end loop;
   end For_Each;

   ----------
   -- Free --
   ----------

   procedure Free (T : in out Instance) is
   begin
      Free (T.Table);
      T.P.Length := 0;
   end Free;

   --------------------
   -- Increment_Last --
   --------------------

   procedure Increment_Last (T : in out Instance) is
   begin
      T.P.Last_Val := T.P.Last_Val + 1;

      if T.P.Last_Val > T.P.Max then
         Reallocate (T);
      end if;
   end Increment_Last;

   ----------
   -- Init --
   ----------

   procedure Init (T : in out Instance) is
      Old_Length : constant Integer := T.P.Length;

   begin
      T.P.Last_Val := Min - 1;
      T.P.Max      := Min + Table_Initial - 1;
      T.P.Length   := T.P.Max - Min + 1;

      --  If table is same size as before (happens when table is never
      --  expanded which is a common case), then simply reuse it. Note
      --  that this also means that an explicit Init call right after
      --  the implicit one in the package body is harmless.

      if Old_Length = T.P.Length then
         return;

      --  Otherwise we can use Reallocate to get a table of the right size.
      --  Note that Reallocate works fine to allocate a table of the right
      --  initial size when it is first allocated.

      else
         Reallocate (T);
      end if;
   end Init;

   ----------
   -- Last --
   ----------

   function Last (T : in Instance) return Table_Index_Type is
   begin
      return Table_Index_Type (T.P.Last_Val);
   end Last;

   ----------------
   -- Reallocate --
   ----------------

   procedure Reallocate (T : in out Instance) is
      New_Length : Integer;
      New_Size   : Integer;

   begin
      if T.P.Max < T.P.Last_Val then
         while T.P.Max < T.P.Last_Val loop
            New_Length := T.P.Length * (100 + Table_Increment) / 100;

            if New_Length > T.P.Length then
               T.P.Length := New_Length;
            else
               T.P.Length := T.P.Length + 1;
            end if;

            T.P.Max := Min + T.P.Length - 1;
         end loop;
      end if;

      New_Size := T.P.Max - Min + 1;

      if T.Table = null then
         T.Table := new Table_Type
           (Table_Low_Bound .. Table_Index_Type (New_Size));

      elsif New_Size > 0 then
         declare
            New_T : Table_Ptr;
         begin
            New_T := new Table_Type
              (Table_Low_Bound .. Table_Index_Type (New_Size));

            --  Copy table into new one

            for K in Table_Index_Type
              range Table_Low_Bound
                      .. Table_Index_Type'Min
                           (Table_Index_Type (T.P.Last_Val), T.Table'Last)
            loop
               New_T (K) := T.Table (K);
            end loop;

            Free (T.Table);
            T.Table := New_T;
         end;

      end if;

      if T.P.Length /= 0 and then T.Table = null then
         raise Storage_Error;
      end if;
   end Reallocate;

   -------------
   -- Release --
   -------------

   procedure Release (T : in out Instance) is
   begin
      T.P.Length := T.P.Last_Val - Integer (Table_Low_Bound) + 1;
      T.P.Max    := T.P.Last_Val;
      Reallocate (T);
   end Release;

   --------------
   -- Set_Item --
   --------------

   procedure Set_Item
     (T     : in out Instance;
      Index : Table_Index_Type;
      Item  : Table_Component_Type)
   is
   begin
      if Integer (Index) > T.P.Last_Val then
         Set_Last (T, Index);
      end if;

      T.Table (Index) := Item;
   end Set_Item;

   --------------
   -- Set_Last --
   --------------

   procedure Set_Last (T : in out Instance; New_Val : Table_Index_Type) is
   begin
      if Integer (New_Val) < T.P.Last_Val then
         T.P.Last_Val := Integer (New_Val);

      else
         T.P.Last_Val := Integer (New_Val);

         if T.P.Last_Val > T.P.Max then
            Reallocate (T);
         end if;
      end if;
   end Set_Last;

end GNAT.Dynamic_Tables;
