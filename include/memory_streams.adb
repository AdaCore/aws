------------------------------------------------------------------------------
--                       Generic memory stream                              --
--                                                                          --
--                Copyright (C) 2003-2015, Dmitriy Anisimkov                --
--                                                                          --
--  This library is free software; you can redistribute it and/or modify    --
--  it under the terms of the GNU General Public License as published by    --
--  the Free Software Foundation; either version 2 of the License, or (at   --
--  your option) any later version.                                         --
--                                                                          --
--  This library is distributed in the hope that it will be useful, but     --
--  WITHOUT ANY WARRANTY; without even the implied warranty of              --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU       --
--  General Public License for more details.                                --
--                                                                          --
--  You should have received a copy of the GNU General Public License       --
--  along with this library; if not, write to the Free Software Foundation, --
--  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.          --
--                                                                          --
--  As a special exception, if other files instantiate generics from this   --
--  unit, or you link this unit with other files to produce an executable,  --
--  this  unit  does not  by itself cause  the resulting executable to be   --
--  covered by the GNU General Public License. This exception does not      --
--  however invalidate any other reasons why the executable file  might be  --
--  covered by the  GNU Public License.                                     --
------------------------------------------------------------------------------

pragma Ada_2012;

with Ada.Unchecked_Deallocation;

package body Memory_Streams is

   procedure Free (Item : in out Buffer_Access);

   procedure Free is
      new Ada.Unchecked_Deallocation (Element_Array, Element_Access);

   procedure Trim_Last_Block (Stream : in out Stream_Type);
   --  Remove unused space from last buffer

   function First (Item : in Buffer_Access) return Element_Index with Inline;

   function Last (Item : in Buffer_Access) return Element_Index with Inline;

   ------------
   -- Append --
   ------------

   procedure Append
     (Stream : in out Stream_Type;
      Value  : in     Element_Array;
      Trim   : in     Boolean := False) is
   begin
      if Value'Length = 0 then
         if Trim then
            Trim_Last_Block (Stream);
         end if;

         return;
      end if;

      if Stream.First = null then
         Stream.First := new Buffer_Type (False);

         if Value'Length >= First_Block_Length or else Trim then
            Stream.First.Data := new Element_Array (1 .. Value'Length);
         else
            Stream.First.Data := new Element_Array (1 .. First_Block_Length);
         end if;

         Stream.First.Data (1 .. Value'Length) := Value;

         Stream.Current     := Stream.First;
         Stream.Last        := Stream.First;
         Stream.Last_Length := Value'Length;

      elsif Stream.Last.Steady
        or else Stream.Last_Length = Stream.Last.Data'Length
      then
         Stream.Last.Next := new Buffer_Type (False);
         Stream.Last      := Stream.Last.Next;

         if Value'Length >= Next_Block_Length or else Trim then
            Stream.Last.Data := new Element_Array (1 .. Value'Length);
         else
            Stream.Last.Data := new Element_Array (1 .. Next_Block_Length);
         end if;

         Stream.Last.Data (1 .. Value'Length) := Value;

         Stream.Last_Length := Value'Length;

      else
         declare
            Block_Length : constant Element_Offset :=
                             Stream.Last_Length + Value'Length;
         begin
            if Block_Length <= Stream.Last.Data'Length then
               Stream.Last.Data
                 (Stream.Last_Length + 1 .. Block_Length) := Value;
               Stream.Last_Length := Block_Length;

               if Trim then
                  Trim_Last_Block (Stream);
               end if;

            else
               declare
                  Split_Value : constant Element_Index :=
                                  Value'First
                                    + Stream.Last.Data'Length
                                    - Stream.Last_Length;
                  Next_Length : constant Element_Index :=
                                  Value'Last - Split_Value + 1;
               begin
                  Stream.Last.Data
                    (Stream.Last_Length + 1 .. Stream.Last.Data'Last) :=
                       Value (Value'First .. Split_Value - 1);

                  Stream.Last.Next := new Buffer_Type (False);
                  Stream.Last      := Stream.Last.Next;

                  if Next_Length >= Next_Block_Length or else Trim then
                     Stream.Last.Data := new Element_Array (1 .. Next_Length);
                  else
                     Stream.Last.Data :=
                       new Element_Array (1 .. Next_Block_Length);
                  end if;

                  Stream.Last.Data (1 .. Next_Length) :=
                    Value (Split_Value .. Value'Last);

                  Stream.Last_Length := Next_Length;
               end;
            end if;
         end;
      end if;

      Stream.Length := Stream.Length + Value'Length;
   end Append;

   ------------
   -- Append --
   ------------

   procedure Append
     (Stream : in out Stream_Type;
      Data   : in     Constant_Access) is
   begin
      if Data'Length = 0 then
         return;
      end if;

      if Stream.First = null then
         Stream.First          := new Buffer_Type (True);
         Stream.First.Const    := Data;
         Stream.Current        := Stream.First;
         Stream.Last           := Stream.First;
         Stream.Last_Length    := Data'Length;
         Stream.Current_Offset := Data'First;

      else
         Trim_Last_Block (Stream);

         Stream.Last.Next       := new Buffer_Type (True);
         Stream.Last            := Stream.Last.Next;
         Stream.Last.Const      := Data;
         Stream.Last_Length     := Data'Length;
      end if;

      Stream.Length  := Stream.Length + Data'Length;
   end Append;

   procedure Append
     (Stream : in out Stream_Type;
      Data   : in     Element_Access) is
   begin
      if Data'Length = 0 then
         return;
      end if;

      if Stream.First = null then
         Stream.First          := new Buffer_Type (False);
         Stream.First.Data     := Data;
         Stream.Current        := Stream.First;
         Stream.Last           := Stream.First;
         Stream.Last_Length    := Data'Length;
         Stream.Current_Offset := Data'First;

      else
         Trim_Last_Block (Stream);

         Stream.Last.Next       := new Buffer_Type (False);
         Stream.Last            := Stream.Last.Next;
         Stream.Last.Data       := Data;
         Stream.Last_Length     := Data'Length;
      end if;

      Stream.Length  := Stream.Length + Data'Length;
   end Append;

   -----------
   -- Close --
   -----------

   procedure Close (Stream : in out Stream_Type) is
      First  : Buffer_Access;
      Length : Element_Offset := 0;
   begin
      while Stream.First /= null loop
         First := Stream.First;

         if First.Next = null then
            Length := Length + Stream.Last_Length;

         else
            if First.Steady then
               Length := Length + First.Const'Length;
            else
               Length := Length + First.Data'Length;
            end if;
         end if;

         Stream.First := First.Next;
         Free (First);
      end loop;

      Stream.Current := null;
      Stream.Last    := null;

      Reset (Stream);

      if Stream.Length /= Length then
         raise Program_Error;
      end if;

      Stream.Length := 0;
   end Close;

   -----------------
   -- End_Of_File --
   -----------------

   function End_Of_File (Stream : in Stream_Type) return Boolean is
   begin
      return Stream.Current = null
        or else (Stream.Current.Next = null
                 and then Stream.Current_Offset > Last (Stream.Current));
   end End_Of_File;

   -----------
   -- First --
   -----------

   function First (Item : in Buffer_Access) return Element_Index is
   begin
      if Item.Steady then
         return Item.Const'First;
      else
         return Item.Data'First;
      end if;
   end First;

   ----------
   -- Free --
   ----------

   procedure Free (Item : in out Buffer_Access) is
      procedure Deallocate is
         new Ada.Unchecked_Deallocation (Buffer_Type, Buffer_Access);
   begin
      if not Item.Steady then
         Free (Item.Data);
      end if;

      Deallocate (Item);
   end Free;

   ----------
   -- Last --
   ----------

   function Last (Item : in Buffer_Access) return Element_Index is
   begin
      if Item.Steady then
         return Item.Const'Last;
      else
         return Item.Data'Last;
      end if;
   end Last;

   -------------
   -- Pending --
   -------------

   function Pending (Stream : in Stream_Type) return Element_Offset is
   begin
      if Stream.Current = null then
         return 0;

      else
         declare
            B : Buffer_Access := Stream.Current;
            S : Element_Offset := 0;
         begin
            while B /= null loop
               if B.Next = null then
                  if B.Steady then
                     S := S + Last (B);
                  else
                     S := S + Stream.Last_Length - Stream.Current_Offset + 1;
                  end if;

               else
                  S := S + Last (B);
               end if;

               B := B.Next;
            end loop;

            return S;
         end;
      end if;
   end Pending;

   ----------
   -- Read --
   ----------

   procedure Read
     (Stream : in out Stream_Type;
      Buffer :    out Element_Array;
      Last   :    out Element_Offset)
   is
      Buffer_Offset : Element_Offset := Buffer'First;
      Block_Over    : Boolean;

      procedure Append (Data : in Element_Array);
      --  Add the Data to the Buffer till necessary from the
      --  Resource.Current_Offset and move it.

      ------------
      -- Append --
      ------------

      procedure Append (Data : in Element_Array) is
         Buffer_Len_1  : constant Element_Offset :=
                           Buffer'Last - Buffer_Offset;
         --  Buffer remain length minus 1

         Current_Len_1 : constant Element_Offset :=
                           Data'Last - Stream.Current_Offset;
         --  Data remain length minus 1

         Current_Last  : Element_Index;
      begin
         Block_Over := Buffer_Len_1 >= Current_Len_1;

         if Block_Over then
            Last := Buffer_Offset + Current_Len_1;

            Buffer (Buffer_Offset .. Last) :=
              Data (Stream.Current_Offset .. Data'Last);

            Buffer_Offset := Last + 1;

            Stream.Current_Offset := Data'Last + 1;

         else
            Last := Buffer'Last;

            Current_Last := Stream.Current_Offset + Buffer_Len_1;

            Buffer (Buffer_Offset .. Last) :=
              Data (Stream.Current_Offset .. Current_Last);

            Stream.Current_Offset := Current_Last + 1;
         end if;
      end Append;

   begin
      Last := Buffer'First - 1;

      if Stream.Current = null then
         return;
      end if;

      loop
         if Stream.Current.Next = null then
            --  Last block

            if Stream.Current.Steady then
               Append (Stream.Current.Const.all);
            else
               Append (Stream.Current.Data
                         (Stream.Current.Data'First
                          .. Stream.Last_Length + Stream.Current.Data'First
                             - 1));
            end if;

            if Block_Over then
               Stream.Current := null;
               exit;
            end if;

         else
            if Stream.Current.Steady then
               Append (Stream.Current.Const.all);
            else
               Append (Stream.Current.Data.all);
            end if;

            if Block_Over then
               Stream.Current := Stream.Current.Next;

               Stream.Current_Offset := First (Stream.Current);
            end if;
         end if;

         exit when Last = Buffer'Last;
      end loop;
   end Read;

   -----------
   -- Reset --
   -----------

   procedure Reset (Stream : in out Stream_Type) is
   begin
      Stream.Current        := Stream.First;
      Stream.Current_Offset := 1;
   end Reset;

   ---------------
   -- Set_Index --
   ---------------

   procedure Set_Index
     (Stream : in out Stream_Type;
      To     : in     Element_Offset)
   is
      Idx : Element_Offset := Last (Stream.First);
   begin
      if To < 1 or else To > Size (Stream) then
         Stream.Current        := Stream.Last;
         Stream.Current_Offset := Last (Stream.Current) + 1;

      else
         Stream.Current        := Stream.First;

         while Idx < To loop
            Stream.Current := Stream.Current.Next;
            Idx := Idx + Last (Stream.Current);
         end loop;

         Stream.Current_Offset := Last (Stream.Current) - Idx + To;
      end if;
   end Set_Index;

   ----------
   -- Size --
   ----------

   function Size (Stream : in Stream_Type) return Element_Offset is
   begin
      return Stream.Length;
   end Size;

   ----------------------
   -- Trim_Last_Buffer --
   ----------------------

   procedure Trim_Last_Block (Stream : in out Stream_Type) is
   begin
      if Stream.Last = null
        or else Stream.Last.Steady
        or else Stream.Last.Data'Length = Stream.Last_Length
      then
         return;
      end if;

      declare
         Ptr : constant Element_Access :=
                 new Element_Array'
                   (Stream.Last.Data (1 .. Stream.Last_Length));
      begin
         Free (Stream.Last.Data);
         Stream.Last.Data   := Ptr;
         Stream.Last_Length := Ptr'Length;
      end;
   end Trim_Last_Block;

end Memory_Streams;
