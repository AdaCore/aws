------------------------------------------------------------------------------
--                       Generic memory stream                              --
--                                                                          --
--                        Copyright (C) 2003                                --
--                        Dmitriy Anisimkov                                 --
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

--  $Id$

with Ada.Unchecked_Deallocation;

package body Memory_Streams is

   procedure Free (Item : in out Buffer_Access);

   procedure Free is
      new Ada.Unchecked_Deallocation (Element_Array, Element_Access);

   ------------
   -- Append --
   ------------

   procedure Append
     (File  : in out Stream_Type;
      Value : in     Element_Array)
   is
      Block_Length : constant Element_Offset
        := File.Last_Length + Value'Length;

   begin
      if File.First = null then
         File.First := new Buffer_Type;

         if Value'Length >= First_Block_Length then
            File.First.Data := new Element_Array (1 .. Value'Length);
         else
            File.First.Data := new Element_Array (1 .. First_Block_Length);
         end if;

         File.First.Data (1 .. Value'Length) := Value;

         File.Current     := File.First;
         File.Last        := File.First;
         File.Last_Length := Value'Length;

      elsif Block_Length <= File.Last.Data'Length then
         File.Last.Data (File.Last_Length + 1
                         .. Block_Length) := Value;
         File.Last_Length := Block_Length;

      else
         declare
            Split_Value : constant Element_Index
              := Value'First + File.Last.Data'Length - File.Last_Length;
            Next_Length : constant Element_Index
              := Value'Last - Split_Value + 1;
         begin
            File.Last.Data (File.Last_Length + 1 .. File.Last.Data'Last)
              := Value (Value'First .. Split_Value - 1);

            File.Last.Next := new Buffer_Type;
            File.Last      := File.Last.Next;

            if Next_Length >= Next_Block_Length then
               File.Last.Data := new Element_Array (1 .. Next_Length);
            else
               File.Last.Data := new Element_Array (1 .. Next_Block_Length);
            end if;

            File.Last.Data (1 .. Next_Length)
              := Value (Split_Value .. Value'Last);

            File.Last_Length := Next_Length;
         end;
      end if;

      File.Length := File.Length + Value'Length;
   end Append;

   ------------
   -- Append --
   ------------

   procedure Append
     (File : in out Stream_Type;
      Data : in     Element_Access) is
   begin
      if File.First = null then
         File.First       := new Buffer_Type;
         File.First.Data  := Data;
         File.Current     := File.First;
         File.Last        := File.First;
         File.Last_Length := Data'Length;

      else
         if File.Last.Data'Length > File.Last_Length then
            declare
               Ptr : constant Element_Access
                := new Element_Array'(File.Last.Data (1 .. File.Last_Length));
            begin
               Free (File.Last.Data);
               File.Last.Data := Ptr;
            end;
         end if;

         File.Last.Next   := new Buffer_Type;
         File.Last        := File.Last.Next;
         File.Last.Data   := Data;
         File.Last_Length := Data'Length;
      end if;

      File.Length  := File.Length + Data'Length;
   end Append;

   -----------
   -- Close --
   -----------

   procedure Close (File : in out Stream_Type) is
      First  : Buffer_Access;
      Length : Element_Offset := 0;
   begin
      while File.First /= null loop
         First := File.First;

         if First.Next = null then
            Length := Length + File.Last_Length;
         else
            Length := Length + First.Data'Length;
         end if;

         File.First := First.Next;
         Free (First);
      end loop;

      File.Current := null;
      File.Last    := null;

      Reset (File);

      if File.Length /= Length then
         raise Program_Error;
      end if;

      File.Length := 0;
   end Close;

   -----------------
   -- End_Of_File --
   -----------------

   function End_Of_File (File : in Stream_Type) return Boolean is
   begin
      return File.Current = null
        or else (File.Current.Next = null
                   and then File.Current_Offset > File.Current.Data'Last);
   end End_Of_File;

   ----------
   -- Free --
   ----------

   procedure Free (Item : in out Buffer_Access) is
      procedure Deallocate is
         new Ada.Unchecked_Deallocation (Buffer_Type, Buffer_Access);
   begin
      Free (Item.Data);
      Deallocate (Item);
   end Free;

   ----------
   -- Read --
   ----------

   procedure Read
     (File   : in out Stream_Type;
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
         Buffer_Len_1    : constant Element_Offset
           := Buffer'Last - Buffer_Offset;
         --  Buffer remain length minus 1

         Current_Len_1   : constant Element_Offset
           := Data'Last - File.Current_Offset;
         --  Data remain length minus 1

         Current_Last    : Element_Index;
      begin
         Block_Over := Buffer_Len_1 >= Current_Len_1;

         if Block_Over then
            Last := Buffer_Offset + Current_Len_1;

            Buffer (Buffer_Offset .. Last)
              := Data (File.Current_Offset .. Data'Last);

            Buffer_Offset := Last + 1;

            File.Current_Offset := Data'Last + 1;

         else
            Last := Buffer'Last;

            Current_Last := File.Current_Offset + Buffer_Len_1;

            Buffer (Buffer_Offset .. Last)
              := Data (File.Current_Offset .. Current_Last);

            File.Current_Offset := Current_Last + 1;
         end if;
      end Append;

   begin
      Last := Buffer'First - 1;

      if File.Current = null then
         return;
      end if;

      loop
         if File.Current.Next = null then
            --  Last block.

            Append (File.Current.Data (1 .. File.Last_Length));

            if Block_Over then
               File.Current := null;
               exit;
            end if;

         else
            Append (File.Current.Data.all);

            if Block_Over then
               File.Current := File.Current.Next;

               File.Current_Offset := File.Current.Data'First;
            end if;
         end if;

         exit when Last = Buffer'Last;
      end loop;
   end Read;

   -----------
   -- Reset --
   -----------

   procedure Reset (File : in out Stream_Type) is
   begin
      File.Current        := File.First;
      File.Current_Offset := 1;
   end Reset;

   ----------
   -- Size --
   ----------

   function Size (File : in Stream_Type) return Element_Offset is
   begin
      return File.Length;
   end Size;

end Memory_Streams;
