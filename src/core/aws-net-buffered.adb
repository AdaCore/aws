------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2002-2015, AdaCore                     --
--                                                                          --
--  This library is free software;  you can redistribute it and/or modify   --
--  it under terms of the  GNU General Public License  as published by the  --
--  Free Software  Foundation;  either version 3,  or (at your  option) any --
--  later version. This library is distributed in the hope that it will be  --
--  useful, but WITHOUT ANY WARRANTY;  without even the implied warranty of --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                    --
--                                                                          --
--  As a special exception under Section 7 of GPL version 3, you are        --
--  granted additional permissions described in the GCC Runtime Library     --
--  Exception, version 3.1, as published by the Free Software Foundation.   --
--                                                                          --
--  You should have received a copy of the GNU General Public License and   --
--  a copy of the GCC Runtime Library Exception along with this program;    --
--  see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see   --
--  <http://www.gnu.org/licenses/>.                                         --
--                                                                          --
--  As a special exception, if other files instantiate generics from this   --
--  unit, or you link this unit with other files to produce an executable,  --
--  this  unit  does not  by itself cause  the resulting executable to be   --
--  covered by the GNU General Public License. This exception does not      --
--  however invalidate any other reasons why the executable file  might be  --
--  covered by the  GNU Public License.                                     --
------------------------------------------------------------------------------

pragma Ada_2012;

with AWS.Containers.Memory_Streams;
with AWS.Default;
with AWS.Translator;

package body AWS.Net.Buffered is

   CRLF : constant Stream_Element_Array :=
            Translator.To_Stream_Element_Array (ASCII.CR & ASCII.LF);

   Input_Limit : Positive := AWS.Default.Input_Line_Size_Limit with Atomic;

   function Is_Empty
     (C : not null access constant Read_Cache) return Boolean
   is
     (C.First > C.Last) with Inline_Always;
   --  Returns true if the read-cache is empty

   procedure Read (Socket : Socket_Type'Class)
     with Pre => Socket.C.R_Cache /= null;
   --  Refill the read-cache

   function Get_Read_Cache
     (Socket : Socket_Type'Class)
      return not null access Read_Cache with Inline;
   --  Get the socket read cache, create the cache if it is not created

   function Get_Write_Cache
     (Socket : Socket_Type'Class)
      return not null access Write_Cache with Inline;
   --  Get the socket write cache, create the cache if it is not created

   -----------
   -- Flush --
   -----------

   procedure Flush (Socket : Socket_Type'Class) is
      C : Write_Cache_Access  renames Socket.C.W_Cache;
   begin
      if C = null then
         return;
      end if;

      if C.Last > 0 then
         Send (Socket, C.Buffer (1 .. C.Last));
         C.Last := 0;
      end if;
   end Flush;

   --------------
   -- Get_Char --
   --------------

   function Get_Char (Socket : Socket_Type'Class) return Character is
      C    : constant not null access Read_Cache := Get_Read_Cache (Socket);
      Char : Character;
   begin
      if Is_Empty (C) then
         Read (Socket);
      end if;

      Char    := Character'Val (C.Buffer (C.First));
      C.First := C.First + 1;

      return Char;
   end Get_Char;

   ---------------------
   -- Get_Input_Limit --
   ---------------------

   function Get_Input_Limit return Stream_Element_Offset is
   begin
      return Stream_Element_Offset (Input_Limit);
   end Get_Input_Limit;

   --------------
   -- Get_Line --
   --------------

   function Get_Line (Socket : Socket_Type'Class) return String is
      Line : constant String := Read_Until (Socket, "" & ASCII.LF, True);
   begin
      if Line'Length > 0 and then Line (Line'Last) = ASCII.LF then
         if Line'Length > 1 and then Line (Line'Last - 1) = ASCII.CR then
            return Line (Line'First .. Line'Last - 2);
         else
            return Line (Line'First .. Line'Last - 1);
         end if;

      else
         return Line;
      end if;
   end Get_Line;

   --------------------
   -- Get_Read_Cache --
   --------------------

   function Get_Read_Cache
     (Socket : Socket_Type'Class) return not null access Read_Cache is
   begin
      if Socket.C.R_Cache = null then
         Socket.C.R_Cache := new Read_Cache (R_Cache_Size);
      end if;

      return Socket.C.R_Cache;
   end Get_Read_Cache;

   ---------------------
   -- Get_Write_Cache --
   ---------------------

   function Get_Write_Cache
     (Socket : Socket_Type'Class) return not null access Write_Cache is
   begin
      if Socket.C.W_Cache = null then
         Socket.C.W_Cache := new Write_Cache (W_Cache_Size);
      end if;

      return Socket.C.W_Cache;
   end Get_Write_Cache;

   --------------
   -- New_Line --
   --------------

   procedure New_Line (Socket : Socket_Type'Class) is
   begin
      Write (Socket, CRLF);
   end New_Line;

   ---------------
   -- Peek_Char --
   ---------------

   function Peek_Char (Socket : Socket_Type'Class) return Character is
      C : constant not null access Read_Cache := Get_Read_Cache (Socket);
   begin
      if Is_Empty (C) then
         Read (Socket);
      end if;

      return Character'Val (Natural (C.Buffer (C.First)));
   end Peek_Char;

   ---------
   -- Put --
   ---------

   procedure Put (Socket : Socket_Type'Class; Item : String) is
   begin
      Write (Socket, Translator.To_Stream_Element_Array (Item));
   end Put;

   --------------
   -- Put_Line --
   --------------

   procedure Put_Line (Socket : Socket_Type'Class; Item : String) is
   begin
      Write (Socket, Translator.To_Stream_Element_Array (Item) & CRLF);
   end Put_Line;

   ----------
   -- Read --
   ----------

   procedure Read (Socket : Socket_Type'Class) is
      C : constant not null access Read_Cache := Socket.C.R_Cache;
   begin
      Receive (Socket, C.Buffer, C.Last);

      --  Reset C.First only after successful Receive, the buffer would
      --  remain empty on timeout this way.

      C.First := C.Buffer'First;
   end Read;

   procedure Read
     (Socket : Socket_Type'Class;
      Data   : out Stream_Element_Array;
      Last   : out Stream_Element_Offset)
   is
      C : constant not null access Read_Cache := Get_Read_Cache (Socket);
   begin
      Flush (Socket);

      if Is_Empty (C) then

         if Data'Length < C.Buffer'Length then
            --  If Data fit in the cache, fill it
            Read (Socket);

         else
            --  Otherwise read the socket directly
            Receive (Socket, Data, Last);
            return;
         end if;
      end if;

      Read_Buffer (Socket, Data, Last);

      --  Data could remain in internal socket buffer, if there is some
      --  space on the buffer, read the socket.

      if Last < Data'Last and then Pending (Socket) > 0 then
         Receive (Socket, Data (Last + 1 .. Data'Last), Last);
      end if;
   end Read;

   function Read
     (Socket : Socket_Type'Class;
      Max    : Stream_Element_Count := 4096) return Stream_Element_Array
   is
      Buffer : Stream_Element_Array (1 .. Max);
      Last   : Stream_Element_Offset;
   begin
      Read (Socket, Buffer, Last);

      return Buffer (1 .. Last);
   end Read;

   procedure Read
     (Socket : Socket_Type'Class; Data : out Stream_Element_Array)
   is
      Last  : Stream_Element_Offset;
      First : Stream_Element_Offset := Data'First;
   begin
      loop
         Read (Socket, Data (First .. Data'Last), Last);
         exit when Last = Data'Last;
         First := Last + 1;
      end loop;
   end Read;

   -----------------
   -- Read_Buffer --
   -----------------

   procedure Read_Buffer
     (Socket : Socket_Type'Class;
      Data   : out Stream_Element_Array;
      Last   : out Stream_Element_Offset)
   is
      C : Read_Cache_Access renames Socket.C.R_Cache;
   begin
      if C = null then
         Last := Last_Index (Data'First, Count => 0);

      else
         declare
            C_Last : constant Stream_Element_Offset :=
                       Stream_Element_Offset'Min
                         (C.Last, C.First + Data'Length - 1);
         begin
            Last := Data'First + C_Last - C.First;
            Data (Data'First .. Last) := C.Buffer (C.First .. C_Last);
            C.First := C_Last + 1;
         end;
      end if;
   end Read_Buffer;

   ----------------
   -- Read_Until --
   ----------------

   function Read_Until
     (Socket    : Socket_Type'Class;
      Delimiter : Stream_Element_Array;
      Wait      : Boolean := True) return Stream_Element_Array
   is
      use Containers.Memory_Streams;

      function Buffered return Stream_Element_Array with Inline;

      procedure Finalize;

      Finalizer : Utils.Finalizer (Finalize'Access) with Unreferenced;
      Buffer    : Stream_Type;

      --------------
      -- Buffered --
      --------------

      function Buffered return Stream_Element_Array is
         Result : Stream_Element_Array (1 .. Size (Buffer));
         Last   : Stream_Element_Offset;
      begin
         Read (Buffer, Result, Last);
         return Result;
      end Buffered;

      --------------
      -- Finalize --
      --------------

      procedure Finalize is
      begin
         Close (Buffer);
      end Finalize;

      C    : constant not null access Read_Cache := Get_Read_Cache (Socket);
      J, K : Stream_Element_Offset;

   begin
      if Wait then
         Flush (Socket);
      end if;

      J := C.First;
      K := Delimiter'First;

      loop
         if J > C.Last then
            if Wait then
               Append (Buffer, C.Buffer (C.First .. C.Last));

               if Size (Buffer) > Stream_Element_Offset (Input_Limit) then
                  raise Data_Overflow with
                    "Size" & Stream_Element_Offset'Image (Size (Buffer));
               end if;

               begin
                  Read (Socket);
               exception
                  when E : Socket_Error =>
                     if Size (Buffer) = 0 or else Is_Timeout (Socket, E)  then
                        raise;
                     else
                        C.First := 1;
                        C.Last  := 0;

                        return Buffered;
                     end if;
               end;

               J := C.First;
            else
               return (1 .. 0 => 0);
            end if;
         end if;

         if C.Buffer (J) = Delimiter (K) then
            if K = Delimiter'Last then
               K := C.First;
               C.First := J + 1;

               return Buffered & C.Buffer (K .. J);

            else
               K := K + 1;
            end if;

            J := J + 1;

         elsif K = Delimiter'First then
            J := J + 1;
         else
            K := Delimiter'First;
         end if;
      end loop;
   end Read_Until;

   function Read_Until
     (Socket    : Socket_Type'Class;
      Delimiter : String;
      Wait      : Boolean := True) return String is
   begin
      return Translator.To_String
        (Read_Until
           (Socket, Translator.To_Stream_Element_Array (Delimiter), Wait));
   end Read_Until;

   ---------------------
   -- Set_Input_Limit --
   ---------------------

   procedure Set_Input_Limit (Limit : Positive) is
   begin
      Input_Limit := Limit;
   end Set_Input_Limit;

   --------------
   -- Shutdown --
   --------------

   procedure Shutdown (Socket : Socket_Type'Class) is
   begin
      begin
         Flush (Socket);
      exception
         when Socket_Error =>
            --  Ignore recent cache buffer send error
            null;
      end;

      Net.Shutdown (Socket);
   end Shutdown;

   -----------
   -- Write --
   -----------

   procedure Write
     (Socket : Socket_Type'Class;
      Item   : Stream_Element_Array)
   is
      C         : constant not null access Write_Cache :=
                    Get_Write_Cache (Socket);
      Next_Last : constant Stream_Element_Offset := C.Last + Item'Length;
   begin
      if Next_Last > C.Max_Size then
         Send (Socket, C.Buffer (1 .. C.Last));
         Send (Socket, Item);
         C.Last := 0;
      else
         C.Buffer (C.Last + 1 .. Next_Last) := Item;
         C.Last := Next_Last;
      end if;
   end Write;

end AWS.Net.Buffered;
