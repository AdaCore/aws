------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                            Copyright (C) 2002                            --
--                                ACT-Europe                                --
--                                                                          --
--  Authors: Dmitriy Anisimkov - Pascal Obry                                --
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

with AWS.Translator;

package body AWS.Net.Buffered is

   CRLF : constant Stream_Element_Array
     := Translator.To_Stream_Element_Array (ASCII.CR & ASCII.LF);

   procedure Write
     (Socket : in Socket_Type'Class;
      Item   : in Stream_Element);
   --  Write Item into the buffered socket, flush the buffer if there is not
   --  enough space left

   procedure Read (Socket : in Socket_Type'Class);
   --  Refill the read-cache, the cache must be empty before the call

   procedure Push (C : in out Cache; Item : in Stream_Element);
   pragma Inline (Push);
   --  Add item into the cache C

   procedure Pop (C : in out Cache; Item : out Stream_Element);
   pragma Inline (Push);
   --  Retreive on item from cache C and place it into Item

   function Get_Byte (Socket : in Socket_Type'Class) return Stream_Element;
   pragma Inline (Get_Byte);
   --  Return a single byte from the input socket

   -----------
   -- Flush --
   -----------

   procedure Flush (Socket : in Socket_Type'Class) is
      C : Cache renames Socket.Self.W_Cache;
   begin
      if C.Size /= 0 then

         declare
            Data : Stream_Element_Array (1 .. C.Size);
         begin
            for K in Data'Range loop
               Pop (C, Data (K));
            end loop;

            Send (Socket, Data);
         end;
      end if;
   end Flush;

   --------------
   -- Get_Byte --
   --------------

   function Get_Byte (Socket : in Socket_Type'Class) return Stream_Element is
      C    : Cache renames Socket.Self.R_Cache;
      Byte : Stream_Element;
   begin
      if C.Size = 0 then
         Read (Socket);
      end if;

      Pop (C, Byte);

      return Byte;
   end Get_Byte;

   --------------
   -- Get_Char --
   --------------

   function Get_Char (Socket : in Socket_Type'Class) return Character is
   begin
      return Character'Val (Natural (Get_Byte (Socket)));
   end Get_Char;

   --------------
   -- Get_Line --
   --------------

   function Get_Line (Socket : in Socket_Type'Class) return String is
      Result : String (1 .. 1_024);
      Index  : Positive := Result'First;
      Char   : Character;
   begin
      Flush (Socket);

      loop
         Char := Get_Char (Socket);

         if Char = ASCII.LF then
            return Result (1 .. Index - 1);

         elsif Char /= ASCII.CR then
            Result (Index) := Char;
            Index := Index + 1;

            if Index > Result'Last then
               return Result & Get_Line (Socket);
            end if;
         end if;
      end loop;
   end Get_Line;

   --------------
   -- New_Line --
   --------------

   procedure New_Line (Socket : in Socket_Type'Class) is
   begin
      Write (Socket, CRLF);
   end New_Line;

   ---------
   -- Pop --
   ---------

   procedure Pop (C : in out Cache; Item : out Stream_Element) is
   begin
      Item := C.Buffer (C.First);

      C.First := C.First + 1;

      if C.First > C.Max_Size then
         C.First := 1;
      end if;

      C.Size := C.Size - 1;
   end Pop;

   ----------
   -- Push --
   ----------

   procedure Push (C : in out Cache; Item : in Stream_Element) is
   begin
      C.Last := C.Last + 1;

      if C.Last > C.Max_Size then
         C.Last := 1;
      end if;

      C.Buffer (C.Last) := Item;
      C.Size := C.Size + 1;
   end Push;

   ---------
   -- Put --
   ---------

   procedure Put (Socket : in Socket_Type'Class; Item : in String) is
   begin
      Write (Socket, Translator.To_Stream_Element_Array (Item));
   end Put;

   --------------
   -- Put_Line --
   --------------

   procedure Put_Line (Socket : in Socket_Type'Class; Item : in String) is
   begin
      Write (Socket, Translator.To_Stream_Element_Array (Item) & CRLF);
   end Put_Line;

   ----------
   -- Read --
   ----------

   procedure Read (Socket : in Socket_Type'Class) is

      C      : Cache renames Socket.Self.R_Cache;

      Buffer : constant Stream_Element_Array := Receive (Socket, R_Cache_Size);
      --  Read a chunk of data from the socket

   begin
      for K in Buffer'Range loop
         Push (C, Buffer (K));
      end loop;
   end Read;

   function Read
     (Socket : in Socket_Type'Class;
      Max    : in Stream_Element_Count := 4096)
      return Stream_Element_Array
   is
      C : Cache renames Socket.Self.R_Cache;
   begin
      Flush (Socket);

      if C.Size = 0 then
         --  No more data, read the socket
         return Receive (Socket, Max);

      else
         declare
            Chunk_Size : constant Stream_Element_Offset
              := Stream_Element_Offset'Min (Max, C.Size);
            Chunk      : Stream_Element_Array (1 .. Chunk_Size);
         begin
            for K in Chunk'Range loop
               Chunk (K) := Get_Byte (Socket);
            end loop;

            return Chunk;
         end;
      end if;
   end Read;

   procedure Read
     (Socket : in     Socket_Type'Class;
      Data   :    out Stream_Element_Array)
   is
      Index : Stream_Element_Offset := Data'First;
      Rest  : Stream_Element_Count  := Data'Length;
   begin
      while Rest > 0 loop
         declare
            Buffer : constant Stream_Element_Array := Read (Socket, Rest);
            Length : constant Stream_Element_Count := Buffer'Length;
         begin
            Data (Index .. Index + Length - 1) := Buffer;
            Index := Index + Length;
            Rest  := Rest - Length;
         end;
      end loop;
   end Read;

   --------------
   -- Shutdown --
   --------------

   procedure Shutdown (Socket : in Socket_Type'Class) is
   begin
      Flush (Socket);
      Net.Shutdown (Socket);
   end Shutdown;

   -----------
   -- Write --
   -----------

   procedure Write
     (Socket : in Socket_Type'Class;
      Item   : in Stream_Element)
   is
      C : Cache renames Socket.Self.W_Cache;
   begin
      if C.Size = C.Max_Size then
         --  The buffer is full, write part of it

         declare
            Chunk : Stream_Element_Array (1 .. W_Cache_Chunk);
         begin
            for K in Chunk'Range loop
               Pop (C, Chunk (K));
            end loop;

            Send (Socket, Chunk);
         end;
      end if;

      Push (C, Item);
   end Write;

   procedure Write
     (Socket : in Socket_Type'Class;
      Item   : in Stream_Element_Array) is
   begin
      for K in Item'Range loop
         Write (Socket, Item (K));
      end loop;
   end Write;

end AWS.Net.Buffered;
