------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                          Copyright (C) 2002-2004                         --
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

with Ada.Strings.Unbounded;

with AWS.Translator;

package body AWS.Net.Buffered is

   CRLF : constant Stream_Element_Array
     := Translator.To_Stream_Element_Array (ASCII.CR & ASCII.LF);

   procedure Read (Socket : in Socket_Type'Class);
   --  Refill the read-cache, the cache must be empty before the call

   function Is_Empty (C : in Read_Cache) return Boolean;
   pragma Inline (Is_Empty);

   -----------
   -- Flush --
   -----------

   procedure Flush (Socket : in Socket_Type'Class) is
      C : Write_Cache renames Socket.C.W_Cache;
   begin
      if C.Last > 0 then
         Send (Socket, C.Buffer (1 .. C.Last));
         C.Last := 0;
      end if;
   end Flush;

   --------------
   -- Get_Char --
   --------------

   function Get_Char (Socket : in Socket_Type'Class) return Character is
      C    : Read_Cache renames Socket.C.R_Cache;
      Char : Character;
   begin
      if Is_Empty (C) then
         Read (Socket);
      end if;

      Char    := Character'Val (C.Buffer (C.First));
      C.First := C.First + 1;

      return Char;
   end Get_Char;

   --------------
   -- Get_Line --
   --------------

   function Get_Line (Socket : in Socket_Type'Class) return String is
      use Ada.Strings.Unbounded;

      Result : Unbounded_String;
      --  The final result

      Buffer : String (1 .. 256);
      pragma Warnings (Off, Buffer);
      --  Intermediate buffer
      Index  : Positive := Buffer'First;
      Char   : Character;
   begin
      Flush (Socket);

      Get_Until_LF : loop
         Char := Get_Char (Socket);

         if Char = ASCII.LF then
            Append (Result, Buffer (1 .. Index - 1));
            exit Get_Until_LF;

         elsif Char /= ASCII.CR then
            Buffer (Index) := Char;
            Index := Index + 1;

            if Index > Buffer'Last then
               Append (Result, Buffer);
               Index := Buffer'First;
            end if;
         end if;
      end loop Get_Until_LF;

      return To_String (Result);
   end Get_Line;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (C : in Read_Cache) return Boolean is
   begin
      return C.First > C.Last;
   end Is_Empty;

   --------------
   -- New_Line --
   --------------

   procedure New_Line (Socket : in Socket_Type'Class) is
   begin
      Write (Socket, CRLF);
   end New_Line;

   ---------------
   -- Peek_Char --
   ---------------

   function Peek_Char (Socket : in Socket_Type'Class) return Character is
      C : Read_Cache renames Socket.C.R_Cache;
   begin
      if Is_Empty (C) then
         Read (Socket);
      end if;

      return Character'Val (Natural (C.Buffer (C.First)));
   end Peek_Char;

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
      C : Read_Cache renames Socket.C.R_Cache;
   begin
      Receive (Socket, C.Buffer, C.Last);

      --  Reset C.First only after successful Receive, the buffer would
      --  remain empty on timeout this way.

      C.First := C.Buffer'First;
   end Read;

   procedure Read
     (Socket : in     Socket_Type'Class;
      Data   :    out Stream_Element_Array;
      Last   :    out Stream_Element_Offset)
   is
      C : Read_Cache renames Socket.C.R_Cache;
   begin
      Flush (Socket);

      if Is_Empty (C) then
         --  No more data, fill the cache
         Read (Socket);
      end if;

      declare
         C_Last : constant Stream_Element_Offset
           := Stream_Element_Offset'Min (C.Last, C.First + Data'Length - 1);
      begin
         Last := Data'First + C_Last - C.First;
         Data (Data'First .. Last) := C.Buffer (C.First .. C_Last);
         C.First := C_Last + 1;
      end;

      --  Data could remain in internal socket buffer, if there is some
      --  space on the buffer, read the socket.

      if Last < Data'Last and then Pending (Socket) > 0 then
         Receive (Socket, Data (Last + 1 .. Data'Last), Last);
      end if;
   end Read;

   function Read
     (Socket : in Socket_Type'Class;
      Max    : in Stream_Element_Count := 4096)
      return Stream_Element_Array
   is
      Buffer : Stream_Element_Array (1 .. Max);
      Last   : Stream_Element_Offset;
   begin
      Read (Socket, Buffer, Last);

      return Buffer (1 .. Last);
   end Read;

   procedure Read
     (Socket : in     Socket_Type'Class;
      Data   :    out Stream_Element_Array)
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

   --------------
   -- Shutdown --
   --------------

   procedure Shutdown (Socket : in Socket_Type'Class) is
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
     (Socket : in Socket_Type'Class;
      Item   : in Stream_Element_Array)
   is
      C : Write_Cache renames Socket.C.W_Cache;
      Next_Last : constant Stream_Element_Offset := C.Last + Item'Length;
   begin
      if Next_Last > C.Max_Size then
         Send (Socket, C.Buffer (1 .. C.Last) & Item);
         C.Last := 0;
      else
         C.Buffer (C.Last + 1 .. Next_Last) := Item;
         C.Last := Next_Last;
      end if;
   end Write;

end AWS.Net.Buffered;
