------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2002-2010, AdaCore                     --
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

with AWS.Containers.Memory_Streams;
with AWS.Default;
with AWS.Translator;

package body AWS.Net.Buffered is

   CRLF : constant Stream_Element_Array :=
            Translator.To_Stream_Element_Array (ASCII.CR & ASCII.LF);

   Input_Limit : Stream_Element_Offset := AWS.Default.Input_Line_Size_Limit;
   pragma Atomic (Input_Limit);

   procedure Read (Socket : Socket_Type'Class);
   --  Refill the read-cache, the cache must be empty before the call

   function Is_Empty (C : Read_Cache) return Boolean;
   pragma Inline (Is_Empty);

   -----------
   -- Flush --
   -----------

   procedure Flush (Socket : Socket_Type'Class) is
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

   function Get_Char (Socket : Socket_Type'Class) return Character is
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

   ---------------------
   -- Get_Input_Limit --
   ---------------------

   function Get_Input_Limit return Stream_Element_Offset is
   begin
      return Input_Limit;
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

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (C : Read_Cache) return Boolean is
   begin
      return C.First > C.Last;
   end Is_Empty;

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
      C : Read_Cache renames Socket.C.R_Cache;
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
      C : Read_Cache renames Socket.C.R_Cache;
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
      C      : Read_Cache renames Socket.C.R_Cache;
      C_Last : constant Stream_Element_Offset :=
                 Stream_Element_Offset'Min (C.Last, C.First + Data'Length - 1);
   begin
      Last := Data'First + C_Last - C.First;
      Data (Data'First .. Last) := C.Buffer (C.First .. C_Last);
      C.First := C_Last + 1;
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

      function Buffered return Stream_Element_Array;
      pragma Inline (Buffered);

      procedure Finalize;

      Finalizer : Utils.Finalizer (Finalize'Access);
      pragma Unreferenced (Finalizer);

      Buffer : Stream_Type;

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

      C    : Read_Cache renames Socket.C.R_Cache;
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

               if Size (Buffer) > Input_Limit then
                  raise Data_Overflow with
                    "Size" & Stream_Element_Offset'Image (Size (Buffer));
               end if;

               begin
                  Read (Socket);
               exception
                  when E : Socket_Error =>
                     if Size (Buffer) = 0 or else Is_Timeout (E)  then
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
      Input_Limit := Stream_Element_Offset (Limit);
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
      C         : Write_Cache renames Socket.C.W_Cache;
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
