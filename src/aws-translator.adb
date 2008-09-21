------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2000-2008, AdaCore                     --
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

with Interfaces;

package body AWS.Translator is

   use Ada.Streams;

   procedure Compress_Decompress
     (Stream : in out AWS.Resources.Streams.Memory.ZLib.Stream_Type'Class;
      Data   : in     Ada.Streams.Stream_Element_Array;
      Result :    out Utils.Stream_Element_Array_Access);
   --  Compress or decompress (depending on the Stream initialization)
   --  Data. Set result with the compressed or decompressed string. This is
   --  used to implement the Compress and Decompress routines.

   package Conversion is

      function To_String
        (Data : in Ada.Streams.Stream_Element_Array)
         return String;
      pragma Inline (To_String);
      --  Convert a Stream_Element_Array to a string

      function To_Stream_Element_Array
        (Data : in String)
         return Ada.Streams.Stream_Element_Array;
      pragma Inline (To_Stream_Element_Array);
      --  Convert a String to a Stream_Element_Array

   end Conversion;

   -------------------
   -- Base64_Decode --
   -------------------

   function Base64_Decode
     (B64_Data : in String)
      return Stream_Element_Array
   is
      use Interfaces;

      function Base64 (C : in Character) return Interfaces.Unsigned_32;
      pragma Inline (Base64);
      --  Returns the base64 stream element given a character

      Base64_Values : constant array (Character) of Interfaces.Unsigned_32
        := ('A' => 0, 'B' => 1, 'C' => 2, 'D' => 3, 'E' => 4, 'F' => 5,
            'G' => 6, 'H' => 7, 'I' => 8, 'J' => 9, 'K' => 10, 'L' => 11,
            'M' => 12, 'N' => 13, 'O' => 14, 'P' => 15, 'Q' => 16, 'R' => 17,
            'S' => 18, 'T' => 19, 'U' => 20, 'V' => 21, 'W' => 22, 'X' => 23,
            'Y' => 24, 'Z' => 25,

            'a' => 26, 'b' => 27, 'c' => 28, 'd' => 29, 'e' => 30, 'f' => 31,
            'g' => 32, 'h' => 33, 'i' => 34, 'j' => 35, 'k' => 36, 'l' => 37,
            'm' => 38, 'n' => 39, 'o' => 40, 'p' => 41, 'q' => 42, 'r' => 43,
            's' => 44, 't' => 45, 'u' => 46, 'v' => 47, 'w' => 48, 'x' => 49,
            'y' => 50, 'z' => 51,

            '0' => 52, '1' => 53, '2' => 54, '3' => 55, '4' => 56,
            '5' => 57, '6' => 58, '7' => 59, '8' => 60, '9' => 61,

            '+' => 62,
            '/' => 63,
            others => 16#ffffffff#);

      function Shift_Left
        (Value  : in Interfaces.Unsigned_32;
         Amount : in Natural)
         return Interfaces.Unsigned_32;
      pragma Import (Intrinsic, Shift_Left);

      function Shift_Right
        (Value  : in Interfaces.Unsigned_32;
         Amount : in Natural)
         return Interfaces.Unsigned_32;
      pragma Import (Intrinsic, Shift_Right);

      Result : Stream_Element_Array
        (Stream_Element_Offset range 1 .. B64_Data'Length);
      R      : Stream_Element_Offset := 1;

      Group  : Interfaces.Unsigned_32 := 0;
      J      : Integer := 18;

      Pad    : Stream_Element_Offset := 0;

      ------------
      -- Base64 --
      ------------

      function Base64 (C : in Character) return Interfaces.Unsigned_32 is
      begin
         pragma Assert (Base64_Values (C) < 64);
         return Base64_Values (C);
      end Base64;

   begin
      for C in B64_Data'Range loop

         if B64_Data (C) = ASCII.LF or else B64_Data (C) = ASCII.CR then
            null;

         else
            case B64_Data (C) is
               when '=' =>
                  Pad := Pad + 1;

               when others =>
                  Group := Group or Shift_Left (Base64 (B64_Data (C)), J);
            end case;

            J := J - 6;

            if J < 0 then
               Result (R .. R + 2) :=
                 (Stream_Element (Shift_Right (Group and 16#FF0000#, 16)),
                  Stream_Element (Shift_Right (Group and 16#00FF00#, 8)),
                  Stream_Element (Group and 16#0000FF#));

               R := R + 3;

               Group := 0;
               J     := 18;
            end if;

         end if;
      end loop;

      return Result (1 .. R - 1 - Pad);
   end Base64_Decode;

   -------------------
   -- Base64_Encode --
   -------------------

   function Base64_Encode (Data : in Stream_Element_Array) return String is

      function Shift_Left
        (Value  : in Stream_Element;
         Amount : in Natural)
         return Stream_Element;
      pragma Import (Intrinsic, Shift_Left);

      function Shift_Right
        (Value  : in Stream_Element;
         Amount : in Natural)
         return Stream_Element;
      pragma Import (Intrinsic, Shift_Right);

      Encoded_Length : constant Integer := 4 * ((Data'Length + 2) / 3);

      Result         : String (1 .. Encoded_Length);
      Last           : Integer := Result'First - 1;
      State          : Positive range 1 .. 3 := 1;
      E, Prev_E      : Stream_Element := 0;

      Base64 : constant array (Stream_Element range 0 .. 63) of Character
        := ('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M',
            'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z',
            'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm',
            'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z',
            '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
            '+', '/');

   begin
      for C in Data'Range loop
         E := Data (C);

         Last := Last + 1;

         case State is
            when 1 =>
               Result (Last) := Base64 (Shift_Right (E, 2) and 16#3F#);
               State := 2;

            when 2 =>
               Result (Last) := Base64 ((Shift_Left (Prev_E, 4) and 16#30#)
                                        or (Shift_Right (E, 4) and 16#F#));
               State := 3;

            when 3 =>
               Result (Last) := Base64 ((Shift_Left (Prev_E, 2) and 16#3C#)
                                        or (Shift_Right (E, 6) and 16#3#));
               Last := Last + 1;
               Result (Last) := Base64 (E and 16#3F#);
               State := 1;
         end case;

         Prev_E := E;
      end loop;

      case State is
         when 1 =>
            null;
         when 2 =>
            Last := Last + 1;
            Result (Last) := Base64 (Shift_Left (Prev_E, 4) and 16#30#);
         when 3 =>
            Last := Last + 1;
            Result (Last) := Base64 (Shift_Left (Prev_E, 2) and 16#3C#);
      end case;

      pragma Assert ((Result'Last - Last) < 3);
      Result (Last + 1 .. Result'Last) := (others => '=');
      return Result;
   end Base64_Encode;

   function Base64_Encode (Data : in String) return String is
      Stream_Data : constant Stream_Element_Array
        := To_Stream_Element_Array (Data);
   begin
      return Base64_Encode (Stream_Data);
   end Base64_Encode;

   --------------
   -- Compress --
   --------------

   function Compress
     (Data   : in Ada.Streams.Stream_Element_Array;
      Level  : in Compression_Level                := Default_Compression;
      Header : in ZL.Header_Type                   := ZL.Default_Header)
      return Utils.Stream_Element_Array_Access
   is
      Stream : ZL.Stream_Type;
      Result : Utils.Stream_Element_Array_Access;
   begin
      ZL.Deflate_Initialize (Stream, Level => Level, Header => Header);

      Compress_Decompress (Stream, Data, Result);
      return Result;
   end Compress;

   -------------------------
   -- Compress_Decompress --
   -------------------------

   procedure Compress_Decompress
     (Stream : in out AWS.Resources.Streams.Memory.ZLib.Stream_Type'Class;
      Data   : in     Ada.Streams.Stream_Element_Array;
      Result :    out Utils.Stream_Element_Array_Access)
   is
      use ZL;

      Chunk_Size  : constant := 4_096;
      --  This variable must be kept with a reasonable value as a chunk is
      --  allocated on the stack.

      First, Last : Stream_Element_Offset;
   begin
      --  Add Data content to the stream for compression/decompression

      First := Data'First;

      loop
         Last := Stream_Element_Offset'Min (Data'Last, First + Chunk_Size);

         Append (Stream, Data (First .. Last));

         exit when Last = Data'Last;

         First := Last + 1;
      end loop;

      --  Read back the data

      Result := new Stream_Element_Array (1 .. Size (Stream));

      declare
         Buffer : Stream_Element_Array (1 .. Chunk_Size);
         K      : Stream_Element_Offset := 1;
      begin
         while not End_Of_File (Stream) loop
            Read (Stream, Buffer, Last);
            Result (K .. K + Last - 1) := Buffer (1 .. Last);
            K := K + Last;
         end loop;
      end;

      --  Close the stream, it will release all associated memory

      Close (Stream);
   end Compress_Decompress;

   ----------------
   -- Conversion --
   ----------------

   package body Conversion is separate;

   ----------------
   -- Decompress --
   ----------------

   function Decompress
     (Data   : in Ada.Streams.Stream_Element_Array;
      Header : in ZL.Header_Type                   := ZL.Default_Header)
      return Utils.Stream_Element_Array_Access
   is
      Stream : ZL.Stream_Type;
      Result : Utils.Stream_Element_Array_Access;
   begin
      ZL.Inflate_Initialize (Stream, Header => Header);

      Compress_Decompress (Stream, Data, Result);
      return Result;
   end Decompress;

   ---------------
   -- QP_Decode --
   ---------------

   function QP_Decode (QP_Data : in String) return String is

      End_Of_QP : constant String := "00";
      K         : Positive := QP_Data'First;
      Result    : String (1 .. QP_Data'Length);
      R         : Natural := 0;
   begin
      loop
         if QP_Data (K) = '=' then
            if K + 1 <= QP_Data'Last and then QP_Data (K + 1) = ASCII.CR then
               K := K + 1;

            elsif K + 2 <= QP_Data'Last then
               declare
                  Hex : constant String := QP_Data (K + 1 .. K + 2);
               begin
                  if Hex /= End_Of_QP then
                     R := R + 1;
                     Result (R) := Character'Val (Utils.Hex_Value (Hex));
                  end if;

                  K := K + 2;
               end;
            end if;

         else
            R := R + 1;
            Result (R) := QP_Data (K);
         end if;

         K := K + 1;

         exit when K > QP_Data'Last;
      end loop;

      return Result (1 .. R);
   end QP_Decode;

   -----------------------------
   -- To_Stream_Element_Array --
   -----------------------------

   function To_Stream_Element_Array
     (Data : in String)
      return Stream_Element_Array
      renames Conversion.To_Stream_Element_Array;

   ---------------
   -- To_String --
   ---------------

   function To_String
     (Data : in Stream_Element_Array)
      return String
      renames Conversion.To_String;

   -------------------------
   -- To_Unbounded_String --
   -------------------------

   function To_Unbounded_String
     (Data : in Ada.Streams.Stream_Element_Array)
      return Ada.Strings.Unbounded.Unbounded_String
   is
      use Ada.Strings.Unbounded;

      Chunk_Size : constant := 1_024;
      Result     : Unbounded_String;
      K          : Stream_Element_Offset := Data'First;
   begin
      while K <= Data'Last loop
         declare
            Last : constant Stream_Element_Offset
              := Stream_Element_Offset'Min (K + Chunk_Size, Data'Last);
         begin
            Append (Result, To_String (Data (K .. Last)));
            K := K + Chunk_Size + 1;
         end;
      end loop;

      return Result;
   end To_Unbounded_String;

end AWS.Translator;
