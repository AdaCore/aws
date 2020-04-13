------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2000-2020, AdaCore                     --
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

with Interfaces;

package body AWS.Translator is

   use Interfaces;

   procedure Compress_Decompress
     (Stream : in out Resources.Streams.Memory.ZLib.Stream_Type'Class;
      Data   : Stream_Element_Array;
      Result : out Utils.Stream_Element_Array_Access);
   --  Compress or decompress (depending on the Stream initialization)
   --  Data. Set result with the compressed or decompressed string. This is
   --  used to implement the Compress and Decompress routines.

   type Base64_Encode_Array is array (Unsigned_8 range 0 .. 63) of Character;

   type Decoding_State is record
      Pad   : Unsigned_32 := 0;
      Group : Unsigned_32 := 0;
      J     : Integer := 18;
   end record;

   type Encoding_State is record
      Last          : Integer := 0;
      To_Char       : access constant Base64_Encode_Array;
      Current_State : Positive range 1 .. 3 := 1;
      Prev_E        : Unsigned_8 := 0; -- position of the character
      Count         : Integer := 0;
   end record;

   procedure Add
     (Add   : not null access procedure (Ch : Character);
      State : in out Encoding_State;
      Ch    : Character);
   --  This method contains the algorithm for encoding the characters

   procedure Add
     (Add   : not null access procedure (Ch : Character);
      State : in out Decoding_State;
      Ch    : Character);
   --  This method contains the algorithm for decoding the characters

   procedure Flush
     (Add   : not null access procedure (Ch : Character);
      State : in out Encoding_State);
   --  This method is implemented for flushing to add the last bits

   procedure Flush
     (Add   : not null access procedure (Ch : Character);
      State : Decoding_State);
   --  This method is implemented for flushing to add the last bits when
   --  padding is absent in Base64URL coding

   package Conversion is

      function To_String (Data : Stream_Element_Array) return String
        with Inline;
      --  Convert a Stream_Element_Array to a string

      function To_Stream_Element_Array
        (Data : String) return Stream_Element_Array with Inline;
      --  Convert a String to a Stream_Element_Array

      function To_Stream_Element_Array
        (Data : String) return Utils.Stream_Element_Array_Access with Inline;
      --  As above but designed for large objects

   end Conversion;

   ----------------------------
   -- Base64 Decode / Encode --
   ----------------------------

   --  The base64 character set

   Base64 : constant array (Base64_Mode) of aliased Base64_Encode_Array :=
             (MIME =>
              ('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M',
               'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z',
               'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm',
               'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z',
               '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
               '+', '/'),
              URL =>
              ('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M',
               'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z',
               'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm',
               'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z',
               '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
               '-', '_'));

   --  The base64 values

   Base64_Values : constant array (Character) of Unsigned_32 :=
                     ('A' => 0, 'B' => 1, 'C' => 2, 'D' => 3, 'E' => 4,
                      'F' => 5, 'G' => 6, 'H' => 7, 'I' => 8, 'J' => 9,
                      'K' => 10, 'L' => 11, 'M' => 12, 'N' => 13, 'O' => 14,
                      'P' => 15, 'Q' => 16, 'R' => 17, 'S' => 18, 'T' => 19,
                      'U' => 20, 'V' => 21, 'W' => 22, 'X' => 23, 'Y' => 24,
                      'Z' => 25,

                      'a' => 26, 'b' => 27, 'c' => 28, 'd' => 29, 'e' => 30,
                      'f' => 31, 'g' => 32, 'h' => 33, 'i' => 34, 'j' => 35,
                      'k' => 36, 'l' => 37, 'm' => 38, 'n' => 39, 'o' => 40,
                      'p' => 41, 'q' => 42, 'r' => 43, 's' => 44, 't' => 45,
                      'u' => 46, 'v' => 47, 'w' => 48, 'x' => 49, 'y' => 50,
                      'z' => 51, '0' => 52, '1' => 53, '2' => 54, '3' => 55,
                      '4' => 56, '5' => 57, '6' => 58, '7' => 59, '8' => 60,
                      '9' => 61, '+' => 62, '/' => 63, '-' => 62, '_' => 63,
                      others => 16#ffffffff#);

   ---------
   -- Add --
   ---------

   procedure Add
     (Add   : not null access procedure (Ch : Character);
      State : in out Encoding_State;
      Ch    : Character)
   is
      E : Unsigned_8 := 0;
   begin
      State.Count := State.Count + 1;
      E := Character'Pos (Ch);

      State.Last := State.Last + 1;

      case State.Current_State is
         when 1 =>
            Add (State.To_Char (Shift_Right (E, 2) and 16#3F#));
            State.Current_State := 2;

         when 2 =>
            Add (State.To_Char ((Shift_Left (State.Prev_E, 4) and 16#30#)
              or (Shift_Right (E, 4) and 16#F#)));
            State.Current_State := 3;

         when 3 =>
            Add (State.To_Char ((Shift_Left (State.Prev_E, 2) and 16#3C#)
              or (Shift_Right (E, 6) and 16#3#)));
            State.Last := State.Last + 1;
            Add (State.To_Char (E and 16#3F#));
            State.Current_State := 1;
      end case;

      State.Prev_E := E;
   end Add;

   procedure Add
     (Add   : not null access procedure (Ch : Character);
      State : in out Decoding_State;
      Ch    : Character) is
   begin
      if Ch /= '=' and then Base64_Values (Ch) = 16#ffffffff# then
         null;

      else
         case Ch is
            when '=' =>
               State.Pad := State.Pad + 1;

            when others =>
               State.Group := State.Group
                 or Shift_Left (Base64_Values (Ch), State.J);
         end case;

         State.J := State.J - 6;

         if State.J < 0 then
            Add (Character'Val (Shift_Right (State.Group and 16#FF0000#, 16)));
            Add (Character'Val (Shift_Right (State.Group and 16#00FF00#, 8)));
            Add (Character'Val (State.Group and 16#0000FF#));

            State.Group := 0;
            State.J     := 18;
         end if;

      end if;
   end Add;

   -------------------
   -- Base64_Decode --
   -------------------

   procedure Base64_Decode
     (B64_Data : Base64_UString;
      Data     : out Unbounded_String)
   is

      procedure Add_Char (Ch : Character);
      --  Append a single char into Data

      --------------
      -- Add_Char --
      --------------

      procedure Add_Char (Ch : Character) is
      begin
         Append (Data, Ch);
      end Add_Char;

      S : Decoding_State;

   begin
      Data := Null_Unbounded_String;

      for C in 1 .. Length (B64_Data) loop
         Add (Add_Char'Access, S, Element (B64_Data, C));
      end loop;

      --  Remove the padding
      if S.Pad /= 0 then
         Delete
           (Data,
            From    => Length (Data) - Positive (S.Pad) + 1,
            Through => Length (Data));

      else
         --  Padding is absent in Base64URL coding

         Flush (Add_Char'Access, S);
      end if;
   end Base64_Decode;

   function Base64_Decode
     (B64_Data : Base64_String) return Stream_Element_Array
   is

      procedure Add_Char (Ch : Character);
      --  Add single char into Result

      Result : Stream_Element_Array
        (Stream_Element_Offset range 1 .. B64_Data'Length);
      Last   : Stream_Element_Offset := 0;
      S      : Decoding_State;

      --------------
      -- Add_Char --
      --------------

      procedure Add_Char (Ch : Character) is
      begin
         Last := Last + 1;
         Result (Last) := Character'Pos (Ch);
      end Add_Char;

   begin
      for Char of B64_Data loop
         Add (Add_Char'Access, S, Char);
      end loop;

      Flush (Add_Char'Access, S);

      return Result (1 .. Last - Stream_Element_Offset (S.Pad));
   end Base64_Decode;

   function Base64_Decode (B64_Data : Base64_String) return String is
   begin
      return To_String (Base64_Decode (B64_Data));
   end Base64_Decode;

   -------------------
   -- Base64_Encode --
   -------------------

   procedure Base64_Encode
     (Data     : Unbounded_String;
      B64_Data : out Base64_UString;
      Mode     : Base64_Mode := MIME)
   is

      procedure Add_Char (Ch : Character);
      --  Add single char into Base64

      --------------
      -- Add_Char --
      --------------

      procedure Add_Char (Ch : Character) is
      begin
         Append (B64_Data, Ch);
      end Add_Char;

      S : Encoding_State;

   begin
      S.To_Char := Base64 (Mode)'Access;

      B64_Data := Null_Unbounded_String;

      for C in 1 .. Length (Data) loop
         Add (Add_Char'Access, S, Element (Data, C));
      end loop;

      Flush (Add_Char'Access, S);
   end Base64_Encode;

   function Base64_Encode
     (Data : Stream_Element_Array;
      Mode : Base64_Mode := MIME) return Base64_String
   is

      procedure Add_Char (Ch : Character);
      --  Add single char into result string

      Result : Unbounded_String;
      S      : Encoding_State;

      --------------
      -- Add_Char --
      --------------

      procedure Add_Char (Ch : Character) is
      begin
         Append (Result, Ch);
      end Add_Char;

   begin
      S.To_Char := Base64 (Mode)'Access;

      for Elem of Data loop
         Add (Add_Char'Access, S, Character'Val (Elem));
      end loop;

      Flush (Add_Char'Access, S);
      return To_String (Result);
   end Base64_Encode;

   function Base64_Encode
     (Data : String; Mode : Base64_Mode := MIME) return Base64_String
   is
      Stream_Data : constant Stream_Element_Array :=
                      To_Stream_Element_Array (Data);
   begin
      return Base64_Encode (Stream_Data, Mode);
   end Base64_Encode;

   --------------
   -- Compress --
   --------------

   function Compress
     (Data   : Stream_Element_Array;
      Level  : Compression_Level            := Default_Compression;
      Header : ZL.Header_Type               := ZL.Default_Header)
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
     (Stream : in out Resources.Streams.Memory.ZLib.Stream_Type'Class;
      Data   : Stream_Element_Array;
      Result : out Utils.Stream_Element_Array_Access)
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
     (Data   : Stream_Element_Array;
      Header : ZL.Header_Type                   := ZL.Default_Header)
      return Utils.Stream_Element_Array_Access
   is
      Stream : ZL.Stream_Type;
      Result : Utils.Stream_Element_Array_Access;
   begin
      ZL.Inflate_Initialize (Stream, Header => Header);

      Compress_Decompress (Stream, Data, Result);
      return Result;
   end Decompress;

   -----------
   -- Flush --
   -----------

   procedure Flush
     (Add   : not null access procedure (Ch : Character);
      State : in out Encoding_State)
   is
      Encoded_Length : Integer;
      Base_Elem      : constant access constant Base64_Encode_Array :=
                         Base64 (MIME)'Access;
   begin
      case State.Current_State is
         when 1 =>
            null;

         when 2 =>
            State.Last := State.Last + 1;
            Add (State.To_Char (Shift_Left (State.Prev_E, 4) and 16#30#));

         when 3 =>
            State.Last := State.Last + 1;
            Add (State.To_Char (Shift_Left (State.Prev_E, 2) and 16#3C#));
      end case;

      --  Add Additional '=' character for the missing bits

      State.Last := State.Last + 1;

      Encoded_Length := 4 * ((State.Count + 2) / 3);

      if State.To_Char = Base_Elem then
         for I in State.Last .. Encoded_Length loop
            Add ('=');
         end loop;
      end if;
   end Flush;

   procedure Flush
     (Add   : not null access procedure (Ch : Character);
      State : Decoding_State) is
   begin
      if State.J <= 12 then
         Add (Character'Val (Shift_Right (State.Group and 16#FF0000#, 16)));
      end if;

      if State.J = 0 then
         Add (Character'Val (Shift_Right (State.Group and 16#00FF00#, 8)));
      end if;
   end Flush;

   ---------------
   -- QP_Decode --
   ---------------

   function QP_Decode (QP_Data : String) return String is

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
     (Data : String)
      return Stream_Element_Array
      renames Conversion.To_Stream_Element_Array;

   function To_Stream_Element_Array
     (Data : String)
      return Utils.Stream_Element_Array_Access
      renames Conversion.To_Stream_Element_Array;

   ---------------
   -- To_String --
   ---------------

   function To_String
     (Data : Stream_Element_Array)
      return String
      renames Conversion.To_String;

   -------------------------
   -- To_Unbounded_String --
   -------------------------

   function To_Unbounded_String
     (Data : Stream_Element_Array) return Unbounded_String
   is
      Chunk_Size : constant := 1_024;
      Result     : Unbounded_String;
      K          : Stream_Element_Offset := Data'First;
   begin
      while K <= Data'Last loop
         declare
            Last : constant Stream_Element_Offset :=
                     Stream_Element_Offset'Min (K + Chunk_Size, Data'Last);
         begin
            Append (Result, To_String (Data (K .. Last)));
            K := K + Chunk_Size + 1;
         end;
      end loop;

      return Result;
   end To_Unbounded_String;

end AWS.Translator;
