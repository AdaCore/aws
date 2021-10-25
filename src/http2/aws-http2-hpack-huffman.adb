------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                      Copyright (C) 2021, AdaCore                         --
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

with Interfaces;

package body AWS.HTTP2.HPACK.Huffman is

   use Interfaces;

   type Node;
   type Node_Access is access Node;

   subtype Bit is Stream_Element range 0 .. 1;

   type Children is array (Bit) of aliased Node_Access;

   type Node (Leaf : Boolean) is record
      case Leaf is
         when True  => C  : Character;
         when False => LR : Children;
      end case;
   end record;

   Root : aliased Node_Access;

   procedure Create_Tree;
   --  Create the fast huffman decoding tree Root (called at elaboration time)

   function Decode_Bit
    (Iter  : in out Node_Access;
     Code  : Bit;
     Value : out Character) return Boolean;
   --  Walk the tree at Root and returns True when a leaf has been reached.
   --  This is used to decode a set of bit and found the actual encoded
   --  character using the binary tree Root.

   type Code is record
      Bits  : Unsigned_32;           -- Huffman code
      N_Bit : Integer range 1 .. 30; -- Number of bits
   end record;

   Table : constant array (Unsigned_16 range 0 .. 256) of Code :=
             ((16#1ff8#,     13),
              (16#7fffd8#,   23),
              (16#fffffe2#,  28),
              (16#fffffe3#,  28),
              (16#fffffe4#,  28),
              (16#fffffe5#,  28),
              (16#fffffe6#,  28),
              (16#fffffe7#,  28),
              (16#fffffe8#,  28),
              (16#ffffea#,   24),
              (16#3ffffffc#, 30),
              (16#fffffe9#,  28),
              (16#fffffea#,  28),
              (16#3ffffffd#, 30),
              (16#fffffeb#,  28),
              (16#fffffec#,  28),
              (16#fffffed#,  28),
              (16#fffffee#,  28),
              (16#fffffef#,  28),
              (16#ffffff0#,  28),
              (16#ffffff1#,  28),
              (16#ffffff2#,  28),
              (16#3ffffffe#, 30),
              (16#ffffff3#,  28),
              (16#ffffff4#,  28),
              (16#ffffff5#,  28),
              (16#ffffff6#,  28),
              (16#ffffff7#,  28),
              (16#ffffff8#,  28),
              (16#ffffff9#,  28),
              (16#ffffffa#,  28),
              (16#ffffffb#,  28),
              (16#14#,        6),
              (16#3f8#,      10),
              (16#3f9#,      10),
              (16#ffa#,      12),
              (16#1ff9#,     13),
              (16#15#,        6),
              (16#f8#,        8),
              (16#7fa#,      11),
              (16#3fa#,      10),
              (16#3fb#,      10),
              (16#f9#,        8),
              (16#7fb#,      11),
              (16#fa#,        8),
              (16#16#,        6),
              (16#17#,        6),
              (16#18#,        6),
              (16#0#,         5),
              (16#1#,         5),
              (16#2#,         5),
              (16#19#,        6),
              (16#1a#,        6),
              (16#1b#,        6),
              (16#1c#,        6),
              (16#1d#,        6),
              (16#1e#,        6),
              (16#1f#,        6),
              (16#5c#,        7),
              (16#fb#,        8),
              (16#7ffc#,     15),
              (16#20#,        6),
              (16#ffb#,      12),
              (16#3fc#,      10),
              (16#1ffa#,     13),
              (16#21#,        6),
              (16#5d#,        7),
              (16#5e#,        7),
              (16#5f#,        7),
              (16#60#,        7),
              (16#61#,        7),
              (16#62#,        7),
              (16#63#,        7),
              (16#64#,        7),
              (16#65#,        7),
              (16#66#,        7),
              (16#67#,        7),
              (16#68#,        7),
              (16#69#,        7),
              (16#6a#,        7),
              (16#6b#,        7),
              (16#6c#,        7),
              (16#6d#,        7),
              (16#6e#,        7),
              (16#6f#,        7),
              (16#70#,        7),
              (16#71#,        7),
              (16#72#,        7),
              (16#fc#,        8),
              (16#73#,        7),
              (16#fd#,        8),
              (16#1ffb#,     13),
              (16#7fff0#,    19),
              (16#1ffc#,     13),
              (16#3ffc#,     14),
              (16#22#,        6),
              (16#7ffd#,     15),
              (16#3#,         5),
              (16#23#,        6),
              (16#4#,         5),
              (16#24#,        6),
              (16#5#,         5),
              (16#25#,        6),
              (16#26#,        6),
              (16#27#,        6),
              (16#6#,         5),
              (16#74#,        7),
              (16#75#,        7),
              (16#28#,        6),
              (16#29#,        6),
              (16#2a#,        6),
              (16#7#,         5),
              (16#2b#,        6),
              (16#76#,        7),
              (16#2c#,        6),
              (16#8#,         5),
              (16#9#,         5),
              (16#2d#,        6),
              (16#77#,        7),
              (16#78#,        7),
              (16#79#,        7),
              (16#7a#,        7),
              (16#7b#,        7),
              (16#7ffe#,     15),
              (16#7fc#,      11),
              (16#3ffd#,     14),
              (16#1ffd#,     13),
              (16#ffffffc#,  28),
              (16#fffe6#,    20),
              (16#3fffd2#,   22),
              (16#fffe7#,    20),
              (16#fffe8#,    20),
              (16#3fffd3#,   22),
              (16#3fffd4#,   22),
              (16#3fffd5#,   22),
              (16#7fffd9#,   23),
              (16#3fffd6#,   22),
              (16#7fffda#,   23),
              (16#7fffdb#,   23),
              (16#7fffdc#,   23),
              (16#7fffdd#,   23),
              (16#7fffde#,   23),
              (16#ffffeb#,   24),
              (16#7fffdf#,   23),
              (16#ffffec#,   24),
              (16#ffffed#,   24),
              (16#3fffd7#,   22),
              (16#7fffe0#,   23),
              (16#ffffee#,   24),
              (16#7fffe1#,   23),
              (16#7fffe2#,   23),
              (16#7fffe3#,   23),
              (16#7fffe4#,   23),
              (16#1fffdc#,   21),
              (16#3fffd8#,   22),
              (16#7fffe5#,   23),
              (16#3fffd9#,   22),
              (16#7fffe6#,   23),
              (16#7fffe7#,   23),
              (16#ffffef#,   24),
              (16#3fffda#,   22),
              (16#1fffdd#,   21),
              (16#fffe9#,    20),
              (16#3fffdb#,   22),
              (16#3fffdc#,   22),
              (16#7fffe8#,   23),
              (16#7fffe9#,   23),
              (16#1fffde#,   21),
              (16#7fffea#,   23),
              (16#3fffdd#,   22),
              (16#3fffde#,   22),
              (16#fffff0#,   24),
              (16#1fffdf#,   21),
              (16#3fffdf#,   22),
              (16#7fffeb#,   23),
              (16#7fffec#,   23),
              (16#1fffe0#,   21),
              (16#1fffe1#,   21),
              (16#3fffe0#,   22),
              (16#1fffe2#,   21),
              (16#7fffed#,   23),
              (16#3fffe1#,   22),
              (16#7fffee#,   23),
              (16#7fffef#,   23),
              (16#fffea#,    20),
              (16#3fffe2#,   22),
              (16#3fffe3#,   22),
              (16#3fffe4#,   22),
              (16#7ffff0#,   23),
              (16#3fffe5#,   22),
              (16#3fffe6#,   22),
              (16#7ffff1#,   23),
              (16#3ffffe0#,  26),
              (16#3ffffe1#,  26),
              (16#fffeb#,    20),
              (16#7fff1#,    19),
              (16#3fffe7#,   22),
              (16#7ffff2#,   23),
              (16#3fffe8#,   22),
              (16#1ffffec#,  25),
              (16#3ffffe2#,  26),
              (16#3ffffe3#,  26),
              (16#3ffffe4#,  26),
              (16#7ffffde#,  27),
              (16#7ffffdf#,  27),
              (16#3ffffe5#,  26),
              (16#fffff1#,   24),
              (16#1ffffed#,  25),
              (16#7fff2#,    19),
              (16#1fffe3#,   21),
              (16#3ffffe6#,  26),
              (16#7ffffe0#,  27),
              (16#7ffffe1#,  27),
              (16#3ffffe7#,  26),
              (16#7ffffe2#,  27),
              (16#fffff2#,   24),
              (16#1fffe4#,   21),
              (16#1fffe5#,   21),
              (16#3ffffe8#,  26),
              (16#3ffffe9#,  26),
              (16#ffffffd#,  28),
              (16#7ffffe3#,  27),
              (16#7ffffe4#,  27),
              (16#7ffffe5#,  27),
              (16#fffec#,    20),
              (16#fffff3#,   24),
              (16#fffed#,    20),
              (16#1fffe6#,   21),
              (16#3fffe9#,   22),
              (16#1fffe7#,   21),
              (16#1fffe8#,   21),
              (16#7ffff3#,   23),
              (16#3fffea#,   22),
              (16#3fffeb#,   22),
              (16#1ffffee#,  25),
              (16#1ffffef#,  25),
              (16#fffff4#,   24),
              (16#fffff5#,   24),
              (16#3ffffea#,  26),
              (16#7ffff4#,   23),
              (16#3ffffeb#,  26),
              (16#7ffffe6#,  27),
              (16#3ffffec#,  26),
              (16#3ffffed#,  26),
              (16#7ffffe7#,  27),
              (16#7ffffe8#,  27),
              (16#7ffffe9#,  27),
              (16#7ffffea#,  27),
              (16#7ffffeb#,  27),
              (16#ffffffe#,  28),
              (16#7ffffec#,  27),
              (16#7ffffed#,  27),
              (16#7ffffee#,  27),
              (16#7ffffef#,  27),
              (16#7fffff0#,  27),
              (16#3ffffee#,  26),
              (16#3fffffff#, 30));

   -----------------
   -- Create_Tree --
   -----------------

   procedure Create_Tree is
      type Word_Bits is array (0 .. 31) of Bit with Pack;

      procedure Insert_Item (K : Unsigned_16);

      -----------------
      -- Insert_Item --
      -----------------

      procedure Insert_Item (K : Unsigned_16) is
         C    : constant Code := Table (K);
         H    : Unsigned_32 := C.Bits;
         Bits : Word_Bits with Size => 32, Address => H'Address;

         Iter : access Node_Access := Root'Access;
      begin
         for K in reverse 0 .. C.N_Bit - 1 loop
            declare
               B : constant Bit := Bits (K);
            begin
               if Iter.all = null then
                  Iter.all := new Node'(False, LR => (null, null));
               end if;

               declare
                  I : constant Node_Access := Iter.all;
               begin
                  Iter := I.LR (B)'Access;
               end;
            end;
         end loop;

         if K = 256 then
            Iter.all := new Node'(True, ASCII.NUL);
         else
            Iter.all := new Node'(True, Character'Val (K));
         end if;
      end Insert_Item;

   begin
      for K in Table'Range loop
         Insert_Item (K);
      end loop;
   end Create_Tree;

   ------------
   -- Decode --
   ------------

   function Decode (Str : Stream_Element_Array) return String is
      --  ??? to be checked if 3 is an upper limit
      Result : String (1 .. Positive (Str'Length) * 3);
      I      : Natural := 0;

      EOS    : constant Unsigned_32 := 16#fffffffa#;

      type Byte_Bits is array (0 .. 7) of Bit with Pack;
      Iter    : Node_Access := Root;
      Padding : Natural := 0;
      Pad_0   : Boolean := False;
      V       : Unsigned_32 := 0;

   begin
      for K in Str'Range loop
         declare
            E    : constant Stream_Element := Str (K);
            Bits : Byte_Bits with Size => 8, Address => E'Address;
            C    : Character;
         begin
            --  Keep last four bytes to check for EOS

            V :=  Shift_Left (V, 8) or Unsigned_32 (E);

            --  Check for an EOS not at the end of the string

            if (V and EOS) = EOS and then K /= Str'Last then
               raise Protocol_Error with
                 Exception_Message (C_Compression_Error, "found EOS");
            end if;

            for B in reverse 0 .. 7 loop
               declare
                  Bit : constant Huffman.Bit := Bits (B);
               begin
                  if Decode_Bit (Iter, Bit, C) then
                     I := I + 1;
                     Result (I) := C;
                     Padding := 0;
                     Pad_0 := False;

                  else
                     Padding := Padding + 1;
                     Pad_0 := Pad_0 or else (Bit = 0);
                  end if;
               end;
            end loop;
         end;
      end loop;

      if Padding > 7 then
         raise Protocol_Error
           with Exception_Message
             (C_Compression_Error, "more than 7 bits of padding");
      elsif Pad_0 then
         raise Protocol_Error
           with Exception_Message
             (C_Compression_Error, "padding with 0");
      end if;

      return Result (1 .. I);
   end Decode;

   ----------------
   -- Decode_Bit --
   ----------------

   function Decode_Bit
    (Iter  : in out Node_Access;
     Code  : Bit;
     Value : out Character) return Boolean is
   begin
      Iter := Iter.LR (Code);

      if Iter.Leaf then
         Value := Iter.C;
         Iter := Root;
         return True;
      else
         return False;
      end if;
   end Decode_Bit;

   ------------
   -- Encode --
   ------------

   function Encode (Str : String) return Stream_Element_Array is
      --  ??? to be checked if 3 is the upper limit
      R : Stream_Element_Array (1 .. Stream_Element_Offset (Str'Length + 3));
      I : Stream_Element_Offset := 0; -- current index on R
      V : Stream_Element := 0;        -- current value
      B : Integer := 8;               -- free bit on V

      procedure Push_Byte;
      --  Push byte V into R

      procedure Encode (C : Character);
      --  Encode single character C

      ------------
      -- Encode --
      ------------

      procedure Encode (C : Character) is
         P : constant Interfaces.Unsigned_16 := Character'Pos (C);
         H : constant Code := Table (P);
         N : Integer := H.N_Bit; -- number of remaining bits
      begin
         Store_Bits : loop
            if B >= N then
               --  Enough room for all the bits
               declare
                  T : Unsigned_32 := H.Bits;
               begin
                  T := Shift_Left (H.Bits, B - N);
                  T := T and (2 ** B - 1);
                  V := V or Stream_Element (T);

                  B := B - N;
               end;

               if B = 0 then
                  Push_Byte;
               end if;

               exit Store_Bits;

            else
               --  Not enough room for all the bits
               --  We must take B out of N.
               declare
                  T : Unsigned_32 := H.Bits;
               begin
                  T := Shift_Right (T, N - B);
                  T := T and (2 ** B - 1);
                  V := V or Stream_Element (T);

                  N := N - B;

                  Push_Byte;
               end;
            end if;
         end loop Store_Bits;
      end Encode;

      ---------------
      -- Push_Byte --
      ---------------

      procedure Push_Byte is
      begin
         I := I + 1;
         R (I) := V;
         V := 0;
         B := 8;
      end Push_Byte;

   begin
      for C of Str loop
         Encode (C);
      end loop;

      --  Push remaing bytes if any

      if B /= 8 then
         --  We set all remaing bits to 1
         V := V or Stream_Element (2 ** B - 1);
         Push_Byte;
      end if;

      return R (1 .. I);
   end Encode;

begin
   Create_Tree;
end AWS.HTTP2.HPACK.Huffman;
