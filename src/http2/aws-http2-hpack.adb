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

with AWS.HTTP2.Connection;
with AWS.HTTP2.HPACK.Huffman;
with AWS.HTTP2.HPACK.Table;
with AWS.Translator;
with AWS.Utils;

package body AWS.HTTP2.HPACK is

   subtype Bit1 is Stream_Element range 0 .. 1;
   subtype Bit2 is Stream_Element range 0 .. 3;
   subtype Bit3 is Stream_Element range 0 .. 7;
   subtype Bit4 is Stream_Element range 0 .. 15;
   subtype Bit8 is Stream_Element range 0 .. 255;

   type Group_Size is (G1, G2, G3, G4);

   type RFC_Byte (Group : Group_Size := Group_Size'First) is record
      case Group is
         when G1 =>
            B0 : Bit1;
            B1 : Bit1;
            B2 : Bit1;
            B3 : Bit1;
            B4 : Bit1;
            B5 : Bit1;
            B6 : Bit1;
            B7 : Bit1;

         when G2 =>
            B20 : Bit2;
            B21 : Bit2;
            B22 : Bit2;
            B23 : Bit2;

         when G3 =>
            B30 : Bit3;

         when G4 =>
            B40 : Bit4;
            B41 : Bit4;
      end case;
   end record with Size => 8, Unchecked_Union;

   for RFC_Byte use record
      B0 at 0 range 7 .. 7;
      B1 at 0 range 6 .. 6;
      B2 at 0 range 5 .. 5;
      B3 at 0 range 4 .. 4;
      B4 at 0 range 3 .. 3;
      B5 at 0 range 2 .. 2;
      B6 at 0 range 1 .. 1;
      B7 at 0 range 0 .. 0;
      --
      B20 at 0 range 6 .. 7;
      B21 at 0 range 4 .. 5;
      B22 at 0 range 2 .. 3;
      B23 at 0 range 0 .. 1;
      --
      B30 at 0 range 5 .. 7;
      --
      B40 at 0 range 4 .. 7;
      B41 at 0 range 0 .. 3;
   end record;

   B_II          : constant Bit2 := 2#01#;
   --  Incremental Indexing

   B_II_No_Indexing : constant Bit4 := 2#0001#;
   --  Incremental Indexing

   B_No_Indexing : constant Bit4 := 0;
   --  No Indexing

   B_Dyn_Table   : constant Bit3 := 2#001#;

   ------------
   -- Decode --
   ------------

   function Decode
     (Table    : not null access HPACK.Table.Object;
      Settings : not null access HTTP2.Connection.Object)
      return AWS.Headers.List
   is
      Byte : Bit8;
      Bit  : RFC_Byte (G1) with Address => Byte'Address;
      BG2  : RFC_Byte (G2) with Address => Byte'Address;
      BG3  : RFC_Byte (G3) with Address => Byte'Address;
      BG4  : RFC_Byte (G4) with Address => Byte'Address;

      Headers : AWS.Headers.List;

      subtype Int_Bit is Positive range 1 .. 7;

      function Get_Integer (N : Int_Bit) return Stream_Element_Count;
      --  Get an integer for which the first part is encoded into N bits. The
      --  integger is possible encoded on multiple bytes.
      --  RFC 7541 / 5.1

      function Get_String_Literal return String;
      --  Get a string literal (either plain or huffaman encoded)
      --  RFC 7541 / 5.2

      procedure Get_Indexed_Name_Value (Idx : Stream_Element_Count);
      --  As above but Idx is referencing a Name & Value pair in the table
      --  RFC 7541 / 6.1

      function Get_Indexed_Name (Idx : Stream_Element_Count) return String;
      --  Get an indexed name from the HPACK static/dynamic table
      --  RFC 7541 / 6.2

      ----------------------
      -- Get_Indexed_Name --
      ----------------------

      function Get_Indexed_Name (Idx : Stream_Element_Count) return String is
      begin
         return HPACK.Table.Get_Name (Table.all, Positive (Idx));
      end Get_Indexed_Name;

      ----------------------------
      -- Get_Indexed_Name_Value --
      ----------------------------

      procedure Get_Indexed_Name_Value (Idx : Stream_Element_Count) is
         I : constant HPACK.Table.Name_Value :=
               HPACK.Table.Get_Name_Value (Table.all, Positive (Idx));
      begin
         AWS.Headers.Add (Headers, I.Name, I.Value);
      end Get_Indexed_Name_Value;

      -----------------
      -- Get_Integer --
      -----------------

      function Get_Integer (N : Int_Bit) return Stream_Element_Count is
         use type Interfaces.Unsigned_32;

         Mask              : constant Bit8 := 2 ** N - 1;
         Continuation_Mask : constant Bit8 := 2#00111_1111#;
         Result            : Interfaces.Unsigned_32 := 0;
         B                 : Bit8 := Byte and Mask;
         K                 : Natural := 0;
         Stop              : Boolean;
      begin
         Result := Interfaces.Unsigned_32 (B);

         --  Check if this is a case where the integer is span on
         --  multiple bytes (RFC-7541 / 5.1).

         if B = Mask then
            loop
               B := Get_Byte;
               Stop := (B and 2#1000_0000#) = 0;
               B := B and Continuation_Mask;
               Result := Result + (2 ** K) * Interfaces.Unsigned_32 (B);
               K := K + 7;
               exit when Stop;
            end loop;
         end if;

         return Stream_Element_Count (Result);
      end Get_Integer;

      ------------------------
      -- Get_String_Literal --
      ------------------------

      function Get_String_Literal return String is
      begin
         --  String Literal (RFC-7541 / 5.2)
         --
         --     0   1   2   3   4   5   6   7
         --   +---+---+---+---+---+---+---+---+
         --   | H |    String Length (7+)     |
         --   +---+---------------------------+
         --   |  String Data (Length octets)  |
         --   +-------------------------------+

         Byte := Get_Byte;

         declare
            Length : constant Stream_Element_Count := Get_Integer (7);
            Str    : Stream_Element_Array
                       (1 .. Stream_Element_Offset (Length));
         begin
            for K in Str'Range loop
               Str (K) := Get_Byte;
            end loop;

            if Bit.B0 = 1 then
               --  Huffman encode
               return Huffman.Decode (Str);

            else
               --  Plain literal
               return Translator.To_String (Str);
            end if;
         end;
      end Get_String_Literal;

      Idx : Stream_Element_Offset;

   begin
      while not End_Of_Stream loop
         Byte := Get_Byte;

         if Bit.B0 = 1 then
            --  Indexed header field (RFC-7541 / 6.1)
            --
            --    0   1   2   3   4   5   6   7
            --  +---+---+---+---+---+---+---+---+
            --  | 1 |        Index (7+)         |
            --  +---+---------------------------+

            Idx := Get_Integer (7);

            if Idx = 0 then
               --  RFC 7541 - 6.1
               raise Constraint_Error with "indexed header field is 0";
            end if;

            Get_Indexed_Name_Value (Idx);

         elsif BG2.B20 = B_II then
            --  Incremental indexing - Indexed name (RFC-7541 / 6.2.1)
            --
            --    0   1   2   3   4   5   6   7
            --  +---+---+---+---+---+---+---+---+
            --  | 0 | 1 |      Index (6+)       |
            --  +---+---+-----------------------+
            --
            --
            --  or
            --
            --     0   1   2   3   4   5   6   7
            --   +---+---+---+---+---+---+---+---+
            --   | 0 | 1 |           0           |
            --   +---+---+-----------------------+

            Idx := Get_Integer (6);

            declare
               Name  : constant String :=
                         (if Idx = 0
                          then Get_String_Literal
                          else Get_Indexed_Name (Idx));
               Value : constant String := Get_String_Literal;
            begin
               Table.Insert (Settings, Name, Value);
               AWS.Headers.Add (Headers, Name, Value);
            end;

         elsif BG3.B30 = B_Dyn_Table then
            --  Dynamic Table Size Update (RFC-7541 / 6.3)
            --
            --    0   1   2   3   4   5   6   7
            --  +---+---+---+---+---+---+---+---+
            --  | 0 | 0 | 1 |   Max size (5+)   |
            --  +---+---------------------------+

            Idx := Get_Integer (5);

            if Natural (Idx) > Settings.Header_Table_Size then
               --  This is a decoding error
               raise Protocol_Error with "error dynamic table update";
            else
               Settings.Set_Dynamic_Header_Table_Size (Natural (Idx));
            end if;

         elsif BG4.B40 = B_II_No_Indexing then
            --  Literal Header Field Never Indexed (RFC-7541 / 6.2.3)
            --
            --     0   1   2   3   4   5   6   7
            --   +---+---+---+---+---+---+---+---+
            --   | 0 | 0 | 0 | 1 |  Index (4+)   |
            --   +---+---+-----------------------+
            --
            --  or
            --
            --     0   1   2   3   4   5   6   7
            --   +---+---+---+---+---+---+---+---+
            --   | 0 | 0 | 0 | 1 |     0         |
            --   +---+---+-----------------------+

            Idx := Get_Integer (4);

            --  Get the name/value

            declare
               Name  : constant String :=
                         (if Idx = 0
                          then Get_String_Literal
                          else Get_Indexed_Name (Idx));
               Value : constant String := Get_String_Literal;
            begin
               AWS.Headers.Add (Headers, Name, Value);
            end;

         elsif BG4.B40 = B_No_Indexing then
            --  No indexing - Indexed Name (RFC-7541 / 6.2.2)
            --
            --     0   1   2   3   4   5   6   7
            --   +---+---+---+---+---+---+---+---+
            --   | 0 | 0 | 0 | 0 |  Index (4+)   |
            --   +---+---+-----------------------+
            --
            --  or
            --
            --     0   1   2   3   4   5   6   7
            --   +---+---+---+---+---+---+---+---+
            --   | 0 | 0 | 0 | 0 |     0         |
            --   +---+---+-----------------------+

            Idx := Get_Integer (4);

            declare
               Name  : constant String :=
                         (if Idx = 0
                          then Get_String_Literal
                          else Get_Indexed_Name (Idx));
               Value : constant String := Get_String_Literal;
            begin
               AWS.Headers.Add (Headers, Name, Value);
            end;

         else
            raise Protocol_Error with "hpack unknown data : " & Byte'Img;
         end if;

         Table.Dump;
      end loop;

      return Headers;
   end Decode;

   ------------
   -- Encode --
   ------------

   function Encode
     (Table    : not null access HPACK.Table.Object;
      Settings : not null access Connection.Object;
      List     : Headers.List) return Stream_Element_Array
   is
      pragma Unreferenced (Settings);

      Res : Utils.Stream_Element_Array_Access :=
              new Stream_Element_Array (1 .. 10000);
      I   : Stream_Element_Offset := 0;

      procedure Append (E : Stream_Element);
      --  Append E into result

      procedure Send (Str : String);
      --  Record Str into result

      ------------
      -- Append --
      ------------

      procedure Append (E : Stream_Element) is
      begin
         I := I + 1;

         if I > Res'Last then
            declare
               N : constant Utils.Stream_Element_Array_Access :=
                     new Stream_Element_Array (1 .. Res'Last * 2);
            begin
               N (1 .. Res'Last) := Res.all;
               Utils.Unchecked_Free (Res);
               Res := N;
            end;
         end if;

         Res (I) := E;
      end Append;

      ----------
      -- Send --
      ----------

      procedure Send (Str : String) is
         H : Bit8;
         S : Stream_Element_Array (1 .. 1) with Address => H'Address;
      begin
         --  ??? if Str'Length > 127 (7 bits) we need to encode the
         --  ??? integer on multiple bytes (5.1).
         --  ??? also Str is never huffman encoded

         H := Str'Length;
         Append (S (1));

         for K in Str'Range loop
            Append (Stream_Element (Character'Pos (Str (K))));
         end loop;
      end Send;

      H : Bit8;
      S : Stream_Element_Array (1 .. 1) with Address => H'Address;

   begin
      for K in 1 .. List.Count loop
         declare
            Name  : constant String := List.Get_Name (K);
            Value : constant String := List.Get_Value (K);
            Both  : Boolean := False;
         begin
            H := Stream_Element
                   (Table.Get_Name_Value_Index
                      (Name, Value, Both => Both));

            H := H or (if Both
                       then 2#1000_0000#   --  RFC 7541 / 6.1
                       else 2#0100_0000#); --  RFC 7541 / 6.2

            Append (S (1));

            --  If we have a value and the indexing above was only for the
            --  name we need to send the value.

            if not Both and then Value /= "" then
               Send (Value);
            end if;
         end;
      end loop;

      return R : Stream_Element_Array (1 .. I) do
         R (1 .. I) := Res (1 .. I);
         Utils.Unchecked_Free (Res);
      end return;
   end Encode;

end AWS.HTTP2.HPACK;
