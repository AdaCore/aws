with Ada.Unchecked_Conversion;

package body MD5 is

  --====================================================================
  -- Authors   Rolf Ebert,
  --           Christoph Grein <Christ-Usch.Grein@T-Online.de>
  -- Version   1.1
  -- Date      16 January 1999
  --====================================================================
  -- This is a direct translation into Ada of the C language Reference
  -- Implementation given in the official MD5 algorithm description.
  -- It was originally written by Rolf Ebert (unknown address).
  --
  -- The official description of the MD5 algorithm can be found at
  --   <ftp://ftp.rsa.com/pub/md5.txt>
  -- License is granted by RSA Data Security, Inc. <http://www.rsa.com>
  -- to make and use derivative works provided that such works are
  -- identified as "derived from the RSA Data Security, Inc. MD5
  -- Message-Digest Algorithm" in all material mentioning or referencing
  -- the derived work. (See the copyright notice in the official
  -- description.)
  --====================================================================
  -- History
  -- Author Version   Date    Reason for change
  --  R.E.    1.0  04.06.1997 Original as found in internet
  --  C.G.    1.1  16.01.1999 Minor code changes; commented to make
  --                          publication legal
  --====================================================================

  function Rotate_Left (Value: Word; Amount: Natural) return Word;
  function Shift_Left  (Value: Word; Amount: Natural) return Word;
  function Shift_Right (Value: Word; Amount: Natural) return Word;
  pragma Import (Intrinsic, Rotate_Left);
  pragma Import (Intrinsic, Shift_Left);
  pragma Import (Intrinsic, Shift_Right);

  ------------------------------------------------------------------------
  --  F, G, H, I are the basic MD5 functions

  function F (X, Y, Z: Word) return Word is
  begin
    return (X and Y) or ((not X) and Z);
  end F;
  pragma Inline (F);

  function G (X, Y, Z: Word) return Word is
  begin
    return (X and Z) or (Y and (not Z));
  end G;
  pragma Inline (G);

  function H (X, Y, Z: Word) return Word is
  begin
    return X xor Y xor Z;
  end H;
  pragma Inline (H);

  function I (X, Y, Z: Word) return Word is
  begin
    return Y xor (X or (not Z));
  end I;
  pragma Inline (I);

  ------------------------------------------------------------------------

  procedure FF (A         : in out Word;
                B, C, D, X: in     Word;
                S         : in     Natural;
                AC        : in     Word) is
  begin
    A := A + F (B, C, D) + X + AC;
    A := Rotate_Left (A, S) + B;
  end FF;
  pragma Inline (FF);

  procedure GG (A         : in out Word;
                B, C, D, X: in     Word;
                S         : in     Natural;
                AC        : in     Word) is
  begin
    A := A + G (B, C, D) + X + AC;
    A := Rotate_Left (A, S) + B;
  end GG;
  pragma Inline (GG);

  procedure HH (A         : in out Word;
                B, C, D, X: in     Word;
                S         : in     Natural;
                AC        : in     Word) is
  begin
    A := A + H (B, C, D) + X + AC;
    A := Rotate_Left (A, S) + B;
  end HH;
  pragma Inline (HH);

  procedure II (A         : in out Word;
                B, C, D, X: in     Word;
                S         : in     Natural;
                AC        : in     Word) is
  begin
     A := A + I (B, C, D) + X + AC;
     A := Rotate_Left (A, S) + B;
  end II;
  pragma Inline (II);

  ------------------------------------------------------------------------

  procedure Encode (Output:    out Byte_Array;
                    Input : in     Word_Array) is

    J: Long_Integer := Output'First;

  begin

    for I in Input'range loop
      Output (J    ) := Byte (             Input (I)      and 16#FF#);
      Output (J + 1) := Byte (Shift_Right (Input (I),  8) and 16#FF#);
      Output (J + 2) := Byte (Shift_Right (Input (I), 16) and 16#FF#);
      Output (J + 3) := Byte (Shift_Right (Input (I), 24) and 16#FF#);
      J := J + 4;
    end loop;

  end Encode;

  procedure Decode (Output:    out Word_Array;
                    Input : in     Byte_Array) is

    J : Long_Integer := Input'First;

  begin

    for I in Output'range loop
      Output (I) :=             Word (Input (J    ))      or
                    Shift_Left (Word (Input (J + 1)),  8) or
                    Shift_Left (Word (Input (J + 2)), 16) or
                    Shift_Left (Word (Input (J + 3)), 24);
      J := J + 4;
    end loop;

  end Decode;

  ------------------------------------------------------------------------

  S11: constant :=  7;
  S12: constant := 12;
  S13: constant := 17;
  S14: constant := 22;
  S21: constant :=  5;
  S22: constant :=  9;
  S23: constant := 14;
  S24: constant := 20;
  S31: constant :=  4;
  S32: constant := 11;
  S33: constant := 16;
  S34: constant := 23;
  S41: constant :=  6;
  S42: constant := 10;
  S43: constant := 15;
  S44: constant := 21;

  procedure Transform (State: in out ABCD_State;
                       Block: in     Buffer_T) is

    A: Word := State (1);
    B: Word := State (2);
    C: Word := State (3);
    D: Word := State (4);

    X: Word_Array (0 .. 15);

  begin

    Decode (X, Block);

    -- Round 1

    FF (A, B, C, D, X ( 0), S11, 16#D76AA478#);  --  1
    FF (D, A, B, C, X ( 1), S12, 16#E8C7B756#);  --  2
    FF (C, D, A, B, X ( 2), S13, 16#242070DB#);  --  3
    FF (B, C, D, A, X ( 3), S14, 16#C1BDCEEE#);  --  4
    FF (A, B, C, D, X ( 4), S11, 16#F57C0FAF#);  --  5
    FF (D, A, B, C, X ( 5), S12, 16#4787C62A#);  --  6
    FF (C, D, A, B, X ( 6), S13, 16#A8304613#);  --  7
    FF (B, C, D, A, X ( 7), S14, 16#FD469501#);  --  8
    FF (A, B, C, D, X ( 8), S11, 16#698098D8#);  --  9
    FF (D, A, B, C, X ( 9), S12, 16#8B44F7AF#);  -- 10
    FF (C, D, A, B, X (10), S13, 16#FFFF5BB1#);  -- 11
    FF (B, C, D, A, X (11), S14, 16#895CD7BE#);  -- 12
    FF (A, B, C, D, X (12), S11, 16#6B901122#);  -- 13
    FF (D, A, B, C, X (13), S12, 16#FD987193#);  -- 14
    FF (C, D, A, B, X (14), S13, 16#A679438E#);  -- 15
    FF (B, C, D, A, X (15), S14, 16#49B40821#);  -- 16

    -- Round 2

    GG (A, B, C, D, X ( 1), S21, 16#F61E2562#);  -- 17
    GG (D, A, B, C, X ( 6), S22, 16#C040B340#);  -- 18
    GG (C, D, A, B, X (11), S23, 16#265E5A51#);  -- 19
    GG (B, C, D, A, X ( 0), S24, 16#E9B6C7AA#);  -- 20
    GG (A, B, C, D, X ( 5), S21, 16#D62F105D#);  -- 21
    GG (D, A, B, C, X (10), S22, 16#02441453#);  -- 22
    GG (C, D, A, B, X (15), S23, 16#D8A1E681#);  -- 23
    GG (B, C, D, A, X ( 4), S24, 16#E7D3FBC8#);  -- 24
    GG (A, B, C, D, X ( 9), S21, 16#21E1CDE6#);  -- 25
    GG (D, A, B, C, X (14), S22, 16#C33707D6#);  -- 26
    GG (C, D, A, B, X ( 3), S23, 16#F4D50D87#);  -- 27
    GG (B, C, D, A, X ( 8), S24, 16#455A14ED#);  -- 28
    GG (A, B, C, D, X (13), S21, 16#A9E3E905#);  -- 29
    GG (D, A, B, C, X ( 2), S22, 16#FCEFA3F8#);  -- 30
    GG (C, D, A, B, X ( 7), S23, 16#676F02D9#);  -- 31
    GG (B, C, D, A, X (12), S24, 16#8D2A4C8A#);  -- 32

    -- Round 3

    HH (A, B, C, D, X ( 5), S31, 16#FFFA3942#);  -- 33
    HH (D, A, B, C, X ( 8), S32, 16#8771F681#);  -- 34
    HH (C, D, A, B, X (11), S33, 16#6D9D6122#);  -- 35
    HH (B, C, D, A, X (14), S34, 16#FDE5380C#);  -- 36
    HH (A, B, C, D, X ( 1), S31, 16#A4BEEA44#);  -- 37
    HH (D, A, B, C, X ( 4), S32, 16#4BDECFA9#);  -- 38
    HH (C, D, A, B, X ( 7), S33, 16#F6BB4B60#);  -- 39
    HH (B, C, D, A, X (10), S34, 16#BEBFBC70#);  -- 40
    HH (A, B, C, D, X (13), S31, 16#289B7EC6#);  -- 41
    HH (D, A, B, C, X ( 0), S32, 16#EAA127FA#);  -- 42
    HH (C, D, A, B, X ( 3), S33, 16#D4EF3085#);  -- 43
    HH (B, C, D, A, X ( 6), S34, 16#04881D05#);  -- 44
    HH (A, B, C, D, X ( 9), S31, 16#D9D4D039#);  -- 45
    HH (D, A, B, C, X (12), S32, 16#E6DB99E5#);  -- 46
    HH (C, D, A, B, X (15), S33, 16#1FA27CF8#);  -- 47
    HH (B, C, D, A, X ( 2), S34, 16#C4AC5665#);  -- 48

    --  Round 4

    II (A, B, C, D, X ( 0), S41, 16#F4292244#);  -- 49
    II (D, A, B, C, X ( 7), S42, 16#432AFF97#);  -- 50
    II (C, D, A, B, X (14), S43, 16#AB9423A7#);  -- 51
    II (B, C, D, A, X ( 5), S44, 16#FC93A039#);  -- 52
    II (A, B, C, D, X (12), S41, 16#655B59C3#);  -- 53
    II (D, A, B, C, X ( 3), S42, 16#8F0CCC92#);  -- 54
    II (C, D, A, B, X (10), S43, 16#FFEFF47D#);  -- 55
    II (B, C, D, A, X ( 1), S44, 16#85845DD1#);  -- 56
    II (A, B, C, D, X ( 8), S41, 16#6FA87E4F#);  -- 57
    II (D, A, B, C, X (15), S42, 16#FE2CE6E0#);  -- 58
    II (C, D, A, B, X ( 6), S43, 16#A3014314#);  -- 59
    II (B, C, D, A, X (13), S44, 16#4E0811A1#);  -- 60
    II (A, B, C, D, X ( 4), S41, 16#F7537E82#);  -- 61
    II (D, A, B, C, X (11), S42, 16#BD3AF235#);  -- 62
    II (C, D, A, B, X ( 2), S43, 16#2AD7D2BB#);  -- 63
    II (B, C, D, A, X ( 9), S44, 16#EB86D391#);  -- 64

    State (1) := State (1) + A;
    State (2) := State (2) + B;
    State (3) := State (3) + C;
    State (4) := State (4) + D;

    --  Zeroize sensitive information.

    X := (others => 0);

  end Transform;

  ------------------------------------------------------------------------

  procedure Init (Ctx: out Context) is
  begin
    Ctx := (State  => (1 => 16#67452301#,
                       2 => 16#Efcdab89#,
                       3 => 16#98badcfe#,
                       4 => 16#10325476#),
            Count  => (others => 0),
            Buffer => (others => 0));
  end Init;

  procedure Update (Ctx: in out Context; Data: in Byte_Array) is

    Index   : Long_Integer;
    Part_Len: Long_Integer;
    I       : Long_Integer;

  begin

    -- compute number of bytes mod 64
    Index := Long_Integer (Shift_Right (Ctx.Count (1), 3) and 16#3F#);

    -- update number of bits
    Ctx.Count (1) := Ctx.Count (1) + Shift_Left (Word (Data'Length), 3);
    if Ctx.Count (1) < Shift_Left (Word (Data'Length), 3) then
      Ctx.Count (2) := Ctx.Count (2) + 1;
    end if;
    Ctx.Count (2) := Ctx.Count (2) + Shift_Right (Word (Data'Length), 29);

    Part_Len := 64 - Index;

    -- Transform as many times as possible.
    if Data'Length >= Part_Len then

      Ctx.Buffer (Index + 1 .. Index + Part_Len) :=
        Data (Data'First .. Data'First + Part_Len - 1);

      Transform (Ctx.State, Ctx.Buffer);

      I := Part_Len;
      while I + 63 < Data'Length loop
        Transform (Ctx.State, Data (I + 1 .. I + 64));
        I := I + 64;
      end loop;

      Index := 0;

    else

      I := 0;

    end if;

    -- Buffer remaining input
    Ctx.Buffer (Index + 1 .. Index + Data'Length - I) := Data (I + 1 .. Data'Length);

  end Update;

  procedure Update (Ctx: in out Context; Data: in String) is

    subtype Data_Byte_Array is Byte_Array (1 .. Data'Length);
    subtype Data_String     is String     (1 .. Data'Length);

    function String_To_Byte_Array is new Ada.Unchecked_Conversion
      (Source => Data_String,
       Target => Data_Byte_Array);

  begin

    Update (Ctx, String_To_Byte_Array (Data_String (Data)));

  end Update;

  procedure Final (Ctx: in out Context; Digest: out Fingerprint) is

    Bits      : Byte_Array (1 .. 8);
    Index     : Long_Integer;
    Pad_Length: Long_Integer;

    Padding   : constant Buffer_T := (1 => 16#80#, others => 0);

  begin

    -- save number of bits
    Encode (Bits, Ctx.Count);

    -- Pad out to 56 mod 64.
    Index := Long_Integer (Shift_Right (Ctx.Count(1), 3) and 16#3F#);
    if Index < 56 then
      Pad_Length := 56 - Index;
    else
      Pad_Length := 120 - Index;
    end if;

    Update (Ctx, Padding (1 .. Pad_Length));

    -- Append length (before padding)
    Update (Ctx, Bits);

    -- Store state in digest
    Encode (Digest, Ctx.State);

    -- Zeroize sensitive information.
    Ctx := (State  => (others => 0),
            Count  => (others => 0),
            Buffer => (others => 0));

  end Final;

  ------------------------------------------------------------------------

  Hex_Tab: constant array (0 .. 15) of Character := "0123456789abcdef";

  function Digest_From_Text (S: in Digest_String) return Fingerprint is

    Digest: Fingerprint;
    Val   : Word;
    Ch    : Character;

  begin

    for I in Digest'range loop

      Ch := S (2 * Integer (I));
      case Ch is
        when '0' .. '9' => Val := Character'Pos (Ch) - Character'Pos ('0');
        when 'a' .. 'f' => Val := Character'Pos (Ch) - Character'Pos ('a') + 10;
        when 'A' .. 'F' => Val := Character'Pos (Ch) - Character'Pos ('A') + 10;
        when others     => raise Malformed;
      end case;

      Val := Shift_Left (Val, 4);

      Ch := S (2 * Integer (I) + 1);
      case Ch is
        when '0' .. '9' => Val := Val + (Character'Pos (Ch) - Character'Pos ('0'));
        when 'a' .. 'f' => Val := Val + (Character'Pos (Ch) - Character'Pos ('a') + 10);
        when 'A' .. 'F' => Val := Val + (Character'Pos (Ch) - Character'Pos ('A') + 10);
        when others     => raise Malformed;
      end case;

      Digest (I) := Byte (Val);

    end loop;

    return Digest;

  end Digest_From_Text;

  function Digest_To_Text (A: in Fingerprint) return Digest_String is

    Str: Digest_String;
    J  : Positive;

  begin

    for I in A'range loop

      J           := 2 * Integer (I) - 1;
      Str (J)     := Hex_Tab (Natural (Shift_Right (Word (A (I)), 4)));
      Str (J + 1) := Hex_Tab (Natural (A (I) and 16#F#));

    end loop;

    return Str;

  end Digest_To_Text;

end MD5;