--  (C) Copyright 2000 by John Halleck, All Rights Reserved.
--  Convert SHA digests to printable strings.
--  This is part of a project at http://www.cc.utah.edu/~nahaj/

package body SHA.Strings is

   Hex : constant String := "0123456789ABCDEF";

   function Hex_From_SHA (Given : Digest) return Hex_SHA_String is
      Where  : Integer range Hex_SHA_String'Range := Hex_SHA_String'First;
      Result : Hex_SHA_String := (others => '*');
      Temp   : Unsigned_32;
   begin
      for I in Digest'Range loop -- For each word
         Temp := Given (I);
         for J in reverse 0 .. 8 - 1 loop
            Result (Where + J) := Hex (Integer (Temp and 16#F#) + 1);
            Temp := Shift_Right (Temp, 4);
         end loop;
         if I /= Digest'Last then
            Where := Where + 8;
         end if;
      end loop;
      return Result;
   end Hex_From_SHA;


   B64 : constant String :=
      "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

   --  Network Base 64 encoding.  (But we don't put the "=" on the end.)
   --  Note that their are four bytes output for each 3 in.
   function B64_From_SHA (Given : Digest) return B64_SHA_String is
      type Unsigned_6 is mod 2 ** 6;
      Where     : Integer range B64_SHA_String'Range := B64_SHA_String'First;
      Result    : B64_SHA_String := (others => '=');
      Bits      : Integer range 0 .. 6 := 0; -- Bits over the 6 needed.
      Saved     : Unsigned_6;
      Bits_8    : constant := 16#FF#;
      Bits_6    : constant := 16#3F#;
      This_Byte : Unsigned_8;
      This_Word : Unsigned_32;
   begin
      Where := Hex_SHA_String'First;
      Bits  := 0;  -- Extra bits over the 6 we need for this character.
      Saved := 0;  -- None yet.
      for I in Digest'Range loop  -- For each word

         This_Word := Given (I);

         for J in 1 .. 32/8 loop  -- and each byte in the word...
            This_Word := Rotate_Left (This_Word, 8);
            This_Byte := Unsigned_8 (This_Word and Bits_8);
            Bits := Bits + 2;

            --  Convert Current Byte;
            Result (Where) := B64 (Integer (
                           Unsigned_6 (Shift_Right (This_Byte, Bits)) or Saved
                           ) + 1);

            --  And set up for next one.
            Saved := Unsigned_6 (Shift_Left (This_Byte, 6 - Bits) and Bits_6);
            if Where /= B64_SHA_String'Last then
               Where := Where + 1;
            end if;

            --  Time for the extra output byte?
            if Bits = 6 then
               Result (Where) := B64 (Natural (Saved) + 1);
               Bits  := 0;
               Saved := 0;
               if Where /= B64_SHA_String'Last then
                  Where := Where + 1;
               end if;
            end if;

         end loop;

      end loop;

      if Bits /= 0 then
         Result (Where) := B64 (Natural (Saved) + 1);
      end if;

      return Result;
   end B64_From_SHA;


end SHA.Strings;
