------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2000-2001                          --
--                                ACT-Europe                                --
--                                                                          --
--  Authors: Dmitriy Anisimov - Pascal Obry                                 --
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

with Ada.Characters.Handling;
with Ada.Strings.Unbounded;

with Interfaces;

package body AWS.Translator is

   use Ada;
   use Ada.Strings.Unbounded;

   ----------------
   -- Decode_URL --
   ----------------

   function Decode_URL (Str : in String) return String is
      I, K   : Positive := Str'First;
      Result : String (Str'Range);
   begin
      while I <= Str'Last loop
         if Str (I) = '+' then
            Result (K) := ' ';
            I := I + 1;

         elsif Str (I) = '%'
           and then I + 2 <= Str'Last
           and then Characters.Handling.Is_Hexadecimal_Digit (Str (I + 1))
           and then Characters.Handling.Is_Hexadecimal_Digit (Str (I + 2))
         then
            declare
               Hex_Num : constant String := "16#" & Str (I + 1 .. I + 2) & '#';
            begin
               Result (K) := Character'Val (Natural'Value (Hex_Num));
               I := I + 3;
            end;

         else
            Result (K) := Str (I);
            I := I + 1;
         end if;

         K := K + 1;
      end loop;

      return Result (Result'First .. K - 1);
   end Decode_URL;

   -------------------
   -- Base64_Encode --
   -------------------

   function Base64_Encode
     (Data : in Streams.Stream_Element_Array)
     return String
   is
      use Streams;
      use type Streams.Stream_Element;

      function Base64 (E : in Stream_Element) return Character;
      --  returns the base64 character given a number

      function Shift_Left (Value  : in Stream_Element;
                           Amount : in Natural) return Stream_Element;
      pragma Import (Intrinsic, Shift_Left);

      function Shift_Right (Value  : in Stream_Element;
                            Amount : in Natural) return Stream_Element;
      pragma Import (Intrinsic, Shift_Right);

      Result : Unbounded_String;
      Length : Natural := 0;
      State  : Positive range 1 .. 3 := 1;
      E, Old : Stream_Element := 0;

      function Base64 (E : in Stream_Element) return Character is
         V : Natural := Natural (E);
      begin
         if V in 0 .. 25 then
            return Character'Val (V + Character'Pos ('A'));
         elsif V in 26 .. 51 then
            return Character'Val (V - 26 + Character'Pos ('a'));
         elsif V in 52 .. 61 then
            return Character'Val (V - 52 + Character'Pos ('0'));
         elsif V = 62 then
            return '+';
         else
            return '/';
         end if;
      end Base64;

   begin
      for C in Data'Range loop
         E := Data (C);

         case State is
            when 1 =>
               Append (Result, Base64 (Shift_Right (E, 2) and 16#3F#));
               State := 2;
            when 2 =>
               Append (Result, Base64 ((Shift_Left (Old, 4) and 16#30#)
                                        or (Shift_Right (E, 4) and 16#F#)));
               State := 3;
            when 3 =>
               Append (Result, Base64 ((Shift_Left (Old, 2) and 16#3C#)
                                       or (Shift_Right (E, 6) and 16#3#)));
               Append (Result, Base64 (E and 16#3F#));
               State := 1;
         end case;

         Old := E;

         Length := Length + 1;

         if Length >= 72 then
            Append (Result, ASCII.LF);
            Length := 0;
         end if;
      end loop;

      case State is
         when 1 =>
            null;
         when 2 =>
            Append (Result, Base64 (Shift_Left (Old, 4) and 16#30#) & "==");
         when 3 =>
            Append (Result, Base64 (Shift_Left (Old, 2) and 16#3C#) & '=');
      end case;

      return To_String (Result);
   end Base64_Encode;

   function Base64_Encode (Data : in String) return String is
      use type Streams.Stream_Element_Offset;
      Stream_Data : Streams.Stream_Element_Array
        (1 .. Streams.Stream_Element_Offset (Data'Length));
      I : Streams.Stream_Element_Offset := 1;
   begin
      for K in Data'Range loop
         Stream_Data (I) := Character'Pos (Data (K));
         I := I + 1;
      end loop;
      return Base64_Encode (Stream_Data);
   end Base64_Encode;

   -------------------
   -- Base64_Decode --
   -------------------

   function Base64_Decode (B64_Data : in String)
                          return Streams.Stream_Element_Array
   is
      use Streams;
      use type Interfaces.Unsigned_32;
      use type Streams.Stream_Element_Offset;

      function Base64 (C : in Character) return Interfaces.Unsigned_32;
      --  returns the base64 stream element given a character

      function Shift_Left (Value  : in Interfaces.Unsigned_32;
                           Amount : in Natural) return Interfaces.Unsigned_32;
      pragma Import (Intrinsic, Shift_Left);

      function Shift_Right (Value  : in Interfaces.Unsigned_32;
                            Amount : in Natural) return Interfaces.Unsigned_32;
      pragma Import (Intrinsic, Shift_Right);

      Result : Stream_Element_Array
        (Stream_Element_Offset range 1 .. B64_Data'Length);
      R      : Stream_Element_Offset := 1;

      Group  : Interfaces.Unsigned_32 := 0;
      J      : Integer := 18;

      Pad    : Stream_Element_Offset := 0;

      function Base64 (C : in Character) return Interfaces.Unsigned_32 is
      begin
         if C in 'A' .. 'Z' then
            return Character'Pos (C) - Character'Pos ('A');
         elsif C in 'a' .. 'z' then
            return Character'Pos (C) - Character'Pos ('a') + 26;
         elsif C in '0' .. '9' then
            return Character'Pos (C) - Character'Pos ('0') + 52;
         elsif C = '+' then
            return 62;
         else
            return 63;
         end if;
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

   -----------------------------
   -- To_Stream_Element_Array --
   -----------------------------

   function To_Stream_Element_Array
     (Data : in String)
     return Streams.Stream_Element_Array
   is
      use Streams;

      Result : Stream_Element_Array
        (Stream_Element_Offset (Data'First)
         .. Stream_Element_Offset (Data'Last));
   begin
      for K in Data'Range loop
         Result (Stream_Element_Offset (K)) := Character'Pos (Data (K));
      end loop;
      return Result;
   end To_Stream_Element_Array;

   ---------------
   -- To_String --
   ---------------

   function To_String
     (Data : in Streams.Stream_Element_Array)
     return String
   is
      Result : String (Integer (Data'First) .. Integer (Data'Last));
   begin
      for K in Data'Range loop
         Result (Integer (K)) := Character'Val (Data (K));
      end loop;
      return Result;
   end To_String;

end AWS.Translator;
