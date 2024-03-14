------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2000-2014, AdaCore                     --
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

with Ada.Real_Time;
with Ada.Streams;
with Ada.Strings.Maps.Constants;
with Ada.Unchecked_Conversion;

with AWS.Translator;
with AWS.Utils;

package body AWS.Digest is

   use Ada;
   use Ada.Streams;
   use GNAT;

   Private_Key : MD5.Context;

   type Modular24_Bits is mod 2 ** 24;
   for Modular24_Bits'Size use 24;

   Nonce_Idx : Modular24_Bits := Modular24_Bits'Mod (Utils.Random) with Atomic;

   subtype Timestamp_String is String (1 .. 4);
   --  The timestamp string is a 24 bits Base64 encoded value

   subtype Index_String is String (1 .. 4);
   --  The Nonce string is a 24 bits Base64 encoded value

   Nonce_Expiration : constant := 300;
   --  Expiration expressed in seconds

   subtype Byte_Array_Of_Modular24 is Stream_Element_Array
     (1 .. Modular24_Bits'Size / Stream_Element_Array'Component_Size);

   subtype Byte_Array_Of_Seconds is Stream_Element_Array
     (1 .. Real_Time.Seconds_Count'Size / Stream_Element_Array'Component_Size);

   function To_Byte_Array is
      new Ada.Unchecked_Conversion
            (Real_Time.Seconds_Count, Byte_Array_Of_Seconds);

   function To_Byte_Array is
      new Ada.Unchecked_Conversion (Modular24_Bits, Byte_Array_Of_Modular24);

   function To_Modular24 is
      new Ada.Unchecked_Conversion (Byte_Array_Of_Modular24, Modular24_Bits);

   -----------------
   -- Check_Nonce --
   -----------------

   function Check_Nonce (Value : String) return Boolean is
      use Real_Time;

      subtype Timestamp_Range is
        Positive range Value'First
          .. Value'First + Timestamp_String'Length - 1;

      subtype Index_Range is
        Positive range Timestamp_Range'Last + 1
          .. Timestamp_Range'Last + Index_String'Length;

      subtype Digest_Range is
        Positive range Index_Range'Last + 1
          .. Index_Range'Last + MD5.Message_Digest'Length;

      Time_Dif      : Modular24_Bits;
      Seconds_Now   : Seconds_Count;
      TS            : Time_Span;
      Index_Nonce   : Index_String;
      Seconds_Array : Byte_Array_Of_Modular24;
      Seconds_Nonce : Modular24_Bits;
      Ctx           : MD5.Context;
      Sample        : Digest_String;

   begin
      if Value'Length /= Nonce'Length then
         return False;
      end if;

      --  Check that we have only base64 digits in the timestamp and index
      --  and hex in the digest.

      declare
         use Ada.Strings.Maps;
         Base64_Set : constant Character_Set :=
           To_Set (Character_Range'(Low => 'a', High => 'z'))
           or To_Set (Character_Range'(Low => 'A', High => 'Z'))
           or To_Set (Character_Range'(Low => '0', High => '9'))
           or To_Set ("+/");
      begin
         if not Is_Subset
           (To_Set (Value (Timestamp_Range'First .. Index_Range'Last)),
            Base64_Set)
           or else not
             Is_Subset
               (To_Set (Value (Digest_Range)), Constants.Hexadecimal_Digit_Set)
         then
            return False;
         end if;
      end;

      Split (Clock, Seconds_Now, TS);

      Seconds_Array := Translator.Base64_Decode (Value (Timestamp_Range));
      Seconds_Nonce := To_Modular24 (Seconds_Array);
      Index_Nonce := Value (Index_Range);

      Time_Dif := Modular24_Bits'Mod (Seconds_Now) - Seconds_Nonce;

      if Time_Dif > Nonce_Expiration then
         return False;
      end if;

      Ctx := Private_Key;

      MD5.Update (Ctx, To_Byte_Array (Seconds_Now - Seconds_Count (Time_Dif)));
      MD5.Update (Ctx, Index_Nonce);

      Sample := MD5.Digest (Ctx);

      return Value (Digest_Range) = Sample;
   end Check_Nonce;

   ------------
   -- Create --
   ------------

   function Create
     (Username, Realm, Password : String;
      Nonce, NC, CNonce, QOP    : String;
      Method, URI               : String) return Digest_String is
   begin
      return MD5.Digest
        (MD5.Digest (Username & ':' & Realm & ':' & Password)
           & Tail (Nonce, NC, CNonce, QOP, Method, URI));
   end Create;

   function Create
     (Username, Realm, Password : String;
      Nonce                     : String;
      Method, URI               : String) return Digest_String is
   begin
      return Create
        (Username, Realm, Password, Nonce, "", "", "", Method, URI);
   end Create;

   ------------------
   -- Create_Nonce --
   ------------------

   function Create_Nonce return Nonce is
      use Real_Time;
      Seconds_Now   : Seconds_Count;
      TS            : Time_Span;
      Timestamp_Str : Timestamp_String;
      Index_Str     : Index_String;
      Ctx           : MD5.Context;
   begin
      Split (Clock, Seconds_Now, TS);

      Ctx := Private_Key;

      Nonce_Idx := Nonce_Idx + 1;
      Index_Str := Translator.Base64_Encode (To_Byte_Array (Nonce_Idx));
      Timestamp_Str :=
        Translator.Base64_Encode
          (To_Byte_Array (Modular24_Bits'Mod (Seconds_Now)));

      MD5.Update (Ctx, To_Byte_Array (Seconds_Now));
      MD5.Update (Ctx, Index_Str);

      --  The Nonce result is composed of three parts:
      --  Five Hex Digits : timestamp to check Nonce expiration
      --  Uniq index      : to avoid generating the same Nonce twice
      --  MD5 digest      : the Nonce main MD5 data

      return Nonce (Timestamp_Str & Index_Str & MD5.Digest (Ctx));
   end Create_Nonce;

   ----------
   -- Tail --
   ----------

   function Tail
     (Nonce, NC, CNonce, QOP, Method, URI : String) return String is
   begin
      return ':' & Nonce
         & (if QOP = "" then "" else ':' & NC & ':' & CNonce & ':' & QOP & ':')
         & MD5.Digest (Method & ':' & URI);
   end Tail;

begin
   MD5.Update (Private_Key, Utils.Random_String (32));
end AWS.Digest;
