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

with Ada.Calendar;
with Ada.Streams;
with Ada.Strings.Maps.Constants;
with Ada.Unchecked_Conversion;

with AWS.Utils;

package body AWS.Digest is

   use Ada;
   use Ada.Streams;
   use GNAT;

   Private_Key : MD5.Context;

   type Word is mod 2 ** 31;

   Nonce_Idx   : Word := 0;
   pragma Atomic (Nonce_Idx);

   subtype Timestamp_String is String (1 .. 5);
   --  The timestamp string

   subtype Nonce_String is String (1 .. 8);
   --  The Nonce string is a 32 bits hex value

   Nonce_Expiration : constant Duration := 300.0;
   --  Expiration expressed in seconds

   subtype Byte_Array_Of_Integer is Stream_Element_Array
     (1 .. Integer'Size / Stream_Element_Array'Component_Size);

   function To_Byte_Array is
      new Ada.Unchecked_Conversion (Integer, Byte_Array_Of_Integer);

   -----------------
   -- Check_Nonce --
   -----------------

   function Check_Nonce (Value : in String) return Boolean is
      use Calendar;

      subtype Timestamp_Range is
        Positive range Value'First
          .. Value'First + Timestamp_String'Length - 1;

      subtype Index_Range is
        Positive range Timestamp_Range'Last + 1
          .. Timestamp_Range'Last + Nonce_String'Length;

      subtype Digest_Range is
        Positive range Index_Range'Last + 1
          .. Index_Range'Last + MD5.Message_Digest'Length;

      subtype Tag_Range is
        Positive range Timestamp_Range'First .. Index_Range'Last;
      --  This is AWS specific tag added before the MD5 digest

      Now           : constant Time := Clock;
      Nonce_Time    : Time;
      Year_Now      : Year_Number;
      Month_Now     : Month_Number;
      Day_Now       : Day_Number;
      Seconds_Now   : Day_Duration;

      Index_Nonce   : Nonce_String;
      Seconds_Nonce : Natural;
      Ctx           : MD5.Context;
      Sample        : Digest_String;
   begin
      if Value'Length /= Nonce'Length then
         return False;
      end if;

      --  Check that we have only hex digits in the timestamp and index

      declare
         use Ada.Strings.Maps;
      begin
         if not Is_Subset
           (To_Set (Value (Tag_Range)), Constants.Hexadecimal_Digit_Set)
         then
            return False;
         end if;
      end;

      Split (Now, Year_Now, Month_Now, Day_Now, Seconds_Now);

      Seconds_Nonce := Utils.Hex_Value (Value (Timestamp_Range));
      Index_Nonce   := Value (Index_Range);

      Nonce_Time := Time_Of
        (Year_Now, Month_Now, Day_Now, Day_Duration (Seconds_Nonce));

      if Nonce_Time > Now then
         --  Could be next day
         Nonce_Time := Nonce_Time - Day_Duration'Last;
         Split (Nonce_Time, Year_Now, Month_Now, Day_Now, Seconds_Now);
      end if;

      if Now - Nonce_Time > Nonce_Expiration then
         return False;
      end if;

      Ctx := Private_Key;

      MD5.Update (Ctx,
                  To_Byte_Array (Year_Now)
                    & To_Byte_Array (Month_Now)
                    & To_Byte_Array (Day_Now)
                    & To_Byte_Array (Seconds_Nonce));
      MD5.Update (Ctx, Index_Nonce);

      Sample := MD5.Digest (Ctx);

      return Value (Digest_Range) = Sample;
   end Check_Nonce;

   ------------
   -- Create --
   ------------

   function Create
     (Username, Realm, Password : in String;
      Nonce, NC, CNonce, QOP    : in String;
      Method, URI               : in String) return Digest_String is
   begin
      return MD5.Digest
        (MD5.Digest (Username & ':' & Realm & ':' & Password)
           & Tail (Nonce, NC, CNonce, QOP, Method, URI));
   end Create;

   function Create
     (Username, Realm, Password : in String;
      Nonce                     : in String;
      Method, URI               : in String) return Digest_String is
   begin
      return Create
               (Username, Realm, Password, Nonce, "", "", "", Method, URI);
   end Create;

   ------------------
   -- Create_Nonce --
   ------------------

   function Create_Nonce return Nonce is
      use Calendar;

      Year_Now      : Year_Number;
      Month_Now     : Month_Number;
      Day_Now       : Day_Number;
      Seconds_Now   : Day_Duration;

      Seconds_Int   : Natural;
      Timestamp_Str : Timestamp_String;
      Index_Str     : Nonce_String;
      Ctx           : MD5.Context;
      Result        : Digest_String;
   begin
      Split (Clock, Year_Now, Month_Now, Day_Now, Seconds_Now);

      Ctx := Private_Key;

      Seconds_Int := Natural (Float'Floor (Float (Seconds_Now)));
      Nonce_Idx   := Nonce_Idx + 1;
      Index_Str
        := Utils.Hex (Integer (Nonce_Idx), Width => Nonce_String'Length);
      Timestamp_Str
        := Utils.Hex (Seconds_Int, Width => Timestamp_String'Length);

      MD5.Update (Ctx, To_Byte_Array (Year_Now));
      MD5.Update (Ctx, To_Byte_Array (Month_Now));
      MD5.Update (Ctx, To_Byte_Array (Day_Now));
      MD5.Update (Ctx, To_Byte_Array (Seconds_Int));
      MD5.Update (Ctx, Index_Str);

      Result := MD5.Digest (Ctx);

      --  The Nonce result is composed of three parts:
      --  Five Hex Digits : timestamp to check Nonce expiration
      --  Uniq index      : to avoid generating the same Nonce twice
      --  MD5 digest      : the Nonce main MD5 data

      return Nonce (Timestamp_Str & Index_Str & Result);
   end Create_Nonce;

   ----------
   -- Tail --
   ----------

   function Tail
     (Nonce, NC, CNonce, QOP, Method, URI : in String) return String
   is
      MUD : constant Digest_String := MD5.Digest (Method & ':' & URI);
   begin
      if QOP = "" then
         return ':' & Nonce & ':' & MUD;
      else
         return ':' & Nonce & ':' & NC & ':' & CNonce & ':' & QOP & ':' & MUD;
      end if;
   end Tail;

begin
   MD5.Update (Private_Key, Utils.Random_Integer'Image (Utils.Random));
end AWS.Digest;
