------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2000-2004                          --
--                               ACT-Europe                                 --
--                                                                          --
--  Authors: Dmitriy Anisimkov - Pascal Obry                                --
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

with Ada.Calendar;
with Ada.Unchecked_Conversion;
with Ada.Streams;
with Ada.Strings.Maps.Constants;

with AWS.Utils;

package body AWS.Digest is

   use Ada;
   use Ada.Streams;

   Private_Key : MD5.Context;

   type Word is mod 2 ** 16;

   Nonce_Idx   : Word := 0;
   pragma Atomic (Nonce_Idx);

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

      Now           : constant Time := Clock;
      Nonce_Time    : Time;
      Year_Now      : Year_Number;
      Month_Now     : Month_Number;
      Day_Now       : Day_Number;
      Seconds_Now   : Day_Duration;

      Index_Nonce   : String (1 .. 4);
      Seconds_Nonce : Natural;
      Ctx           : MD5.Context;
      Sample        : Digest_String;
   begin
      Split (Now, Year_Now, Month_Now, Day_Now, Seconds_Now);

      if Value'Length /= Nonce'Length then
         return False;
      end if;

      declare
         use Ada.Strings.Maps;
      begin
         if not Is_Subset
           (To_Set (String (Value (1 .. 9))), Constants.Hexadecimal_Digit_Set)
         then
            return False;
         end if;
      end;

      Seconds_Nonce := Utils.Hex_Value (String (Value (1 .. 5)));
      Index_Nonce   := Value (6 .. 9);

      Nonce_Time := Time_Of
        (Year_Now, Month_Now, Day_Now, Day_Duration (Seconds_Nonce));

      if Nonce_Time > Now then
         --  Could be next day
         Nonce_Time := Nonce_Time - Day_Duration'Last;

         Split (Nonce_Time,
                Year_Now, Month_Now, Day_Now, Seconds_Now);
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

      return String (Value (10 .. Value'Last)) = Sample;
   end Check_Nonce;

   ------------
   -- Create --
   ------------

   function Create
     (Username, Realm, Password : in String;
      Nonce, NC, CNonce, QOP    : in String;
      Method, URI               : in String)
      return Digest_String is
   begin
      return Create
        (Username => Username,
         Realm    => Realm,
         Password => Password,
         Nonce    => Nonce & ':' & NC & ':' & CNonce & ':' & QOP,
         Method   => Method,
         URI      => URI);
   end Create;

   function Create
     (Username, Realm, Password : in String;
      Nonce                     : in String;
      Method, URI               : in String)
      return Digest_String is
   begin
      return MD5.Digest
        (MD5.Digest (Username & ':' & Realm & ':' & Password)
           & ':' & Nonce & ':'
           & MD5.Digest (Method & ':' & URI));
   end Create;

   ------------------
   -- Create_Nonce --
   ------------------

   function Create_Nonce return Nonce is
      use Calendar;

      Year_Now    : Year_Number;
      Month_Now   : Month_Number;
      Day_Now     : Day_Number;
      Seconds_Now : Day_Duration;

      Seconds_Int : Natural;
      Index_Nonce : String (1 .. 4);
      Ctx         : MD5.Context;
      Result      : Digest_String;
   begin
      Split (Clock, Year_Now, Month_Now, Day_Now, Seconds_Now);

      Ctx := Private_Key;

      Seconds_Int := Natural (Float'Floor (Float (Seconds_Now)));
      Nonce_Idx   := Nonce_Idx + 1;
      Index_Nonce := Utils.Hex (Integer (Nonce_Idx), Width => 4);

      MD5.Update (Ctx, To_Byte_Array (Year_Now));
      MD5.Update (Ctx, To_Byte_Array (Month_Now));
      MD5.Update (Ctx, To_Byte_Array (Day_Now));
      MD5.Update (Ctx, To_Byte_Array (Seconds_Int));
      MD5.Update (Ctx, Index_Nonce);

      --  Place the digest string representation into the result variable

      Result := MD5.Digest (Ctx);

      --  Five hex digits before MD5 digest for the nonce expiration check

      return Nonce (Utils.Hex (Seconds_Int, Width => 5)
                      & Index_Nonce
                      & Result);
   end Create_Nonce;

begin
   MD5.Update (Private_Key, Utils.Random_Integer'Image (Utils.Random));
end AWS.Digest;
