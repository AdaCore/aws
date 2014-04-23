------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2008-2014, AdaCore                     --
--                                                                          --
--  This is free software;  you can redistribute it  and/or modify it       --
--  under terms of the  GNU General Public License as published  by the     --
--  Free Software  Foundation;  either version 3,  or (at your option) any  --
--  later version.  This software is distributed in the hope  that it will  --
--  be useful, but WITHOUT ANY WARRANTY;  without even the implied warranty --
--  of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU     --
--  General Public License for  more details.                               --
--                                                                          --
--  You should have  received  a copy of the GNU General  Public  License   --
--  distributed  with  this  software;   see  file COPYING3.  If not, go    --
--  to http://www.gnu.org/licenses for a complete copy of the license.      --
------------------------------------------------------------------------------

with Ada.Streams;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with AWS.Translator;
with AWS.Utils;

procedure Test_base64 is

   use Ada.Text_IO;
   use Ada.Strings.Unbounded;

   use AWS;

   S1 : constant String := "a" & ASCII.LF;
   S2 : constant String := "012345678" & ASCII.LF;
   S3 : constant String := "0123456789" & ASCII.LF;
   S4 : constant String := "letmein" & ASCII.LF;

   procedure Check (Name, Str : String) is
      B64 : constant String := Translator.Base64_Encode (Str);
      Std : constant String := Translator.Base64_Decode (B64);
   begin
      Put_Line (Name & "='" & B64 & ''');

      if Std /= Str then
         Put_Line ("ERROR: decoded string does not match!");
         Put_Line ("  '" & Str & "' /= '" & Std & ''');
      end if;
   end Check;

   procedure Check (Name : String; Str : Unbounded_String) is
      B64 : Unbounded_String;
      Std : Unbounded_String;
   begin
      Translator.Base64_Encode (Str, B64);
      Translator.Base64_Decode (B64, Std);

      Put_Line (Name & "='" & To_String (B64) & ''');

      if Std /= Str then
         Put_Line ("ERROR: decoded string does not match!");
         Put_Line ("  '" & To_String (Str) & "' /= '" & To_String (Std) & ''');
      end if;
   end Check;

begin
   Put_Line ("Strings:");
   Check ("S1", S1);
   Check ("S2", S2);
   Check ("S3", S3);
   Check ("S4", S4);
   Put_Line ("Unbounded_String:");
   Check ("S1", To_Unbounded_String (S1));
   Check ("S2", To_Unbounded_String (S2));
   Check ("S3", To_Unbounded_String (S3));
   Check ("S4", To_Unbounded_String (S4));

   for J in 1 .. 1024 loop
      declare
         use Ada.Streams;
         use AWS.Utils;
         use AWS.Translator;
         Bin : Stream_Element_Array (1 .. Stream_Element_Offset (J rem 256));
      begin
         for K in Bin'Range loop
            Bin (K) := Stream_Element'Mod (Random);
         end loop;

         declare
            use Ada.Strings;
            URL_Str  : constant String := Base64_Encode (Bin, URL);
            MIME_Str : constant String := Base64_Encode (Bin, MIME);
            Unb_Str1 : Unbounded_String;
            Unb_Str2 : Unbounded_String;
         begin
            if Base64_Decode (URL_Str) /= Bin then
               Put_Line ("Error " & URL_Str);
            end if;

            if Base64_Decode (MIME_Str) /= Bin then
               Put_Line ("Error " & MIME_Str);
            end if;

            Base64_Encode
              (To_Unbounded_String (URL_Str & MIME_Str), Unb_Str1, MIME);
            Base64_Encode (Unb_Str1, Unb_Str2, URL);

            Base64_Decode (Unb_Str2, Unb_Str1);
            Base64_Decode (Unb_Str1, Unb_Str2);

            if To_String (Unb_Str2) /= URL_Str & MIME_Str then
               Put_Line ("Error " & To_String (Unb_Str2));
            end if;

            if Fixed.Translate
                 (Fixed.Trim
                    (MIME_Str,
                     Left => Maps.Null_Set, Right => Maps.To_Set ('=')),
                  Maps.To_Mapping ("+/", "-_"))
               /= URL_Str
            then
               Put_Line ("URL:  " & URL_Str);
               Put_Line ("MIME: " & MIME_Str);
            end if;
         end;
      end;
   end loop;
end Test_Base64;
