------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2008-2012, AdaCore                     --
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

with Ada.Strings.Unbounded;
with Ada.Text_IO;

with AWS.Translator;

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
end Test_Base64;
