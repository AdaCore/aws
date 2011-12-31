------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2011-2012, AdaCore                     --
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

with Ada.Text_IO;    use Ada.Text_IO;
with Ada.Streams;    use Ada.Streams;

with AWS.Translator; use AWS.Translator;

procedure SEA2Str is

   procedure Test (D : in out Stream_Element_Array);
   --  Do test

   procedure Test (D : in out Stream_Element_Array) is
   begin
      for K in D'Range loop
         D (K) := Character'Pos (Character'Val (K + 44));
      end loop;

      declare
         S : constant String := To_String (D);
      begin
         Put_Line (Positive'Image (S'First) & '>' & To_String (D) & '<');
      end;
   end Test;

   D1 : Stream_Element_Array (-10 .. 12);
   D2 : Stream_Element_Array (5 .. 20);

begin
   Test (D1);
   Test (D2);
end SEA2Str;
