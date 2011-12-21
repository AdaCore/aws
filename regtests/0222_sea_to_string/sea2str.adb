------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                       Copyright (C) 2011, AdaCore                        --
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
