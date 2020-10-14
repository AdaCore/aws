------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                      Copyright (C) 2020, AdaCore                         --
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

with Ada.Exceptions;
with Ada.Streams.Stream_IO;
with Ada.Text_IO;

with Zlib.Streams;

procedure ZLib_Deflate_Empty is

   use Ada;
   use Ada.Exceptions;

   package SIO renames Ada.Streams.Stream_IO;
   package ZS renames Zlib.Streams;

   F : SIO.File_Type;
   S : SIO.Stream_Access;

   Z : ZS.Stream_Type;

   I : Ada.Streams.Stream_Element_Array (1 .. 1);
   L : Ada.Streams.Stream_Element_Offset;
begin
   SIO.Open (F, SIO.In_File, "invalid.gz");
   S := SIO.Stream (F);
   ZS.Create (Stream          => Z,
              Mode            => ZS.In_Stream,
              Back            => ZS.Stream_Access (S),
              Back_Compressed => True,
              Header          => ZLib.Auto);
   ZS.Read (Z, I, L);
exception
   when E : others =>
      Text_IO.Put_Line ("OK: " & Exception_Message (E));
end ZLib_Deflate_Empty;
