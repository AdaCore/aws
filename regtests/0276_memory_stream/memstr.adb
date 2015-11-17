------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                       Copyright (C) 2015, AdaCore                        --
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
with Ada.Text_IO;

with Memory_Streams;

procedure MemStr is

   use Ada;
   use Ada.Streams;

   type Stream_Access is access Stream_Element_Array;
   type Constant_Stream_Access is access constant Stream_Element_Array;

   package Mem is new Memory_Streams
     (Stream_Element, Stream_Element_Offset, Stream_Element_Array,
      Stream_Access, Constant_Stream_Access);

   procedure Display (S : Mem.Stream_Type);
   --  Display S status

   -------------
   -- Display --
   -------------

   procedure Display (S : Mem.Stream_Type) is
      use Text_IO;
   begin
      Put_Line ("size" & Stream_Element_Offset'Image (Mem.Size (S)));
      Put_Line ("pending" & Stream_Element_Offset'Image (Mem.Pending (S)));
   end Display;

   S      : Mem.Stream_Type;
   Buffer : Stream_Element_Array (1 .. 10);
   Read   : Stream_Element_Array (1 .. 4);
   Last   : Stream_Element_Offset;

begin
   for K in Buffer'Range loop
      Buffer (K) := Stream_Element (K);
   end loop;

   Mem.Append (S, Buffer);
   Display (S);

   Mem.Read (S, Read (1 .. 2), Last);
   Display (S);

   if Read (1 .. 2) /= Buffer (1 .. 2) then
      Text_IO.Put_Line ("1. Read failed!");
   end if;

   Mem.Read (S, Read (1 .. 4), Last);
   Display (S);

   if Read /= Buffer (3 .. 6) then
      Text_IO.Put_Line ("2. Read failed!");
   end if;

   for K in 1 .. 10_000 loop
      Mem.Append (S, Buffer);
   end loop;

   Display (S);

   Mem.Read (S, Read (1 .. 3), Last);
   Display (S);

   if Read (1 .. 3) /= Buffer (7 .. 9) then
      Text_IO.Put_Line ("3. Read failed!");
   end if;

   Mem.Reset (S);
   Display (S);

   Mem.Close (S);
   Display (S);
end MemStr;
