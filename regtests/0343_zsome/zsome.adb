------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                       Copyright (C) 2021, AdaCore                        --
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

with AWS.Resources.Streams.Memory.ZLib;
with AWS.Utils;
with Memory_Some;
with ZLib.Streams;

procedure ZSome is

   use Ada.Streams;

   Back   : aliased Memory_Some.Stream_Type;
   Stream : ZLib.Streams.Stream_Type;
   Sample : Stream_Element_Array (0 .. 255);
   Buffer : Stream_Element_Array (Sample'Range);
   Last   : Stream_Element_Offset;
   Count  : constant := 50000;

   procedure Print (Message : String) renames Ada.Text_IO.Put_Line;

begin
   for J in Sample'Range loop
      Sample (J) := Stream_Element (J);
   end loop;

   Stream.Create
     (Mode            => ZLib.Streams.Duplex,
      Back            => Back'Unchecked_Access,
      Back_Compressed => True);

   for J in 1 .. Count loop
      for K in Sample'Range loop
         Buffer (K) := Sample (K) xor Stream_Element'Mod (J);
      end loop;

      Stream.Write (Buffer);
   end loop;

   Stream.Flush (ZLib.Finish);

   for J in 1 .. Count loop
      Stream.Read (Buffer, Last);

      if Last < Buffer'Last then
         Print ("Wrong Last" & Last'Img);
      end if;

      for K in Sample'Range loop
         if Buffer (K) /= (Sample (K) xor Stream_Element'Mod (J)) then
            Print ("Wrong buffer");
         end if;
      end loop;
   end loop;

   if not Stream.End_Of_Stream then
      Print ("End of stream is not detected " & Back.End_Of_Stream'Img);

      Stream.Read (Buffer, Last);

      if Last >= Buffer'First then
         Print ("Wrong data at tail");
      end if;
   end if;

end ZSome;
