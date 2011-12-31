------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2003-2012, AdaCore                     --
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

procedure ZBig is

   use AWS.Resources.Streams.Memory.ZLib;
   use Ada.Streams;

   Stream : Stream_Type;

   Buffer : AWS.Utils.Stream_Element_Array_Access :=
              new Ada.Streams.Stream_Element_Array (1 .. 20_000_000);

begin
   for J in Buffer'Range loop
      Buffer (J) := Stream_Element (J rem 256);
   end loop;

   Deflate_Initialize (Stream, Level => 1);
   Append (Stream, Buffer.all);
   Ada.Text_IO.Put_Line (Stream_Element_Offset'Image (Size (Stream)));

   Close (Stream);

   Deflate_Initialize (Stream);
   Append (Stream, Buffer.all);

   AWS.Utils.Unchecked_Free (Buffer);

   Ada.Text_IO.Put_Line (Stream_Element_Offset'Image (Size (Stream)));

   Close (Stream);
end ZBig;
