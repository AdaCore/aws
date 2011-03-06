------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2003-2011, AdaCore                     --
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

   AWS.Utils.Free (Buffer);

   Ada.Text_IO.Put_Line (Stream_Element_Offset'Image (Size (Stream)));

   Close (Stream);
end ZBig;
