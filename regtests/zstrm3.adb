------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                            Copyright (C) 2003                            --
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
--  covered by the GNU Public License.                                      --
------------------------------------------------------------------------------

--  $Id$

with Ada.Text_IO;
with Ada.Exceptions;
with Ada.Streams;

with AWS.Resources.Streams.ZLib;
with AWS.Resources.Streams.Memory.ZLib;
with AWS.Translator;

procedure ZStrm3 is

   use Ada.Streams;
   use Ada.Text_IO;
   use AWS;

   procedure Display
     (Resource   : in out Resources.File_Type;
      Compressed : in     Boolean)
   is
      Buffer : Stream_Element_Array (1 .. 1_024);
      Last   : Stream_Element_Offset;
   begin
      Put_Line ("Compressed : " & Boolean'Image (Compressed));
      Resources.Support_Compressed (Resource, Compressed);
      Resources.Reset (Resource);
      Resources.Read (Resource, Buffer, Last);
      Put_Line ("> " & Translator.To_String (Buffer (1 .. Last)));
   end Display;

   Z_Stream : aliased AWS.Resources.Streams.Memory.ZLib.Stream_Type;
   Resource : AWS.Resources.File_Type;

begin
   Resources.Streams.Memory.Zlib.Deflate_Initialize (Z_Stream);

   Resources.Streams.Memory.Zlib.Append
     (Z_Stream, Translator.To_Stream_Element_Array ("une première ligne"));
   Resources.Streams.Memory.Zlib.Append
     (Z_Stream, Translator.To_Stream_Element_Array ("and a second one"));

   Resources.Streams.Create (Resource, Z_Stream'Unchecked_Access);

   Put_Line ("Is_Compressed : "
             & Boolean'Image (Resources.Is_Compressed (Resource)));

   Display (Resource, Compressed => True);
   Display (Resource, Compressed => False);
end ZStrm3;
