------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2000-2008, AdaCore                     --
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

--  Test for user defined stream

with Ada.Text_IO;
with Ada.Exceptions;
with Ada.Streams;

with AWS.Resources.Streams.ZLib;

with User_Strm;

procedure ZStrm is

   use Ada.Streams;
   use Ada.Text_IO;
   use AWS;

   File_Size : constant := 12_345_678;

   Stream : AWS.Resources.Streams.Stream_Access := new User_Strm.File_Tagged;
   Encode : AWS.Resources.Streams.Stream_Access;
   Decode : AWS.Resources.Streams.Stream_Access;

   Sample : AWS.Resources.Streams.Stream_Access := new User_Strm.File_Tagged;

   Buffer : Stream_Element_Array (1 .. 1000);
   Last   : Stream_Element_Offset;

   S_Buffer : Stream_Element_Array (1 .. 1000);
   S_Last   : Stream_Element_Offset;

begin
   User_Strm.Create
     (Resource       => Stream.all,
      Undefined_Size => True,
      Size           => File_Size);

   Encode := new Resources.Streams.ZLib.Stream_Type;

   Resources.Streams.ZLib.Deflate_Initialize
     (Resources.Streams.ZLib.Stream_Type (Encode.all), Stream);

   Decode := new Resources.Streams.ZLib.Stream_Type;

   Resources.Streams.ZLib.Inflate_Initialize
     (Resources.Streams.ZLib.Stream_Type (Decode.all), Encode);

   User_Strm.Create
     (Resource       => Sample.all,
      Undefined_Size => False,
      Size           => File_Size);

   loop
      Resources.Streams.Read (Decode.all, Buffer, Last);
      Resources.Streams.Read (Sample.all, S_Buffer, S_Last);

      if Buffer (1 .. Last) /= S_Buffer (1 .. S_Last) then
         Ada.Text_IO.Put_Line ("Error.");
      end if;

      exit when Last < Buffer'Last;
   end loop;

   Ada.Text_IO.Put_Line (Stream_Element_Offset'Image (Last));
end ZStrm;
