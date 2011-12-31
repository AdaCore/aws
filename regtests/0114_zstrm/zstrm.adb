------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2000-2012, AdaCore                     --
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

--  Test for user defined stream

with Ada.Text_IO;
with Ada.Exceptions;
with Ada.Streams;

with AWS.Resources.Streams.ZLib;

with Z_User_Strm;

procedure ZStrm is

   use Ada.Streams;
   use Ada.Text_IO;
   use AWS;

   File_Size : constant := 12_345_678;

   Stream : AWS.Resources.Streams.Stream_Access := new Z_User_Strm.File_Tagged;
   Encode : AWS.Resources.Streams.Stream_Access;
   Decode : AWS.Resources.Streams.Stream_Access;

   Sample : AWS.Resources.Streams.Stream_Access := new Z_User_Strm.File_Tagged;

   Buffer : Stream_Element_Array (1 .. 1000);
   Last   : Stream_Element_Offset;

   S_Buffer : Stream_Element_Array (1 .. 1000);
   S_Last   : Stream_Element_Offset;

begin
   Z_User_Strm.Create
     (Resource       => Stream.all,
      Undefined_Size => True,
      Size           => File_Size);

   Encode := new Resources.Streams.ZLib.Stream_Type;

   Resources.Streams.ZLib.Deflate_Initialize
     (Resources.Streams.ZLib.Stream_Type (Encode.all), Stream);

   Decode := new Resources.Streams.ZLib.Stream_Type;

   Resources.Streams.ZLib.Inflate_Initialize
     (Resources.Streams.ZLib.Stream_Type (Decode.all), Encode);

   Z_User_Strm.Create
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
