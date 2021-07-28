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

with Ada.Command_Line;
with Ada.Exceptions;
with Ada.Streams;
with Ada.Text_IO;

with AWS.Resources.Streams.ZLib;

with Z_User_Strm;

procedure ZStrm is

   use Ada.Command_Line;
   use Ada.Streams;
   use Ada.Text_IO;
   use AWS;

   File_Size : constant Stream_Element_Count :=
                 (if Argument_Count = 0 then 12_345_678
                  else Stream_Element_Count'Value (Argument (1)));

   Stream : AWS.Resources.Streams.Stream_Access := new Z_User_Strm.File_Tagged;
   Encode : AWS.Resources.Streams.Stream_Access;
   Decode : AWS.Resources.Streams.Stream_Access;

   Sample : AWS.Resources.Streams.Stream_Access := new Z_User_Strm.File_Tagged;

   Buffer : Stream_Element_Array (1 .. 1000);
   Last   : Stream_Element_Offset;

   S_Buffer : Stream_Element_Array (1 .. 1000);
   S_Last   : Stream_Element_Offset;

   Length : Stream_Element_Count := 0;

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

      Length := Length + Last;

      if Buffer (1 .. Last) /= S_Buffer (1 .. S_Last) then
         Ada.Text_IO.Put_Line ("Error.");
      end if;

      if Last < Buffer'Last then
         if not Decode.End_Of_File then
            Ada.Text_IO.Put_Line ("End of file is not detected.");
         end if;

         exit;
      end if;

      exit when Decode.End_Of_File;
   end loop;

   if Length /= File_Size then
      Ada.Text_IO.Put_Line ("Wrong file size.");
   end if;

   Ada.Text_IO.Put_Line (Stream_Element_Offset'Image (Last));
end ZStrm;
