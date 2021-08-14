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

with Ada.Direct_IO;
with Ada.Exceptions;
with Ada.Streams;           use Ada.Streams;
with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;
with ZLib.Streams;

procedure Test_Streams_End is

   subtype Sword is
     Integer range -2 ** 15 .. 2 ** 15 - 1 with Object_Size => 16;

   Header : ZLib.Header_Type := ZLib.Default;

   procedure Createfile (Filename : String) is
      F : Ada.Streams.Stream_IO.File_Type;
      Z : aliased ZLib.Streams.Stream_Type;
      S : Stream_Access;
   begin
      Create (F, Out_File, Filename);
      ZLib.Streams.Create
         (Z, ZLib.Streams.Out_Stream, ZLib.Streams.Stream_Access (Stream (F)),
          Back_Compressed => True, Header => Header);
      S := Z'Unchecked_Access;
      for I in 1 .. 4 loop
         Sword'Write (S, I);
      end loop;
      ZLib.Streams.Close (Z);
      Close (F);
   end Createfile;

   procedure Readfile (Filename : String) is
      F : Ada.Streams.Stream_IO.File_Type;
      Z : aliased ZLib.Streams.Stream_Type;
      S : Stream_Access;
      W : Sword;
      Rest : Stream_Element_Array (1 .. 1);
      Last : Stream_Element_Offset;
   begin
      Open (F, In_File, Filename);
      ZLib.Streams.Create
         (Z, ZLib.Streams.In_Stream, ZLib.Streams.Stream_Access (Stream (F)),
          Back_Compressed => True, Header => Header);
      S := Z'Unchecked_Access;
      for I in 1 .. 4 loop
         Sword'Read (S, W);
         Put_Line (W'Image & (if Z.End_Of_Stream then " last" else ""));
      end loop;

      begin
         for J in 1 .. 2 loop
            Z.Read (Rest, Last);
            Put (Last'Img);
         end loop;
      exception
         when E : ZLib.ZLib_Error =>
            Put_Line ("End of stream: " & Ada.Exceptions.Exception_Message (E));
      end;
      New_Line;

      ZLib.Streams.Close (Z);
      Close (F);

   exception
      when E : ZLib.ZLib_Error =>
         ZLib.Streams.Close (Z);
         Close (F);
         raise;
   end Readfile;

   procedure Shorten_One (Filename : String) is
      package Chio is new Ada.Direct_IO (Character);
      use Chio;
      Data : Unbounded_String;
      F : Chio.File_Type;
      Ch : Character;
   begin
      Open (F, In_File, Filename);
      while not End_Of_File (F) loop
         Read (F, Ch);
         Append (Data, Ch);
      end loop;
      Close (F);

      Data := Unbounded_Slice (Data, 1, Length (Data) - 1);

      Create (F, Out_File, Filename);
      for I in 1 .. Length (Data) loop
         Write (F, Element (Data, I));
      end loop;
      Close (F);

      Put_Line ("Created file with length " & Integer'Image (Length (Data)));
   end Shorten_One;

   procedure Test is
   begin
      Createfile ("Streamtest.xxx");
      loop
         Readfile ("Streamtest.xxx");
         Shorten_One ("Streamtest.xxx");
      end loop;
   exception
      when E : ZLib.ZLib_Error =>
         Put_Line ("OK: " & Ada.Exceptions.Exception_Message (E));
   end Test;

begin
   Test;
   Header := ZLib.GZip;
   Test;
end Test_Streams_End;
