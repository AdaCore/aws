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

--  Test for the embedded resource files

with Ada.Text_IO;
with Ada.Exceptions;
with Ada.Streams.Stream_IO;

with GNAT.OS_Lib;

with AWS.Resources;
with AWS.Translator;

procedure Tres2 is

   use Ada;
   use Ada.Text_IO;
   use Ada.Streams;
   use AWS;

   procedure Create_File;
   --  Create file for the test

   F : Resources.File_Type;
   Filename : constant String := "file.tmp";

   Buffer : String (1 .. 1_024);
   Last   : Natural;

   Elements : Stream_Element_Array (1 .. 4);
   E_Last   : Stream_Element_Offset;

   procedure Create_File is
      File : Stream_IO.File_Type;

      L1   : constant Stream_Element_Array :=
        Translator.To_Stream_Element_Array ("123456789" & ASCII.LF);
      L2   : constant Stream_Element_Array :=
        Translator.To_Stream_Element_Array ("ABC" & ASCII.LF);
      L3   : constant Stream_Element_Array :=
        Translator.To_Stream_Element_Array ("abc" & ASCII.LF);
   begin
      Stream_IO.Create (File, Stream_IO.Out_File, Filename);

      Stream_IO.Write (File, L1);
      Stream_IO.Write (File, L2);

      for K in 1 .. 3_000 loop
         Stream_IO.Write (File, L1);
         Stream_IO.Write (File, L3);
      end loop;

      Stream_IO.Close (File);
   end Create_File;

   Success : Boolean;

begin
   Create_File;

   Resources.Open (F, Filename);

   Resources.Get_Line (F, Buffer, Last);
   Text_IO.Put_Line ("1) " & Buffer (1 .. Last));

   Resources.Read (F, Elements, E_Last);
   Text_IO.Put_Line ("2) " & Translator.To_String (Elements (1 .. E_Last)));

   Resources.Get_Line (F, Buffer, Last);
   Text_IO.Put_Line ("3) " & Buffer (1 .. Last));

   Resources.Read (F, Elements, E_Last);
   Text_IO.Put_Line ("4) " & Translator.To_String (Elements (1 .. E_Last)));

   for K in 1 .. 2_999 loop
      Resources.Get_Line (F, Buffer, Last);
      if Buffer (1 .. Last) /= "123456789" then
         Text_IO.Put_Line ("Error " & Buffer (1 .. Last)
                             & "; K=" & Positive'Image (K));
      end if;

      Resources.Read (F, Elements, E_Last);

      if Translator.To_String (Elements (1 .. 3)) /= "abc" then
         Text_IO.Put_Line ("Error " & Translator.To_String (Elements (1 .. 3))
                             & "; K=" & Positive'Image (K));
      end if;
   end loop;

   Resources.Close (F);

   GNAT.OS_Lib.Delete_File (Filename, Success);

exception
   when E : others =>
      Put_Line ("Main Error " & Exceptions.Exception_Information (E));
end Tres2;
