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

with AWS.Translator;
with AWS.Utils;

procedure TCompress is

   use Ada;
   use Ada.Streams;
   use AWS;

   use type Streams.Stream_Element_Array;

   Comp_Level : Translator.Compression_Level := 0;

   ----------
   -- Test --
   ----------

   procedure Test (Str : String) is
      Data   : constant Streams.Stream_Element_Array :=
                 Translator.To_Stream_Element_Array (Str);
      Comp   : Utils.Stream_Element_Array_Access;
      Decomp : Utils.Stream_Element_Array_Access;
   begin
      Comp   := Translator.Compress (Data, Comp_Level);
      Decomp := Translator.Decompress (Comp.all);

      if Data = Decomp.all then
         Text_IO.Put_Line ("Ok");
      else
         Text_IO.Put_Line ("Nok: " & Translator.To_String (Decomp.all));
      end if;

      Text_IO.Put_Line
        (Integer'Image (Data'Length) & " bytes compressed to"
           & Integer'Image (Comp'Length));

      Utils.Unchecked_Free (Comp);
      Utils.Unchecked_Free (Decomp);
   end Test;

   ---------
   -- Run --
   ---------

   procedure Run is
   begin
      Test ("");

      Test ("simple");

      Test ("AWS can compress and decompress data");

      Test ("A longer text to test the real factor compression which is "
              & "almost null on very small chunk of data. So this test is "
              & "one on which we will display real size.");

      Test ("A longer text to test the real factor compression which is "
              & "almost null on very small chunk of data. So this test is "
              & "the only one which will display real size. And again "
              & "A longer text to test the real factor compression which is "
              & "almost null on very small chunk of data. So this test is "
              & "the only one which will display real size. And again "
              & "A longer text to test the real factor compression which is "
              & "almost null on very small chunk of data. So this test is "
              & "the only one which will display real size. And again "
              & "A longer text to test the real factor compression which is "
              & "almost null on very small chunk of data. So this test is "
              & "the only one which will display real size. And again "
              & "A longer text to test the real factor compression which is "
              & "almost null on very small chunk of data. So this test is "
              & "the only one which will display real size. And again "
              & "A longer text to test the real factor compression which is "
              & "almost null on very small chunk of data. So this test is "
              & "the only one which will display real size. That's all :)");
   end Run;

begin
   Text_IO.Put_Line
     ("=== Compression" & Translator.Compression_Level'Image (Comp_Level));

   Run;

   Comp_Level := 2;

   Text_IO.New_Line;
   Text_IO.Put_Line
     ("=== Compression" & Translator.Compression_Level'Image (Comp_Level));

   Run;

   Comp_Level := 9;

   Text_IO.New_Line;
   Text_IO.Put_Line
     ("=== Compression" & Translator.Compression_Level'Image (Comp_Level));

   Run;

   declare
      V : Stream_Element_Array := (89, 87, 55);
   begin
      Text_IO.Put_Line (Translator.To_String (V));
   end;
end TCompress;
