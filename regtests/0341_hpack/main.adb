------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                      Copyright (C) 2021, AdaCore                         --
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
with AWS.Headers;
with AWS.HTTP2.Connection;
with AWS.HTTP2.HPACK.Table;

procedure Main is

   use Ada.Streams;
   use Ada.Text_IO;
   use AWS.HTTP2.HPACK;

   Settings         : aliased AWS.HTTP2.Connection.Object;
   Tab_Enc, Tab_Dec : aliased Table.Object;

   H : AWS.Headers.List;

   ----------
   -- Test --
   ----------

   procedure Test is
      Binary : constant Stream_Element_Array :=
                 Encode (Tab_Enc'Access, Settings'Access, H);
      Index  : Stream_Element_Offset := Binary'First;

      -------------------
      -- End_Of_Stream --
      -------------------

      function End_Of_Stream return Boolean is (Index > Binary'Last);

      ---------------
      -- Next_Byte --
      ---------------

      function Next_Byte return Stream_Element is
      begin
         return Result : constant Stream_Element := Binary (Index) do
            Index := Index + 1;
         end return;
      end Next_Byte;

      ------------
      -- Decode --
      ------------

      function Decode is new AWS.HTTP2.HPACK.Decode (End_Of_Stream, Next_Byte);

      -----------
      -- Print --
      -----------

      procedure Print (It : AWS.Headers.List) is
      begin
         Put_Line ("#" & It.Count'Img & It.Length'Img);
         for J in 1 .. It.Count loop
            Put_Line (It.Get_Line (J));
         end loop;
      end Print;

      M : constant AWS.Headers.List :=
            Decode (Tab_Dec'Access, Settings'Access);

      use type AWS.Headers.List;
      use type AWS.HTTP2.HPACK.Table.Object;

   begin
      if M /= H then
         Ada.Text_IO.Put_Line ("============= Headers differ");
         Print (H);
         Print (M);
         New_Line;
      end if;

      if Tab_Enc /= Tab_Dec then
         Put_Line ("--  Enc");
         Tab_Enc.Dump;
         Put_Line ("--  Dec");
         Tab_Dec.Dump;
      end if;
   end Test;

   Size : Positive;

begin
   H.Case_Sensitive (False);

   H.Add (":method", "GET");
   H.Add (":path", "/readme.txt");
   H.Add (":scheme", "https");
   H.Add (":authority", "localhost:4433");
   H.Add ("user-agent", "curl/7.68.0");
   H.Add ("accept", "*/*");
   H.Add ("x-custom", "custom-value");
   H.Add ("x-another-one", "another-value");
   H.Add ("x-one-more", "more-value");
   H.Add ("x-last-one", "last-value");

   Test;
   Test;

   for J in -1000 .. -1 loop
      Size := Tab_Dec.Size;

      H.Add ("x-header" & Integer'Image (J - 1), 'v' & J'Img);
      H.Add ("x-header" & Integer'Image (J - 1), 'v' & Integer'Image (J - 1));
      H.Add ("x-header" & J'Img, 'v' & J'Img);

      Test;

      --  Exit after test of table truncation to the same size

      exit when Size = Tab_Dec.Size;
   end loop;
end Main;
