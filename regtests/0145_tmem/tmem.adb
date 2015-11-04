------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2004-2015, AdaCore                     --
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

with Ada.Calendar;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with Memory_Streams;

procedure TMem is
   use Ada.Text_IO;
   use Ada.Strings.Unbounded;
   use Ada.Calendar;

   type String_Access is access all String;
   type Constant_Access is access constant String;

   package Strings is new Memory_Streams
     (Element         => Character,
      Element_Index   => Positive,
      Element_Array   => String,
      Element_Access  => String_Access,
      Constant_Access => Constant_Access,
      Next_Block_Length => 80);

   use Strings;

   Stream : Stream_Type;
   UStr   : Unbounded_String;

   Stamp  : Time;

   Buffer : String (1 .. 20);
   Sample : aliased constant String := "987654321" & ASCII.Lf;

   Ptr    : String_Access;
   CPtr   : Constant_Access := Sample'Access;

   Last   : Natural;

   procedure Compare_Result (Offset : Positive) is
      Str   : String (1 .. 200);
      Last  : Natural;
      Index : Positive := Offset;
   begin
      if Length (UStr) /= Size (Stream) then
         Put_Line ("Length error");
         raise Constraint_Error;
      end if;

      loop
         Str := (others => '-');

         Read (Stream, Str, Last);

         if Str (1 .. Last) /= Slice (UStr, Index, Index + Last - 1) then
            Put_Line ("Content error " & Integer'Image (Last));
            raise Constraint_Error;
         end if;

         Index := Index + Last;

         exit when Last < Str'Last;
      end loop;

      if Index /= Size (Stream) + 1 then
         Put_Line ("Read length error.");
         raise Constraint_Error;
      end if;

      if not End_Of_File (Stream) then
         Put_Line ("End of file error.");
         raise Constraint_Error;
      end if;
   end Compare_Result;

begin
   Ptr := new String'((1010 .. 1070 => '=', 1071 => ASCII.LF));
   Append (Stream, Ptr);

   for J in 1 .. 99 loop
      Append
        (Stream,
         "123456789" & J'Img & J'Img & J'Img & J'Img & J'Img & J'Img & J'Img
          & J'Img & J'Img & J'Img & J'Img & J'Img & J'Img & J'Img
          & J'Img & J'Img & J'Img & J'Img & J'Img & J'Img & J'Img
          & J'Img & J'Img & J'Img & J'Img & J'Img & J'Img & J'Img
         & ASCII.Lf);
   end loop;

   Append (Stream, "123456789" & ASCII.Lf);

   Append (Stream, Sample);

   Append (Stream, CPtr);

   Append (Stream, "variable after constant OB04-011" & ASCII.Lf);

   Ptr := new String'((1010 .. 1070 => '=', 1071 => ASCII.LF));
   Append (Stream, Ptr);

   Put_Line (Integer'Image (Size (Stream)));

   loop
      Read (Stream, Buffer, Last);
      Put (Buffer (1 .. Last));
      exit when Last < Buffer'Last;
   end loop;

   if not End_Of_File (Stream) then
      Put_Line ("End of file error.");
      raise Constraint_Error;
   end if;

   Clear (Stream);

   --  Test speed

   Stamp := Clock;

   for J in 1 .. 10000 loop
      Append
        (Stream,
         "absdefghijkl-ABSDEFGHIJKL-1234578"
           & Integer'Image (J) & ASCII.Lf,
         Trim => J = 10000);
   end loop;

   --  Not for regression test.
   --  Put_Line ("Stream method spend     " & Duration'Image (Clock - Stamp));

   Stamp := Clock;

   for J in 1 .. 10000 loop
      Append (UStr, "absdefghijkl-ABSDEFGHIJKL-1234578"
                    & Integer'Image (J) & ASCII.Lf);
   end loop;

   --  Not for regression test.
   --  Put_Line ("Unbounded string method " & Duration'Image (Clock - Stamp));

   --  Compare result

   Put_Line ("Size " & Integer'Image (Size (Stream)));

   Compare_Result (1);

   Set_Index (Stream, 188_894);
   Compare_Result (188_894);

   Set_Index (Stream, 7);
   Compare_Result (7);

   Set_Index (Stream, 200_000);
   Compare_Result (200_000);

   Set_Index (Stream, 388_894);
   Compare_Result (388_894);

   Close (Stream);

   Put_Line ("Done.");
end TMem;
