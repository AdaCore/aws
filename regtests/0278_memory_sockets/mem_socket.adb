------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                       Copyright (C) 2015, AdaCore                        --
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

--  Test memory socket

with Ada.Streams;
with Ada.Text_IO;

with AWS.Net.Memory;
with AWS.Translator;

procedure Mem_Socket is
   use Ada;
   use Ada.Streams;

   use AWS;

   S1, S2, S3 : Net.Memory.Socket_Type;
   Buffer     : Stream_Element_Array (1 .. 100);
   Last       : Stream_Element_Offset;
begin
   --  Test an anonymous socket

   S1.Send (Translator.To_Stream_Element_Array ("some data"), Last);
   Text_IO.Put_Line
     ("S1: Pending " & Stream_Element_Offset'Image (S1.Pending));
   S1.Receive (Buffer, Last);
   Text_IO.Put_Line
     ("    " & Translator.To_String (Buffer (Buffer'First .. Last)));
   Text_IO.Put_Line
     ("S1: Pending " & Stream_Element_Offset'Image (S1.Pending));

   --  Create a "named" memory socket

   Net.Memory.Connect (S2, Host => ":memory:", Port => 76);
   S2.Send (Translator.To_Stream_Element_Array ("some bigger data"), Last);
   Text_IO.Put_Line
     ("S2: Pending " & Stream_Element_Offset'Image (S2.Pending));

   --  Read from another socket

   Net.Memory.Connect (S3, Host => ":memory:", Port => 76);
   Text_IO.Put_Line
     ("S3: Pending " & Stream_Element_Offset'Image (S3.Pending));
   S3.Receive (Buffer, Last);
   Text_IO.Put_Line
     ("    " & Translator.To_String (Buffer (Buffer'First .. Last)));
   Text_IO.Put_Line
     ("S3: Pending " & Stream_Element_Offset'Image (S3.Pending));
end Mem_Socket;
