------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2000-2001                          --
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
--  covered by the  GNU Public License.                                     --
------------------------------------------------------------------------------

--  $Id$

--  Test for the embedded compressed resource feature

with Ada.Exceptions;
with Ada.Streams;
with Ada.Text_IO;

with AWS.Resources;
with AWS.Translator;

with Zresres;

procedure Zres is

   use Ada;
   use Ada.Streams;
   use AWS;

   File   : Resources.File_Type;
   Buffer : Stream_Element_Array (1 .. 1_024);
   Last   : Stream_Element_Offset;

begin
   Text_IO.Put_Line ("Start zres test");

   --  Read non compressed resource

   Resources.Open (File, "file1.txt");

   Resources.Read (File, Buffer, Last);
   Text_IO.Put_Line ("> " & Translator.To_String (Buffer (1 .. Last)));

   Resources.Close (File);

   --  Read compressed resource

   Resources.Open (File, "file2.txt");
   Resources.Support_Compressed (File, False);

   Resources.Read (File, Buffer, Last);
   Text_IO.Put_Line ("> " & Translator.To_String (Buffer (1 .. Last)));

   Resources.Close (File);

exception
   when E : others =>
      Text_IO.Put_Line ("Main Error " & Exceptions.Exception_Information (E));
end Zres;
