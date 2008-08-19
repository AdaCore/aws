------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2003-2008, AdaCore                     --
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

with Ada.Exceptions;
with Ada.Text_IO;

with AWS.Utils;

procedure Zfile is

   use Ada.Text_IO;
   use AWS;

   ------------
   -- Create --
   ------------

   procedure Create is
      File : File_Type;
   begin
      Create (File, Out_File, "zfile.txt");
      Put_Line (File, "First line in this file...");
      Put_Line (File, "And this is the second!");
      New_Line (File);
      Put_Line (File, "Well let's end this file now.");
      Close (File);
   end Create;

   ------------
   -- Output --
   ------------

   procedure Output is
      File   : File_Type;
      Buffer : String (1 .. 100);
      Last   : Natural;
      Len    : Natural := 0;
   begin
      Open (File, In_File, "zfile.txt");

      while not End_Of_File (File) loop
         Get_Line (File, Buffer, Last);
         Put_Line (Buffer (1 .. Last));
         Len := Len + Last;
      end loop;

      Delete (File);

      New_Line;
      Put_Line ("Bytes" & Natural'Image (Len));
   end Output;

begin
   Create;
   Utils.Compress ("zfile.txt", 9);
   Utils.Decompress ("zfile.txt.gz");
   Output;
exception
   when E : others =>
      Put_Line ("ZFile Error !");
      Put_Line (Ada.Exceptions.Exception_Information (E));
end Zfile;
