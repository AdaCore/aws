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
