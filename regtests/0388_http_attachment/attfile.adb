------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                       Copyright (C) 2026, AdaCore                        --
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

with Ada.Directories;
with Ada.Text_IO;

with AWS.Attachments;
with AWS.Headers;

procedure AttFile is
   use Ada;
   use AWS;

   L : Attachments.List;
   F : Text_IO.File_Type;
begin
   --  Create a file in a subdirectory and add it to the attachment list
   Directories.Create_Directory ("somedir");
   Directories.Create_Directory ("somedir/deep");

   Text_IO.Create (F, Text_IO.Out_File, "somedir/deep/file.txt");
   Text_IO.Put_Line (F, "This is a test file");
   Text_IO.Close (F);

   Attachments.Add (L, "somedir/deep/file.txt", Headers.Empty_List);

   Text_IO.Put_Line ("Filename : " & Attachments.Filename (L.Get (1)));
end AttFile;
