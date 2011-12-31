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

with Ada.Text_IO;
with AWS.Resources;
with AWS.Utils;

with zresres;

procedure ERes is

   procedure Test (Name : String) is
      use AWS.Resources;
   begin
      Ada.Text_IO.Put_Line (File_Instance'Image (Exist (Name)));
   end Test;

begin
   Test ("this-file-does-not-exist");
   Test ("filea.txt");
   Test ("fileb.txt.gz");
   Test ("fileb.txt");
   Test ("adains.png");
   Test ("file1.txt");
   Test ("file2.txt.gz");
   Test ("file2.txt");
end ERes;
