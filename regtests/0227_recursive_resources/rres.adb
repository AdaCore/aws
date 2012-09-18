------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                       Copyright (C) 2012, AdaCore                        --
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

with AWS.Resources.Embedded;
with AWS.Utils;

with rdemo;

procedure RRes is

   use Ada;
   use AWS;

   procedure Test (Name : String) is
      use AWS.Resources;

      File   : Resources.File_Type;
      Buffer : String (1 .. 100);
      Last   : Natural;
      Gzip   : Boolean := False;
   begin
      if Resources.Exist (Name)  = None then
         Text_IO.Put_Line ("Resource " & Name & " not found.");

      else
         Text_IO.Put_Line ("Name " & Name);

         Resources.Embedded.Open (File, Name, Gzip => Gzip);

         while not Resources.End_Of_File (File) loop
            Resources.Get_Line (File, Buffer, Last);
            Text_IO.Put_Line (": " & Buffer (1 .. Last));
         end loop;

         Resources.Close (File);
      end if;
   end Test;

begin
   Test ("root");
   Test ("root.txt");
   Test ("dir1/text1.txt");
   Test ("dir1/sdir/text3.txt");
   Test ("dir1/sdir/text4.txt");
   Test ("dir2/text5.txt");
end RRes;
