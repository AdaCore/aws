------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                    Copyright (C) 2004-2012, AdaCore                      --
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

--  Create a password for the hotplug server authorization file

with Ada.Command_Line;
with Ada.Text_IO;
with AWS.Default;

with GNAT.MD5;

procedure AWS_Password is
   use Ada;
   use Ada.Command_Line;
begin
   if Argument_Count = 2 then
      Text_IO.Put_Line
        (GNAT.MD5.Digest
           (Argument (1) & ':' & AWS.Default.Admin_Realm
            & ':' & Argument (2)));

   elsif Argument_Count = 3 then
      Text_IO.Put_Line
        (GNAT.MD5.Digest
           (Argument (1) & ':' & Argument (3) & ':' & Argument (2)));

   else
      Text_IO.Put_Line ("Usage: aws_password <module> <password> [realm]");
   end if;
end AWS_Password;
