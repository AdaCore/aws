------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2008-2015, AdaCore                     --
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

with AWS.Parameters;

procedure Param2 is

   use Ada;
   use AWS;

   List : Parameters.List;
   Mist : Parameters.List;

   procedure Print_Union (Left, Right : Parameters.List; Unique : Boolean) is
   begin
      Text_IO.Put_Line
        ("> " & Parameters.URI_Format (Left.Union (Right, Unique)));
   end Print_Union;

begin
   List.Add ("name1", "value_xxxx");
   List.Add ("name2", "value_2");
   List.Update ("name1", "value_1");

   Text_IO.Put_Line ("1> " & Parameters.URI_Format (List));

   List.Add ("name3", "value");
   List.Update ("name3", "value_3");

   Text_IO.Put_Line ("2> " & Parameters.URI_Format (List));

   Mist.Add ("name1", "mist-1");
   Mist.Add ("name3", "mist-2");
   Mist.Add ("char+", "value%23", Decode => True);
   Print_Union (List, Mist, False);
   Print_Union (List, Mist, True);
   Print_Union (Mist, List, False);
   Print_Union (Mist, List, True);

exception
   when E : others =>
      Text_IO.Put_Line ("Main Error " & Exceptions.Exception_Information (E));
end Param2;
