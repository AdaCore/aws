------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2008-2012, AdaCore                     --
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

with AWS.Parameters.Set;

procedure Param2 is

   use Ada;
   use AWS;

   List : Parameters.List;

begin
   Parameters.Set.Add (List, "name1", "value_xxxx");
   Parameters.Set.Add (List, "name2", "value_2");
   Parameters.Set.Update (List, "name1", "value_1");

   Text_IO.Put_Line ("1> " & Parameters.URI_Format (List));

   Parameters.Set.Add (List, "name3", "value");
   Parameters.Set.Update (List, "name3", "value_3");

   Text_IO.Put_Line ("2> " & Parameters.URI_Format (List));
exception
   when E : others =>
      Text_IO.Put_Line ("Main Error " & Exceptions.Exception_Information (E));
end Param2;
