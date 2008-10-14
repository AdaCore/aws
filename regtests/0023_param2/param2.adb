------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                       Copyright (C) 2008, AdaCore                        --
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
