------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                       Copyright (C) 2009, AdaCore                        --
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

with Ada.Text_IO;            use Ada.Text_IO;

with AWS.Resources.Embedded;
with AWS.Utils;
with AWS.Templates;

with Res;

procedure Bug is
   use AWS;

   Name : constant String := "text.txt";
   Set  : Templates.Translate_Set;
begin
   Put_Line ("On disk : " & Aws.Utils.Is_Regular_File (Name)'Img);
   Put_Line ("Embedded: " & Aws.Resources.Embedded.Exist (Name)'Img);

   Templates.Insert (Set, Templates.Assoc ("NAME", "text.txt"));
   Put_Line (Templates.Parse (Name, Set));
end Bug;
