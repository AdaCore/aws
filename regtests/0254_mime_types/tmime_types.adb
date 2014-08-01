------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                       Copyright (C) 2014, AdaCore                        --
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

with AWS.Config.Set;
with AWS.MIME;

procedure Tmime_Types is

   use Ada;
   use AWS;

   procedure Test (Filename : String) is
   begin
      Text_IO.Put (Filename);
      Text_IO.Set_Col (25);
      Text_IO.Put_Line (" => " & MIME.Content_Type (Filename));
   end Test;

begin
   AWS.Config.Set.MIME_Types ("mime2.types");

   Test ("aws.ada");
   Test ("aws.ecma");
   Test ("aws.toto");
end Tmime_Types;
