------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2006-2012, AdaCore                     --
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
with AWS.Session;

procedure Session6 is
   use Ada;
   use AWS;
   SID : Session.Id := Session.Create;
begin
   Session.Set (SID, "key1", "value1");

   if not Session.Exist (SID, "key1") then
      Text_IO.Put_Line ("Error, key1 not found!");
   end if;

   begin
      Session.Remove (SID, "key1");
      Session.Remove (SID, "toto");

      if Session.Exist (SID, "key1") then
         Text_IO.Put_Line ("Error, key1 has not been removed!");
      else
         Text_IO.Put_Line ("OK, session key removed");
      end if;
   exception
      when others =>
         Text_IO.Put_Line ("Error, no exception should be raised");
   end;
end Session6;
