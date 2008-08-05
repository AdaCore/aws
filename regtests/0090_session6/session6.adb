------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2006-2008, AdaCore                     --
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
