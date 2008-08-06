------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2007-2008, AdaCore                     --
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

--  See whether the Base64 images of authentication come out as
--  expected.

with Ada.Text_IO;

with AWS.SMTP.Authentication.Plain;

procedure Test_SMTP_Authentication is
   use Ada;
   use AWS;

   package SMTPA renames AWS.SMTP.Authentication.Plain;

   a  : constant String := "foo";
   p  : constant String := "asdf2345";

   an : constant String := "";
   pn : constant String := "";

   Secret : SMTPA.Credential;

   procedure Test (Authcid : String; Passwd : String; Expected : String);
   --  prints OK or FAIL depending on whether the encoded img of
   --  Authcid and Passwd = Expected

   ----------
   -- Test --
   ----------

   procedure Test (Authcid : String; Passwd : String; Expected : String) is
   begin
      Text_IO.Put_Line ("?? '" & Authcid & "', '" & Passwd & "':");

      Secret := SMTPA.Initialize (Authcid, Passwd);

      declare
         img : constant String := SMTPA.Image (Secret);
      begin
         if img = Expected then
            Text_IO.Put_Line (">> OK " & img & " = " & Expected);
         else
            Text_IO.Put_Line (">> FAIL " & img & " /= " & Expected);
         end if;
      end;
      Text_IO.New_Line;
   end Test;

begin
   Test (a, p, "AGZvbwBhc2RmMjM0NQ==");
   Test (a, pn, "AGZvbwA=");
   Test (an, p, "AABhc2RmMjM0NQ==");
   Test (an, pn, "AAA=");
end Test_SMTP_Authentication;
