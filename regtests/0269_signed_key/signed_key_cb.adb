------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                       Copyright (C) 2015, AdaCore                        --
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

with AWS.MIME;
with AWS.Response.Set;

package body Signed_Key_CB is

   use Ada;

   Mess : Boolean := False;

   -----------
   -- HW_CB --
   -----------

   function HW_CB (Request : Status.Data) return Response.Data is
      Answer : Response.Data;
   begin
      return Response.Build (MIME.Text_HTML, "Hello World!");
   end HW_CB;

   ------------------
   -- Set_Password --
   ------------------

   function Set_Password (File : String) return String is
   begin
      if not Mess then
         Text_IO.Put_Line ("Asking for password for " & File);
      end if;

      Mess := True;

      return "foobar";
   end Set_Password;

end Signed_Key_CB;
