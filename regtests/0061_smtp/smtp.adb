------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2004-2012, AdaCore                     --
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
with AWS.SMTP;

procedure SMTP is

   use Ada;
   use AWS;

   procedure Display (E : AWS.SMTP.E_Mail_Data) is
   begin
      Text_IO.Put_Line ("Name    " & AWS.SMTP.Image (E, AWS.SMTP.Name));
      Text_IO.Put_Line ("Address " & AWS.SMTP.Image (E, AWS.SMTP.Address));
      Text_IO.Put_Line ("        " & AWS.SMTP.Image (E, AWS.SMTP.Full));
   end Display;

begin
   Display (AWS.SMTP.Parse ("Pascal Obry <p.obry@domain.com>"));
   Display (AWS.SMTP.Parse ("p.obry@domain.com (Pascal Obry)"));
end SMTP;
