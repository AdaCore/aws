------------------------------------------------------------------------------
--                   S M T P - Simple Mail Transfer Protocol                --
--                                                                          --
--                            Copyright (C) 2007                            --
--                                 AdaCore                                  --
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
------------------------------------------------------------------------------

with Ada.Text_IO;

with AWS.SMTP.Client;
with AWS.SMTP.Authentication.Plain;

procedure Test_Mail is

   use Ada;
   use AWS;

   Auth : aliased constant SMTP.Authentication.Plain.Credential :=
     SMTP.Authentication.Plain.Initialize ("user_id", "password");
   --  For authentication pass Auth to the server initialization below

   Server : SMTP.Receiver := SMTP.Client.Initialize
     ("host_here");
   --  For authenticating SMTP server pass :
   --  Credential => Auth'Unchecked_Access

   Stat : SMTP.Status;

begin
   SMTP.Client.Send
     (Server,
      From    => SMTP.E_Mail ("Your Name", "Your e-mailx"),
      To      => (1 => SMTP.E_Mail ("Pascal Obry", "aws@obry.net")),
      Subject => "Sending e-mail from Ada code",
      Message => "Thanks to AWS/SMTP, it's easy !",
      Status  => Stat);

   Text_IO.Put_Line ("Stat : " & SMTP.Status_Message (Stat));
end Test_Mail;
