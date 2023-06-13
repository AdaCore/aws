------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                       Copyright (C) 2023, AdaCore                        --
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

with AWS.Attachments;
with AWS.Headers;
with AWS.Messages;
with AWS.MIME;
with AWS.Net.SSL;
with AWS.SMTP.Client;
with AWS.SMTP.Server;

with SMTP_Pck;

procedure SMTP_STARTTLS is

   use Ada;
   use Ada.Exceptions;
   use AWS;

   From_Name  : constant String := "My Name";
   From_Email : constant String := "my.name@righthere.fr";
   Filename   : constant String := "ada.gif";

   Family : Net.Family_Type;
   Host   : SMTP.Receiver;
   Server : SMTP.Server.Handle;
   Status : SMTP.Status;
   Attac  : Attachments.List;
   Alter  : Attachments.Alternatives;
   EOL    : constant String := ASCII.CR & ASCII.LF;

begin
   if Net.IPv6_Available then
      Family := Net.Family_Inet6;
   else
      Family := Net.Family_Inet;
   end if;

   Host := SMTP.Initialize
     (Net.Localhost (Net.IPv6_Available), 0,
      Security => SMTP.STARTTLS,
      Family   => Family,
      Timeout  => 1.0);

   SMTP.Server.Start (Server, Host, SMTP_Pck.Dump_Mail'Access);

   --  Recreate Host with port defined

   Host := SMTP.Initialize
     (Net.Localhost (Net.IPv6_Available), SMTP.Server.Port (Server),
      Security => SMTP.STARTTLS,
      Family   => Family,
      Timeout  => 1.0);

   --  Send simple message

   Text_IO.Put_Line ("xxxxxxxxxxxxxxxxxxxxxxxxxxxxxx 1");

   SMTP.Client.Send
     (Host,
      From    => SMTP.E_Mail ("Pascal Obry", "obry@nowhere.org"),
      To      => SMTP.E_Mail ("John Doe", "john.doe@nothere.net"),
      Subject => "First message",
      Message => "First message body" & EOL & "." & EOL & ".al" & EOL & "..br",
      Status  => Status);

   SMTP_Pck.Callback.Wait; Text_IO.Flush;

   --  Send simple message, plus attachment

   Text_IO.Put_Line ("xxxxxxxxxxxxxxxxxxxxxxxxxxxxxx 2");

   SMTP.Client.Send
     (Host,
      From        => SMTP.E_Mail (From_Name, From_Email),
      To          => (1 => SMTP.E_Mail ("Pascal Obry", "aws@obry.net")),
      Subject     => "Sending e-mail + attachment from Ada code",
      Message     => "Thanks to AWS/SMTP, it's easy !",
      Attachments => (1 => SMTP.Client.File (Filename)),
      Status      => Status);

   SMTP_Pck.Callback.Wait; Text_IO.Flush;

   SMTP.Server.Shutdown (Server);

exception
   when E : others =>
      Text_IO.Put_Line
        ("SMTP_STARTTLS exception:" & Exception_Information (E));
end SMTP_STARTTLS;
