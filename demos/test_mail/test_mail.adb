------------------------------------------------------------------------------
--                   S M T P - Simple Mail Transfer Protocol                --
--                                                                          --
--                       Copyright (C) 2007-2012, AdaCore                   --
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

--  You need to edit this demo to set the proper SMTP server and recipient

with Ada.Text_IO;

with AWS.Attachments;
with AWS.MIME;
with AWS.SMTP.Client;
with AWS.SMTP.Authentication.Plain;

procedure Test_Mail is

   use Ada;
   use AWS;

   --  Edit the following four constants:
   SMTP_Server : constant String := "smtp.here.com";
   From_Name   : constant String := "your name";
   From_Email  : constant String := "your email";
   Filename    : constant String := "c:\tmp\unzip.exe";

   Auth : aliased constant SMTP.Authentication.Plain.Credential :=
     SMTP.Authentication.Plain.Initialize ("user_id", "password");
   --  For authentication pass Auth to the server initialization below

   Server : SMTP.Receiver := SMTP.Client.Initialize (SMTP_Server);
   --  For authenticating SMTP server pass :
   --  Credential => Auth'Unchecked_Access

   Attachments : SMTP.Client.Attachment_Set (1 .. 2);
   Stat        : SMTP.Status;

   Attac : AWS.Attachments.List;
   Alter : AWS.Attachments.Alternatives;

begin
   --  Send simple message

   SMTP.Client.Send
     (Server,
      From    => SMTP.E_Mail (From_Name, From_Email),
      To      => (1 => SMTP.E_Mail ("Pascal Obry", "aws@obry.net")),
      Subject => "Sending e-mail from Ada code",
      Message => "Thanks to AWS/SMTP, it's easy !",
      Status  => Stat);

   Text_IO.Put_Line ("Stat : " & SMTP.Status_Message (Stat));

   --  Send simple message, plus attachment

   SMTP.Client.Send
     (Server,
      From        => SMTP.E_Mail (From_Name, From_Email),
      To          => (1 => SMTP.E_Mail ("Pascal Obry", "aws@obry.net")),
      Subject     => "Sending e-mail + attachment from Ada code",
      Message     => "Thanks to AWS/SMTP, it's easy !",
      Attachments => (1 => SMTP.Client.File (Filename)),
      Status      => Stat);

   Text_IO.Put_Line ("Stat : " & SMTP.Status_Message (Stat));

   --  Send alternative messages plus attachment

   AWS.Attachments.Add
     (Alter,
      AWS.Attachments.Value
        ("this is the default plain text",
         Content_Type => MIME.Text_Plain));
   AWS.Attachments.Add
     (Alter,
      AWS.Attachments.Value
        ("<p>this is the default <i>HTML</i> text",
         Content_Type => MIME.Text_HTML));

   AWS.Attachments.Add (Attac, Alter);

   SMTP.Client.Send
     (Server,
      From        => SMTP.E_Mail (From_Name, From_Email),
      To          => (1 => SMTP.E_Mail ("Pascal Obry", "aws@obry.net")),
      Subject     => "Sending alternative parts e-mail from Ada code",
      Attachments => Attac,
      Status      => Stat);

   Text_IO.Put_Line ("Stat : " & SMTP.Status_Message (Stat));

   --  Send alternative messages plus attachment

   AWS.Attachments.Add
     (Attac,
      Filename   => Filename,
      Content_Id => "unzip.exe",
      Encode     => AWS.Attachments.Base64);

   SMTP.Client.Send
     (Server,
      From        => SMTP.E_Mail (From_Name, From_Email),
      To          => (1 => SMTP.E_Mail ("Pascal Obry", "aws@obry.net")),
      Subject     =>
        "Sending alternative parts e-mail plus attachment from Ada code",
      Attachments => Attac,
      Status      => Stat);

   Text_IO.Put_Line ("Stat : " & SMTP.Status_Message (Stat));
end Test_Mail;
