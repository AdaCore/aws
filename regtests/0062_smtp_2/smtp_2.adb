------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                    Copyright (C) 2008-2010, AdaCore                      --
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

with Ada.Exceptions;
with Ada.Text_IO;

with AWS.Attachments;
with AWS.Headers.Set;
with AWS.Messages;
with AWS.MIME;
with AWS.SMTP.Client;
with AWS.SMTP.Server;

with SMTP_Pck;

with Get_Free_Port;

procedure SMTP_2 is

   use Ada;
   use Ada.Exceptions;
   use AWS;

   From_Name  : constant String := "My Name";
   From_Email : constant String := "my.name@righthere.fr";
   Filename   : constant String := "ada.gif";

   Port   : Positive := 9025;
   Host   : SMTP.Receiver;
   Server : SMTP.Server.Handle;
   Status : SMTP.Status;
   Attac  : Attachments.List;
   Alter  : Attachments.Alternatives;

begin
   Get_Free_Port (Port);

   Host := SMTP.Initialize ("localhost", Port);

   SMTP.Server.Start (Server, Host, SMTP_Pck.Dump_Mail'Access);

   --  Send simple message

   Text_IO.Put_Line ("xxxxxxxxxxxxxxxxxxxxxxxxxxxxxx 1");

   SMTP.Client.Send
     (Host,
      From    => SMTP.E_Mail ("Pascal Obry", "obry@nowhere.org"),
      To      => SMTP.E_Mail ("John Doe", "john.doe@nothere.net"),
      Subject => "First message",
      Message => "First message body",
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

   --  Send alternative messages plus attachment

   Text_IO.Put_Line ("xxxxxxxxxxxxxxxxxxxxxxxxxxxxxx 3");

   Attachments.Add
     (Alter,
      Attachments.Value
        ("this is the default plain text",
         Content_Type => MIME.Text_Plain));
   Attachments.Add
     (Alter,
      AWS.Attachments.Value
        ("<p>this is the default <i>HTML</i> text",
         Content_Type => MIME.Text_HTML));

   Attachments.Add (Attac, Alter);

   SMTP.Client.Send
     (Host,
      From        => SMTP.E_Mail (From_Name, From_Email),
      To          => (1 => SMTP.E_Mail ("Pascal Obry", "aws@obry.net")),
      Subject     => "Sending alternative parts e-mail from Ada code",
      Attachments => Attac,
      Status      => Status);

   SMTP_Pck.Callback.Wait; Text_IO.Flush;

   --  Send alternative messages plus attachment

   Text_IO.Put_Line ("xxxxxxxxxxxxxxxxxxxxxxxxxxxxxx 4");

   Attachments.Add
     (Attac,
      Filename   => Filename,
      Content_Id => "ada.gif",
      Encode     => Attachments.Base64);

   SMTP.Client.Send
     (Host,
      From        => SMTP.E_Mail (From_Name, From_Email),
      To          => (1 => SMTP.E_Mail ("Pascal Obry", "aws@obry.net")),
      Subject     =>
        "Sending alternative parts e-mail plus attachment from Ada code",
      Attachments => Attac,
      Status      => Status);

   SMTP_Pck.Callback.Wait; Text_IO.Flush;

   --  Check multiple Content_Type, ensure the one in Data is used

   declare
      CT_Attac   : Attachments.List;
      CT_Headers : Headers.List;
   begin
      Headers.Set.Add
         (CT_Headers,
          Messages.Content_Type_Token, MIME.Text_Plain);

      Attachments.Add
        (CT_Attac,
         Name => "utf8",
         Data => Attachments.Value
           ("this is the default plain text UTF-8",
            Encode       => Attachments.Base64,
            Content_Type => MIME.Text_Plain & "; charset=UTF-8"),
         Headers => CT_Headers);

      SMTP.Client.Send
        (Host,
         From        => SMTP.E_Mail (From_Name, From_Email),
         To          => (1 => SMTP.E_Mail ("Pascal Obry", "aws@obry.net")),
         Subject     => "Sending an UTF-8 attachement from Ada code",
         Attachments => CT_Attac,
         Status      => Status);

      SMTP_Pck.Callback.Wait; Text_IO.Flush;
   end;

   SMTP.Server.Shutdown (Server);

exception
   when E : others =>
      Text_IO.Put_Line ("SMTP_2 exception:" & Exception_Information (E));
end SMTP_2;
