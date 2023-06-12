------------------------------------------------------------------------------
--                   S M T P - Simple Mail Transfer Protocol                --
--                                                                          --
--                         Copyright (C) 2023, AdaCore                      --
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

--  This demo send a message using plain TLS or a STARTTLS switching protocol.
--  You need to edit this demo to set the proper SMTP server and recipient.

with Ada.Directories;
with Ada.Text_IO;

with AWS.SMTP.Authentication.Plain;
with AWS.SMTP.Client;

procedure Test_SMail is

   --  Edit the following seven constants:

   SMTP_Server   : constant String := "smtp.gmail.com";
   SMTP_Login    : constant String := "";
   SMTP_Password : constant String := "";
   From_Name     : constant String := "";
   From_Email    : constant String := "";
   To_Name       : constant String := "";
   To_Email      : constant String := "";

   procedure Mail (Subject : String; STARTTLS : Boolean) is
      use Ada;
      use AWS;

      SMTP_Server_Name : constant String := "smtp.gmail.com";
      Status           : SMTP.Status;
   begin
      declare
         Auth      : aliased constant SMTP.Authentication.Plain.Credential :=
                       SMTP.Authentication.Plain.Initialize
                         (SMTP_Login, SMTP_Password);

         Msg       : constant String := "A message, STARTTLS is "
                                        & STARTTLS'Img;

         Receivers : constant SMTP.Recipients :=
                       (1 =>  SMTP.E_Mail(To_Name, To_Email));
      begin
         declare
            SMTP_Server : SMTP.Receiver := SMTP.Client.Initialize
              (SMTP_Server_Name,
               Port       => (if STARTTLS then 587 else 465),
               Security   => (if STARTTLS then SMTP.STARTTLS else SMTP.TLS),
               Credential => Auth'Unchecked_Access);
         begin
            SMTP.Client.Send
              (Server  => SMTP_Server,
               From    => SMTP.E_Mail (From_Name, From_Email),
               To      => Receivers,
               Subject => Subject,
               Message => Msg,
               Status  => Status);
          end;
       end;

       if not SMTP.Is_Ok (Status) then
         Text_IO.Put_Line
           ("Can't send message: " & SMTP.Status_Message (Status));
       end if;
   end Mail;

begin
   Mail ("Test from AWS", STARTTLS => True);
   Mail ("Test from AWS", STARTTLS => False);
end Test_SMail;
