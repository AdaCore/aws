------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                   S M T P - Simple Mail Transfer Protocol                --
--                                                                          --
--                     Copyright (C) 2000-2011, AdaCore                     --
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

--
--  This unit implements an API to send email messages. It is possible to send
--  simple email [RFC 821] and email with MIME attachments [RFC 2045 & 2049].
--
--  How to send an email:
--
--  1) Initialize a Server to send the messages.
--
--     Wanadoo : SMTP.Receiver := SMTP.Client.Initialize ("smtp.wanadoo.fr");
--
--     Optionally, request Authentication
--
--     Auth : aliased SMTP.Authentication.Credential :=
--              SMTP.Authentication.Plain.Initialize ("id", "password");
--
--     Wanadoo : SMTP.Receiver :=
--                 SMTP.Client.Initialize
--                   ("smtp.wanadoo.fr", Credential => Auth'Access);
--
--  2) Send a message via the server.
--
--     Result : SMTP.Status;
--
--     SMTP.Client.Send
--        (Server  => Wanadoo,
--         From    => SMTP.E_Mail ("Pascal Obry", "pascal@obry.net"),
--         To      => SMTP.E_Mail
--                      ("Dmitriy Anisimkov", "anisimkov@ada-ru.org"),
--         Subject => "Latest Ada news",
--         Message => "now Ada can send SMTP mail!",
--         Status  => Result);

with Ada.Strings.Unbounded;

with AWS.Attachments;

package AWS.SMTP.Client is

   use Ada.Strings.Unbounded;

   Server_Error : exception renames SMTP.Server_Error;

   function Initialize
     (Server_Name : String;
      Port        : Positive := Default_SMTP_Port;
      Credential  : access constant Authentication.Credential'Class := null)
      return Receiver renames SMTP.Initialize;

   procedure Send
     (Server  : Receiver;
      From    : E_Mail_Data;
      To      : E_Mail_Data;
      Subject : String;
      Message : String;
      Status  : out SMTP.Status);
   --  Send a message via Server. The email is a simple message composed of a
   --  subject and a text message body. Raise Server_Error in case of an
   --  unrecoverable error (e.g. can't contact the server).

   type Attachment is private;
   --  This is an attachment object, either a File or some Base64 encoded
   --  content.
   --  only simple attachments are supported. For full attachment support use
   --  AWS.Attachments with the corresponding Send routine below.

   function File (Filename : String) return Attachment;
   --  Returns a file attachment. Filename point to a file on the file system

   function Base64_Data (Name, Content : String) return Attachment;
   --  Returns a base64 encoded attachment. Content must already be Base64
   --  encoded data. The attachment is named Name.
   --  This is a way to send a file attachment from in-memory data.

   type Attachment_Set is array (Positive range <>) of Attachment;
   --  A set of file attachments.

   procedure Send
     (Server      : Receiver;
      From        : E_Mail_Data;
      To          : E_Mail_Data;
      Subject     : String;
      Message     : String := "";
      Attachments : Attachment_Set;
      Status      : out SMTP.Status);
   --  Send a message via Server. The email is a MIME message composed of a
   --  subject, a message and a set of MIME encoded files. Raise Server_Error
   --  in case of an unrecoverable error (e.g. can't contact the server).
   --  Raises Constraint_Error if a file attachment cannot be opened.

   type Message_File is new String;

   procedure Send
     (Server   : Receiver;
      From     : E_Mail_Data;
      To       : E_Mail_Data;
      Subject  : String;
      Filename : Message_File;
      Status   : out SMTP.Status);
   --  Send filename content via Server. The email is a message composed of a
   --  subject and a message body coming from a file. Raises Server_Error in
   --  case of an unrecoverable error (e.g. can't contact the server). Raises
   --  Constraint_Error if Filename cannot be opened.

   --
   --  Extentded interfaces to send a message to many recipients
   --

   procedure Send
     (Server  : Receiver;
      From    : E_Mail_Data;
      To      : Recipients;
      Subject : String;
      Message : String;
      Status  : out SMTP.Status);
   --  Send a message via Server. The mail is a simple message composed of a
   --  subject and a text message body. Raise Server_Error in case of an
   --  unrecoverable error (e.g. can't contact the server).

   procedure Send
     (Server : Receiver;
      From   : E_Mail_Data;
      To     : Recipients;
      Source : String;
      Status : out SMTP.Status);
   --  Send a message via Server. The email Source has already been composed by
   --  other means, such as the GNATcoll email facilities.
   --  Raise Server_Error in case of an unrecoverable error, e.g. can't contact
   --  the server.

   procedure Send
     (Server      : Receiver;
      From        : E_Mail_Data;
      To          : Recipients;
      Subject     : String;
      Message     : String := "";
      Attachments : Attachment_Set;
      Status      : out SMTP.Status);
   --  Send a message via Server. The email is a MIME message composed of a
   --  subject, a message and a set of files MIME encoded. Raise Server_Error
   --  in case of an unrecoverable error (e.g. can't contact the server).
   --  Raises Constraint_Error if a file attachment cannot be opened.

   procedure Send
     (Server      : Receiver;
      From        : E_Mail_Data;
      To          : Recipients;
      Subject     : String;
      Attachments : AWS.Attachments.List;
      Status      : out SMTP.Status);
   --  As above but takes an attachment list which support complex attachments
   --  like multiplart/alternative.

private

   use Ada;

   type Attachment_Mode is (File, Base64_Data);

   type Attachment (Kind : Attachment_Mode := File) is record
      Name : Unbounded_String;

      case Kind is
         when File =>
            null;
         when Base64_Data =>
            Data : Unbounded_String;
      end case;
   end record;

end AWS.SMTP.Client;
