------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2000-2017, AdaCore                     --
--                                                                          --
--  This library is free software;  you can redistribute it and/or modify   --
--  it under terms of the  GNU General Public License  as published by the  --
--  Free Software  Foundation;  either version 3,  or (at your  option) any --
--  later version. This library is distributed in the hope that it will be  --
--  useful, but WITHOUT ANY WARRANTY;  without even the implied warranty of --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                    --
--                                                                          --
--  As a special exception under Section 7 of GPL version 3, you are        --
--  granted additional permissions described in the GCC Runtime Library     --
--  Exception, version 3.1, as published by the Free Software Foundation.   --
--                                                                          --
--  You should have received a copy of the GNU General Public License and   --
--  a copy of the GCC Runtime Library Exception along with this program;    --
--  see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see   --
--  <http://www.gnu.org/licenses/>.                                         --
--                                                                          --
--  As a special exception, if other files instantiate generics from this   --
--  unit, or you link this unit with other files to produce an executable,  --
--  this  unit  does not  by itself cause  the resulting executable to be   --
--  covered by the GNU General Public License. This exception does not      --
--  however invalidate any other reasons why the executable file  might be  --
--  covered by the  GNU Public License.                                     --
------------------------------------------------------------------------------

pragma Ada_2012;

with Ada.Calendar;
with Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Text_IO;

with GNAT.Calendar.Time_IO;

with AWS.Headers;
with AWS.Messages;
with AWS.MIME;
with AWS.Net.Buffered;
with AWS.SMTP.Authentication;
with AWS.Utils;

package body AWS.SMTP.Client is

   procedure Open
     (Server : Receiver;
      Sock   : out Net.Socket_Access;
      Status : out SMTP.Status);
   --  Open session with a SMTP server

   procedure Close
     (Sock   : in out Net.Socket_Access;
      Status : in out SMTP.Status);
   --  Close session with the SMTP server

   procedure Output_Header
     (Sock    : Net.Socket_Type'Class;
      From    : E_Mail_Data;
      To      : Recipients;
      CC      : Recipients;
      BCC     : Recipients;
      Subject : String;
      Status  : out SMTP.Status;
      Is_MIME : Boolean := False;
      To_All  : Boolean);
   --  Output SMTP headers (MAIL, RCPT, DATA, From, To, Subject, Date)

   procedure Output_Simple_Header
     (Sock   : Net.Socket_Type'Class;
      From   : E_Mail_Data;
      To     : Recipients;
      CC     : Recipients;
      BCC    : Recipients;
      Status : out SMTP.Status;
      To_All : Boolean);
   --  Output SMTP protocol commands (MAIL, RCPT, DATA)

   procedure Put_Translated_Line
     (Sock : Net.Socket_Type'Class;
      Text : String);
   --  Translate a leading dot to two dots

   procedure Terminate_Mail_Data (Sock : in out Net.Socket_Type'Class);
   --  Send string CRLF & '.' & CRLF

   procedure Shutdown (Sock : in out Net.Socket_Access);
   --  Shutdown and close the socket. Do not raise an exception if the Socket
   --  is not connected.

   -----------------
   -- Base64_Data --
   -----------------

   function Base64_Data (Name, Content : String) return Attachment is
   begin
      return (Base64_Data,
              To_Unbounded_String (Name), To_Unbounded_String (Content));
   end Base64_Data;

   -----------
   -- Close --
   -----------

   procedure Close
     (Sock   : in out Net.Socket_Access;
      Status : in out SMTP.Status)
   is
      Answer : Server_Reply;
   begin
      Net.Buffered.Put_Line (Sock.all, "QUIT");

      Check_Answer (Sock.all, Answer);

      if Answer.Code /= Service_Closing then
         Add (Answer, Status);
      end if;

      Net.Buffered.Shutdown (Sock.all);
      Net.Free (Sock);
   end Close;

   ----------
   -- File --
   ----------

   function File (Filename : String) return Attachment is
   begin
      return (File, To_Unbounded_String (Filename));
   end File;

   ----------
   -- Open --
   ----------

   procedure Open
     (Server : Receiver;
      Sock   : out Net.Socket_Access;
      Status : out SMTP.Status)
   is
      Answer : Server_Reply;
   begin
      --  Clear status code
      Clear (Status);

      --  Open server
      Sock := Net.Socket (Security => Server.Secure);

      Sock.Set_Timeout (Server.Timeout);
      Sock.Connect
        (To_String (Server.Name), Server.Port, Family => Server.Family);

      --  Check connect message
      Check_Answer (Sock.all, Answer);

      if Answer.Code = Service_Ready then

         --  Open session
         Net.Buffered.Put_Line (Sock.all, "HELO " & Net.Host_Name);
         Check_Answer (Sock.all, Answer);

         --  If no success, close the connection

         if Answer.Code /= Requested_Action_Ok then
            Add (Answer, Status);
            Shutdown (Sock);
         end if;

      else
         Add (Answer, Status);
         Shutdown (Sock);
      end if;
   exception
      when others =>
         Shutdown (Sock);
         raise;
   end Open;

   -------------------
   -- Output_Header --
   -------------------

   procedure Output_Header
     (Sock    : Net.Socket_Type'Class;
      From    : E_Mail_Data;
      To      : Recipients;
      CC      : Recipients;
      BCC     : Recipients;
      Subject : String;
      Status  : out SMTP.Status;
      Is_MIME : Boolean := False;
      To_All  : Boolean)
   is
      function Current_Date return String;
      --  Returns current date and time for SMTP "Date:" field

      procedure Send (Header_Name : String; Emails : Recipients);
      --  Write header Header_Name for the corresponding recipients

      ------------------
      -- Current_Date --
      ------------------

      function Current_Date return String is
         TZ : constant String := Utils.Time_Zone;
      begin
         --  Format is: Mon, 1 Jan 2002 12:00:00 (+/-)HHMM

         if TZ = "" then
            return GNAT.Calendar.Time_IO.Image
              (Calendar.Clock, "%a, %-d %b %Y %T");
         else
            return GNAT.Calendar.Time_IO.Image
              (Calendar.Clock, "%a, %-d %b %Y %T ") & TZ;
         end if;
      end Current_Date;

      ----------
      -- Send --
      ----------

      procedure Send (Header_Name : String; Emails : Recipients) is
      begin
         if Emails /= No_Recipient then
            Net.Buffered.Put
              (Sock, Header_Name & ": " & Image (Emails (Emails'First)));

            for K in Emails'First + 1 .. Emails'Last loop
               Net.Buffered.Put (Sock, ", " & Image (Emails (K)));
            end loop;

            Net.Buffered.New_Line (Sock);
         end if;
      end Send;

   begin
      --  Output the MAIL, RCPT and DATA headers
      Output_Simple_Header (Sock, From, To, CC, BCC, Status, To_All);

      if Is_Ok (Status) then
         --  Time Stamp
         Net.Buffered.Put_Line (Sock, "Date: " & Current_Date);

         --  From
         Net.Buffered.Put_Line (Sock, "From: " & Image (From));

         --  Subject
         Net.Buffered.Put_Line (Sock, "Subject: " & Subject);

         --  To
         Send ("To", To);

         --  CC
         Send ("Cc", CC);

         --  Note that BCC is not sent here as these are the data headers
         --  sent to the client. The BCC recipients have been added into the
         --  RCPT TO: fields.

         if Is_MIME then
            Net.Buffered.Put_Line
              (Sock, "MIME-Version: 1.0 (produced by AWS/SMTP)");
         else
            Net.Buffered.New_Line (Sock);
         end if;
      end if;
   end Output_Header;

   --------------------------
   -- Output_Simple_Header --
   --------------------------

   procedure Output_Simple_Header
     (Sock   : Net.Socket_Type'Class;
      From   : E_Mail_Data;
      To     : Recipients;
      CC     : Recipients;
      BCC    : Recipients;
      Status : out SMTP.Status;
      To_All : Boolean)
   is
      Have_One : Boolean := False;

      procedure Send (Emails : Recipients);
      --  Procedure set to this recipient

      ----------
      -- Send --
      ----------

      procedure Send (Emails : Recipients) is
         Answer : Server_Reply;
      begin
         for K in Emails'Range loop
            Net.Buffered.Put_Line
              (Sock,
               "RCPT TO:<" & Image (Emails (K), Address) & '>');

            Check_Answer (Sock, Answer);

            if Answer.Code = Requested_Action_Ok then
               Have_One := True;
            else
               Add (Answer, Status);
            end if;
         end loop;
      end Send;

      Answer : Server_Reply;
   begin
      --  MAIL
      Net.Buffered.Put_Line
        (Sock, "MAIL FROM:<" & Image (From, Address) & '>');

      Check_Answer (Sock, Answer);

      if Answer.Code = Requested_Action_Ok then
         --  RCPT

         Send (To);

         Send (CC);

         Send (BCC);

         if not To_All and then Have_One then
            Status := (Code => Requested_Action_Ok,
                       Reason => Null_Unbounded_String,
                       Warnings => Status.Reason);
         end if;

         if Is_Ok (Status) then
            --  DATA
            Net.Buffered.Put_Line (Sock, "DATA");
            Check_Answer (Sock, Answer);

            if Answer.Code /= Start_Mail_Input then
               --  Not possible to send mail header data
               Add (Answer, Status);
            end if;
         end if;

      else
         --  Error in From address
         Add (Answer, Status);
      end if;
   end Output_Simple_Header;

   -------------------------
   -- Put_Translated_Line --
   -------------------------

   procedure Put_Translated_Line
     (Sock : Net.Socket_Type'Class; Text : String)
   is
      use Ada.Strings;
      First  : Natural := Text'First;
      LF_Dot : Natural;
   begin
      if Text'Length > 0 and then Text (Text'First) = '.' then
         Net.Buffered.Put (Sock, ".");
      end if;

      loop
         LF_Dot := Fixed.Index (Text, ASCII.LF & '.', First);
         exit when LF_Dot = 0;

         Net.Buffered.Put (Sock, Text (First .. LF_Dot + 1));

         --  Write dot after line feed twice

         First := LF_Dot + 1;
      end loop;

      Net.Buffered.Put_Line (Sock, Text (First .. Text'Last));
   end Put_Translated_Line;

   ----------
   -- Send --
   ----------

   procedure Send
     (Server  : Receiver;
      From    : E_Mail_Data;
      To      : E_Mail_Data;
      Subject : String;
      Message : String;
      Status  : out SMTP.Status;
      CC      : Recipients := No_Recipient;
      BCC     : Recipients := No_Recipient;
      To_All  : Boolean    := True) is
   begin
      Send (Server, From, Recipients'(1 => To),
            Subject, Message, Status, CC, BCC, To_All);
   end Send;

   ----------
   -- Send --
   ----------

   procedure Send
     (Server      : Receiver;
      From        : E_Mail_Data;
      To          : E_Mail_Data;
      Subject     : String;
      Message     : String := "";
      Attachments : Attachment_Set;
      Status      : out SMTP.Status;
      CC          : Recipients := No_Recipient;
      BCC         : Recipients := No_Recipient;
      To_All      : Boolean    := True) is
   begin
      Send (Server, From, Recipients'(1 => To),
            Subject, Message, Attachments, Status, CC, BCC, To_All);
   end Send;

   ----------
   -- Send --
   ----------

   procedure Send
     (Server   : Receiver;
      From     : E_Mail_Data;
      To       : E_Mail_Data;
      Subject  : String;
      Filename : Message_File;
      Status   : out SMTP.Status;
      CC       : Recipients := No_Recipient;
      BCC      : Recipients := No_Recipient;
      To_All   : Boolean    := True)
   is
      Buffer : String (1 .. 2_048);
      Last   : Natural;
      File   : Text_IO.File_Type;

      Sock   : Net.Socket_Access;
      Answer : Server_Reply;
   begin
      --  Open server
      Open (Server, Sock, Status);

      if Is_Ok (Status) then
         if Server.Auth /= null then
            Server.Auth.Before_Send (Sock.all, Status);
         end if;

         if Is_Ok (Status) then
            Output_Header
              (Sock.all, From, Recipients'(1 => To), CC, BCC, Subject, Status,
               To_All => To_All);

            if Is_Ok (Status) then
               --  Message body
               Text_IO.Open (File, Text_IO.In_File, String (Filename));

               while not Text_IO.End_Of_File (File) loop
                  Text_IO.Get_Line (File, Buffer, Last);
                  Put_Translated_Line (Sock.all, Buffer (1 .. Last));
               end loop;

               Text_IO.Close (File);

               Terminate_Mail_Data (Sock.all);

               Check_Answer (Sock.all, Answer);

               if Is_Ok (Status) and then Server.Auth /= null then
                  Server.Auth.After_Send (Sock.all, Status);
               end if;

               if Answer.Code /= Requested_Action_Ok then
                  Add (Answer, Status);
               end if;
            end if;
         end if;

         Close (Sock, Status);
      end if;

   exception
      --  Raise Server_Error for all problems encountered

      when E : others =>
         Shutdown (Sock);

         if Text_IO.Is_Open (File) then
            Text_IO.Close (File);
         end if;

         raise Server_Error with Ada.Exceptions.Exception_Information (E);
   end Send;

   ----------
   -- Send --
   ----------

   procedure Send
     (Server  : Receiver;
      From    : E_Mail_Data;
      To      : Recipients;
      Subject : String;
      Message : String;
      Status  : out SMTP.Status;
      CC      : Recipients := No_Recipient;
      BCC     : Recipients := No_Recipient;
      To_All  : Boolean    := True)
   is
      Sock   : Net.Socket_Access;
      Answer : Server_Reply;
   begin
      Open (Server, Sock, Status);

      if Is_Ok (Status) then
         if Server.Auth /= null then
            Server.Auth.Before_Send (Sock.all, Status);
         end if;

         if Is_Ok (Status) then
            Output_Header
              (Sock.all, From, To, CC, BCC, Subject, Status, To_All => To_All);

            if Is_Ok (Status) then
               --  Message body
               Put_Translated_Line (Sock.all, Message);

               Terminate_Mail_Data (Sock.all);

               Check_Answer (Sock.all, Answer);

               if Is_Ok (Status) and then Server.Auth /= null then
                  Server.Auth.After_Send (Sock.all, Status);
               end if;

               if Answer.Code /= Requested_Action_Ok then
                  Add (Answer, Status);
               end if;
            end if;
         end if;

         Close (Sock, Status);
      end if;

   exception
      --  Raise Server_Error for all problems encountered

      when E : others =>
         Shutdown (Sock);
         raise Server_Error with Ada.Exceptions.Exception_Information (E);
   end Send;

   ----------
   -- Send --
   ----------

   procedure Send
     (Server : Receiver;
      From   : E_Mail_Data;
      To     : Recipients;
      Source : String;
      Status : out SMTP.Status;
      CC     : Recipients := No_Recipient;
      BCC    : Recipients := No_Recipient;
      To_All : Boolean    := True)
   is
      Sock   : Net.Socket_Access;
      Answer : Server_Reply;
   begin
      Open (Server, Sock, Status);

      if Is_Ok (Status) then
         if Server.Auth /= null then
            Server.Auth.Before_Send (Sock.all, Status);
         end if;

         if Is_Ok (Status) then
            Output_Simple_Header (Sock.all, From, To, CC, BCC, Status, To_All);

            if Is_Ok (Status) then
               --  Message body
               Put_Translated_Line (Sock.all, Source);

               Terminate_Mail_Data (Sock.all);

               Check_Answer (Sock.all, Answer);

               if Is_Ok (Status) and then Server.Auth /= null then
                  Server.Auth.After_Send (Sock.all, Status);
               end if;

               if Answer.Code /= Requested_Action_Ok then
                  Add (Answer, Status);
               end if;
            end if;
         end if;

         Close (Sock, Status);
      end if;

   exception
      --  Raise Server_Error for all problems encountered

      when E : others =>
         Shutdown (Sock);
         raise Server_Error with Ada.Exceptions.Exception_Information (E);
   end Send;

   ----------
   -- Send --
   ----------

   procedure Send
     (Server      : Receiver;
      From        : E_Mail_Data;
      To          : Recipients;
      Subject     : String;
      Message     : String := "";
      Attachments : Attachment_Set;
      Status      : out SMTP.Status;
      CC          : Recipients := No_Recipient;
      BCC         : Recipients := No_Recipient;
      To_All      : Boolean    := True)
   is
      Att_List : AWS.Attachments.List;
   begin
      --  Fill attachments list

      if Message /= "" then
         AWS.Attachments.Add
           (Att_List,
            "",
            AWS.Attachments.Value
              (Data         => Message,
               Content_Type => MIME.Text_Plain));
      end if;

      for A of Attachments loop
         declare
            H : AWS.Headers.List;
         begin
            case A.Kind is
               when File        =>
                  AWS.Attachments.Add
                    (Att_List,
                     Filename   => To_String (A.Name),
                     Content_Id => "",
                     Encode     => AWS.Attachments.Base64);

               when Base64_Data =>
                  H.Add (Messages.Content_Transfer_Encoding_Token, "base64");

                  AWS.Attachments.Add
                    (Att_List,
                     To_String (A.Name),
                     AWS.Attachments.Value
                       (Name   => To_String (A.Name),
                        Data   => To_String (A.Data)),
                     Headers => H);
            end case;
         end;
      end loop;

      Send (Server, From, To, Subject, Att_List, Status, CC, BCC, To_All);
   end Send;

   procedure Send
     (Server      : Receiver;
      From        : E_Mail_Data;
      To          : Recipients;
      Subject     : String;
      Attachments : AWS.Attachments.List;
      Status      : out SMTP.Status;
      CC          : Recipients := No_Recipient;
      BCC         : Recipients := No_Recipient;
      To_All      : Boolean    := True)
   is
      Sock     : Net.Socket_Access;
      Answer   : Server_Reply;
      Boundary : Unbounded_String;
   begin
      Open (Server, Sock, Status);

      if Is_Ok (Status) then
         if Server.Auth /= null then
            Server.Auth.Before_Send (Sock.all, Status);
         end if;

         if Is_Ok (Status) then
            Output_Header
              (Sock.all, From, To, CC, BCC, Subject, Status, Is_MIME => True,
               To_All => To_All);

            if Is_Ok (Status) then
               --  Send MIME header

               AWS.Attachments.Send_MIME_Header
                 (Sock.all, Attachments, Boundary);

               --  Message for non-MIME compliant Mail reader

               Net.Buffered.Put_Line
                 (Sock.all, "This is multipart MIME message");
               Net.Buffered.Put_Line
                 (Sock.all,
                  "If you read this, your mailer does not support MIME.");
               Net.Buffered.New_Line (Sock.all);

               AWS.Attachments.Send
                 (Sock.all, Attachments, To_String (Boundary));

               Terminate_Mail_Data (Sock.all);

               Check_Answer (Sock.all, Answer);

               if Is_Ok (Status) and then Server.Auth /= null then
                  Server.Auth.After_Send (Sock.all, Status);
               end if;

               if Answer.Code /= Requested_Action_Ok then
                  Add (Answer, Status);
               end if;
            end if;
         end if;

         Close (Sock, Status);
      end if;

   exception
      --  Raise Server_Error for all problem encountered

      when E : others =>
         Shutdown (Sock);
         raise Server_Error with Ada.Exceptions.Exception_Information (E);
   end Send;

   --------------
   -- Shutdown --
   --------------

   procedure Shutdown (Sock : in out Net.Socket_Access) is
      use type Net.Socket_Access;
   begin
      if Sock /= null then
         Net.Buffered.Shutdown (Sock.all);
         Net.Free (Sock);
      end if;
   end Shutdown;

   -------------------------
   -- Terminate_Mail_Data --
   -------------------------

   procedure Terminate_Mail_Data (Sock : in out Net.Socket_Type'Class) is
   begin
      Net.Buffered.New_Line (Sock);
      Net.Buffered.Put (Sock, ".");
      Net.Buffered.New_Line (Sock);
   end Terminate_Mail_Data;

end AWS.SMTP.Client;
