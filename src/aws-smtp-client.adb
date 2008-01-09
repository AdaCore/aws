------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                   S M T P - Simple Mail Transfer Protocol                --
--                                                                          --
--                         Copyright (C) 2000-2008                          --
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
--  As a special exception, if other files instantiate generics from this   --
--  unit, or you link this unit with other files to produce an executable,  --
--  this  unit  does not  by itself cause  the resulting executable to be   --
--  covered by the GNU General Public License. This exception does not      --
--  however invalidate any other reasons why the executable file  might be  --
--  covered by the  GNU Public License.                                     --
------------------------------------------------------------------------------

with Ada.Calendar;
with Ada.Text_IO;
with Ada.Exceptions;

with GNAT.Calendar.Time_IO;

with AWS.MIME;
with AWS.Net.Buffered;
with AWS.SMTP.Authentication;
pragma Warnings (Off, AWS.SMTP.Authentication);
--  Work around a visibility problem

package body AWS.SMTP.Client is

   procedure Open
     (Server : in     Receiver;
      Sock   :    out Net.Socket_Access;
      Status :    out SMTP.Status);
   --  Open session with a SMTP server

   procedure Close
     (Sock   : in out Net.Socket_Access;
      Status : in out SMTP.Status);
   --  Close session with the SMTP server.

   procedure Output_Header
     (Sock     : in     Net.Socket_Type'Class;
      From     : in     E_Mail_Data;
      To       : in     Recipients;
      Subject  : in     String;
      Status   :    out SMTP.Status;
      Is_MIME  : in     Boolean := False);
   --  Output SMTP headers (MAIL, RCPT, DATA, From, To, Subject, Date)

   procedure Put_Translated_Line
     (Sock : in Net.Socket_Type'Class;
      Text : in String);
   --  Translate a leading dot to two dots

   procedure Terminate_Mail_Data (Sock : in out Net.Socket_Type'Class);
   --  Send string CRLF & '.' & CRLF

   procedure Shutdown (Sock : in out Net.Socket_Access);
   --  Shutdown and close the socket. Do not raise an exception if the Socket
   --  is not connected.

   -----------------
   -- Base64_Data --
   -----------------

   function Base64_Data (Name, Content : in String) return Attachment is
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

   function File (Filename : in String) return Attachment is
   begin
      return (File, To_Unbounded_String (Filename));
   end File;

   ----------
   -- Open --
   ----------

   procedure Open
     (Server : in     Receiver;
      Sock   :    out Net.Socket_Access;
      Status :    out SMTP.Status)
   is
      Answer : Server_Reply;
   begin
      --  Clear status code
      Clear (Status);

      --  Open server
      Sock := Net.Socket (Security => False);

      Net.Connect (Sock.all, To_String (Server.Name), Server.Port);

      --  Check connect message
      Check_Answer (Sock.all, Answer);

      if Answer.Code = Service_Ready then

         --  Open session
         Net.Buffered.Put_Line
           (Sock.all, "HELO " & Net.Host_Name);
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
   end Open;

   -------------------
   -- Output_Header --
   -------------------

   procedure Output_Header
     (Sock     : in     Net.Socket_Type'Class;
      From     : in     E_Mail_Data;
      To       : in     Recipients;
      Subject  : in     String;
      Status   :    out SMTP.Status;
      Is_MIME  : in     Boolean := False)
   is
      function Current_Date return String;
      --  Returns current date and time for SMTP "Date:" field.

      ------------------
      -- Current_Date --
      ------------------

      function Current_Date return String is
      begin
         --  Format is: Mon, 1 Jan 2002 12:00:00
         return GNAT.Calendar.Time_IO.Image
           (Calendar.Clock, "%a, %-d %b %Y %T");
      end Current_Date;

      Answer : Server_Reply;

   begin
      --  MAIL
      Net.Buffered.Put_Line
        (Sock, "MAIL FROM:<" & Image (From, Address) & '>');

      Check_Answer (Sock, Answer);

      if Answer.Code = Requested_Action_Ok then

         --  RCPT
         for K in To'Range loop
            Net.Buffered.Put_Line
              (Sock,
               "RCPT TO:<" & Image (To (K), Address) & '>');

            Check_Answer (Sock, Answer);

            if Answer.Code /= Requested_Action_Ok then
               Add (Answer, Status);
            end if;
         end loop;

         if Is_Ok (Status) then

            --  DATA
            Net.Buffered.Put_Line (Sock, "DATA");
            Check_Answer (Sock, Answer);

            if Answer.Code = Start_Mail_Input then

               --  Time Stamp
               Net.Buffered.Put_Line (Sock, "Date: " & Current_Date);

               --  From
               Net.Buffered.Put_Line (Sock, "From: " & Image (From));

               --  Subject
               Net.Buffered.Put_Line (Sock, "Subject: " & Subject);

               --  To
               Net.Buffered.Put (Sock, "To: " & Image (To (To'First)));

               for K in To'First + 1 .. To'Last loop
                  Net.Buffered.Put (Sock, ", " & Image (To (K)));
               end loop;

               Net.Buffered.New_Line (Sock);

               if Is_MIME then
                  Net.Buffered.Put_Line
                    (Sock, "MIME-Version: 1.0 (produced by AWS/SMTP)");
               else
                  Net.Buffered.New_Line (Sock);
               end if;

            else
               --  Not possible to send mail header data.
               Add (Answer, Status);
            end if;
         end if;

      else
         --  Error in From address
         Add (Answer, Status);
      end if;
   end Output_Header;

   -------------------------
   -- Put_Translated_Line --
   -------------------------

   procedure Put_Translated_Line
     (Sock : in Net.Socket_Type'Class;
      Text : in String) is
   begin
      if Text'Length > 0 and then Text (Text'First) = '.' then
         Net.Buffered.Put (Sock, ".");
      end if;

      Net.Buffered.Put_Line (Sock, Text);
   end Put_Translated_Line;

   ----------
   -- Send --
   ----------

   procedure Send
     (Server  : in     Receiver;
      From    : in     E_Mail_Data;
      To      : in     E_Mail_Data;
      Subject : in     String;
      Message : in     String;
      Status  :    out SMTP.Status) is
   begin
      Send (Server, From, Recipients'(1 => To), Subject, Message, Status);
   end Send;

   ----------
   -- Send --
   ----------

   procedure Send
     (Server      : in     Receiver;
      From        : in     E_Mail_Data;
      To          : in     E_Mail_Data;
      Subject     : in     String;
      Message     : in     String := "";
      Attachments : in     Attachment_Set;
      Status      :    out SMTP.Status)
   is
   begin
      Send (Server, From, Recipients'(1 => To),
            Subject, Message, Attachments, Status);
   end Send;

   ----------
   -- Send --
   ----------

   procedure Send
     (Server   : in     Receiver;
      From     : in     E_Mail_Data;
      To       : in     E_Mail_Data;
      Subject  : in     String;
      Filename : in     Message_File;
      Status   :    out SMTP.Status)
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
              (Sock.all, From, Recipients'(1 => To), Subject, Status);

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
     (Server  : in     Receiver;
      From    : in     E_Mail_Data;
      To      : in     Recipients;
      Subject : in     String;
      Message : in     String;
      Status  :    out SMTP.Status)
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
            Output_Header (Sock.all, From, To, Subject, Status);

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
     (Server      : in     Receiver;
      From        : in     E_Mail_Data;
      To          : in     Recipients;
      Subject     : in     String;
      Message     : in     String := "";
      Attachments : in     Attachment_Set;
      Status      :    out SMTP.Status)
   is
      Att_List : AWS.Attachments.List;
   begin
      --  Fill attachments list

      if Message /= "" then
         AWS.Attachments.Add
           (Att_List,
            Content      => Message,
            Content_Type => MIME.Text_Plain);
      end if;

      for K in Attachments'Range loop
         declare
            A : constant Attachment := Attachments (K);
         begin
            case A.Kind is
               when File        =>
                  AWS.Attachments.Add
                    (Att_List,
                     Filename   => To_String (A.Name),
                     Content_Id => "",
                     Encode     => AWS.Attachments.Base64);

               when Base64_Data =>
                  AWS.Attachments.Add_Base64
                    (Att_List,
                     Name    => To_String (A.Name),
                     Content => To_String (A.Data));
            end case;
         end;
      end loop;

      Send (Server, From, To, Subject, Att_List, Status);
   end Send;

   procedure Send
     (Server      : in     Receiver;
      From        : in     E_Mail_Data;
      To          : in     Recipients;
      Subject     : in     String;
      Attachments : in     AWS.Attachments.List;
      Status      :    out SMTP.Status)
   is
      use type AWS.Attachments.Root_MIME_Kind;
      Sock               : Net.Socket_Access;
      Answer             : Server_Reply;
      Boundary           : Unbounded_String;
   begin
      Open (Server, Sock, Status);

      if Is_Ok (Status) then
         if Server.Auth /= null then
            Server.Auth.Before_Send (Sock.all, Status);
         end if;

         if Is_Ok (Status) then
            Output_Header
              (Sock.all, From, To, Subject, Status, Is_MIME => True);

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
