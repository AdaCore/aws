------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                   S M T P - Simple Mail Transfer Protocol                --
--                                                                          --
--                         Copyright (C) 2000-2002                          --
--                                ACT-Europe                                --
--                                                                          --
--  Authors: Dmitriy Anisimkov - Pascal Obry                                --
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

--  $Id$

with Ada.Calendar;
with Ada.Text_IO;
with Ada.Exceptions;
with Ada.Streams.Stream_IO;

with GNAT.Calendar.Time_IO;
with GNAT.Directory_Operations;

with AWS.Translator;
with Sockets;
with Sockets.Naming;

package body AWS.SMTP.Client is

   use Ada;

   --
   --  Status codes in server replies
   --

   type Server_Reply is record
      Code   : Reply_Code;
      Reason : Unbounded_String;
   end record;

   function Image (Answer : in Server_Reply) return String;
   --  Returns the string representation for Answer.

   procedure Check_Answer
     (Sock  : in     Sockets.Socket_FD;
      Reply :    out Server_Reply);
   --  Read a reply from the SMTP server (listening on Sock) and fill the Reply
   --  structure.

   type Address_Mode is (Full, Name, Address);

   function E_Mail
     (E_Mail : in E_Mail_Data;
      Mode   : in Address_Mode := Full) return String;
   --  Returns E_Mail only (Mode = Address), recipient name only (Mode = Name)
   --  or Name and e-mail (Mode = Full).

   procedure Add (Answer : in out Server_Reply; Status : in out SMTP.Status);
   --  Add status code and reason to the list of server's reply.

   procedure Open
     (Server : in     Receiver;
      Sock   :    out Sockets.Socket_FD;
      Status :    out SMTP.Status);
   --  Open session with a SMTP server

   procedure Close
     (Sock   : in out Sockets.Socket_FD;
      Status : in out SMTP.Status);
   --  Close session with the SMTP server.

   procedure Output_Header
     (Sock     : in     Sockets.Socket_FD;
      From     : in     E_Mail_Data;
      To       : in     Recipients;
      Subject  : in     String;
      Status   :    out SMTP.Status;
      Complete : in     Boolean := True);
   --  Output SMTP headers (MAIL, RCPT, DATA, From, To, Subject, Date)

   procedure Output_MIME_Header
     (Sock     : in     Sockets.Socket_FD;
      Boundary :    out Unbounded_String);
   --  Output MIME SMTP headers, return the MIME boundary

   procedure Put_Translated_Line
     (Sock : in Sockets.Socket_FD;
      Text : in String);
   --  Translate a leading dot to two dots

   procedure Terminate_Mail_Data (Sock : in out Sockets.Socket_FD);
   --  Send string CRLF & '.' & CRLF

   procedure Send_MIME_Attachment
     (Sock : in Sockets.Socket_FD; File : in Attachment);
   --  Send file Filename as a MIME attachment. This procedure send the MIME
   --  attachment headers but does not send the MIME boundary.

   procedure Send_MIME_Message
     (Sock : in Sockets.Socket_FD; Message : in String);
   --  Send textual message as a MIME content. This procedure send the
   --  MIME headers but does not send the MIME boundary.

   ---------
   -- Add --
   ---------

   procedure Add (Answer : in out Server_Reply; Status : in out SMTP.Status) is
   begin
      if Status.Value /= Null_Unbounded_String then
         Append (Status.Value, ASCII.LF);
      end if;

      Append (Status.Value, Image (Answer));

      Status.Code := Answer.Code;
   end Add;

   -----------------
   -- Base64_Data --
   -----------------

   function Base64_Data (Name, Content : in String) return Attachment is
   begin
      return (Base64_Data,
              To_Unbounded_String (Name), To_Unbounded_String (Content));
   end Base64_Data;

   ------------------
   -- Check_Answer --
   ------------------

   procedure Check_Answer
     (Sock  : in     Sockets.Socket_FD;
      Reply :    out Server_Reply)
   is
      Buffer : constant String := Sockets.Get_Line (Sock);
   begin
      Reply :=
        (Reply_Code'Value (Buffer (Buffer'First .. Buffer'First + 2)),
         To_Unbounded_String (Buffer (Buffer'First + 4 .. Buffer'Last)));
   end Check_Answer;

   -----------
   -- Close --
   -----------

   procedure Close
     (Sock   : in out Sockets.Socket_FD;
      Status : in out SMTP.Status)
   is
      Answer : Server_Reply;
   begin
      Sockets.Put_Line (Sock, "QUIT");

      Check_Answer (Sock, Answer);

      if Answer.Code /= Service_Closing then
         Add (Answer, Status);
      end if;

      Sockets.Shutdown (Sock);
   end Close;

   ------------
   -- E_Mail --
   ------------

   function E_Mail
     (E_Mail : in E_Mail_Data;
      Mode   : in Address_Mode := Full) return String is
   begin
      case Mode is
         when Full =>
            return To_String (E_Mail.Name)
              & " <" & To_String (E_Mail.Address) & '>';
         when Name =>
            return To_String (E_Mail.Name);
         when Address =>
            return To_String (E_Mail.Address);
      end case;
   end E_Mail;

   ----------
   -- File --
   ----------

   function File (Filename : in String) return Attachment is
   begin
      return (File, To_Unbounded_String (Filename));
   end File;

   -----------
   -- Image --
   -----------

   function Image (Answer : in Server_Reply) return String is
      Code_Image : constant String := Reply_Code'Image (Answer.Code);
   begin
      return Code_Image (Code_Image'First + 1 .. Code_Image'Last)
        & ' '
        & To_String (Answer.Reason);
   end Image;

   ----------------
   -- Initialize --
   ----------------

   function Initialize
     (Server_Name : in String;
      Port        : in Positive := Default_SMTP_Port)
      return Receiver
   is
      Dummy : Sockets.Socket_FD;
      pragma Warnings (Off, Dummy);
   begin
      return (To_Unbounded_String (Server_Name), Port, Dummy);
   end Initialize;

   ----------
   -- Open --
   ----------

   procedure Open
     (Server : in     Receiver;
      Sock   :    out Sockets.Socket_FD;
      Status :    out SMTP.Status)
   is
      Answer : Server_Reply;
   begin
      --  Clear status code
      Clear (Status);

      --  Open server
      Sockets.Socket (Sock, Sockets.AF_INET, Sockets.SOCK_STREAM);
      Sockets.Connect (Sock, To_String (Server.Name), Server.Port);

      --  Check connect message
      Check_Answer (Sock, Answer);

      if Answer.Code = Service_Ready then

         --  Open session
         Sockets.Put_Line (Sock, "HELO " & Sockets.Naming.Host_Name);
         Check_Answer (Sock, Answer);

         --  If no succes close the connection
         if Answer.Code /= Requested_Action_Ok then
            Add (Answer, Status);
            Sockets.Shutdown (Sock);
         end if;

      else
         Add (Answer, Status);
         Sockets.Shutdown (Sock);
      end if;
   end Open;

   -------------------
   -- Output_Header --
   -------------------

   procedure Output_Header
     (Sock     : in     Sockets.Socket_FD;
      From     : in     E_Mail_Data;
      To       : in     Recipients;
      Subject  : in     String;
      Status   :    out SMTP.Status;
      Complete : in     Boolean := True)
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
      Sockets.Put_Line (Sock, "MAIL FROM:<" & E_Mail (From, Address) & '>');
      Check_Answer (Sock, Answer);

      if Answer.Code = Requested_Action_Ok then

         --  RCPT
         for K in To'Range loop
            Sockets.Put_Line (Sock,
                              "RCPT TO:<" & E_Mail (To (K), Address) & '>');
            Check_Answer (Sock, Answer);

            if Answer.Code /= Requested_Action_Ok then
               Add (Answer, Status);
            end if;
         end loop;

         if Is_Ok (Status) then

            --  DATA
            Sockets.Put_Line (Sock, "DATA");
            Check_Answer (Sock, Answer);

            if Answer.Code = Start_Mail_Input then

               --  Time Stamp
               Sockets.Put_Line (Sock, "Date: " & Current_Date);

               --  From
               Sockets.Put_Line (Sock, "From: " & E_Mail (From));

               --  Subject
               Sockets.Put_Line (Sock, "Subject: " & Subject);

               --  To
               Sockets.Put (Sock, "To: " & E_Mail (To (To'First)));

               for K in To'First + 1 .. To'Last loop
                  Sockets.Put (Sock, ", " & E_Mail (To (K)));
               end loop;

               Sockets.New_Line (Sock);

               if Complete then
                  Sockets.New_Line (Sock);
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

   --------------------------
   -- Output_MIME_Boundary --
   --------------------------

   procedure Output_MIME_Header
     (Sock     : in     Sockets.Socket_FD;
      Boundary :    out Unbounded_String)
   is
      L_Boundary : constant String
        := GNAT.Calendar.Time_IO.Image (Calendar.Clock, "----=_NextPart_%s");
   begin
      Boundary := To_Unbounded_String (L_Boundary);

      Sockets.Put_Line (Sock, "MIME-Version: 1.0 (produced by AWS/SMTP)");
      Sockets.Put_Line (Sock, "Content-Type: multipart/mixed;");
      Sockets.Put_Line (Sock, "    boundary =""" & L_Boundary & '"');
      Sockets.New_Line (Sock);
   end Output_MIME_Header;

   -------------------------
   -- Put_Translated_Line --
   -------------------------

   procedure Put_Translated_Line
     (Sock : in Sockets.Socket_FD;
      Text : in String) is
   begin
      if Text'Length > 0 and then Text (Text'First) = '.' then
         Sockets.Put (Sock, ".");
      end if;

      Sockets.Put_Line (Sock, Text);
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
      Message     : in     String;
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

      Sock   : Sockets.Socket_FD;
      Answer : Server_Reply;
   begin
      --  Open server
      Open (Server, Sock, Status);

      if Is_Ok (Status) then

         Output_Header (Sock, From, Recipients'(1 => To), Subject, Status);

         if Is_Ok (Status) then
            --  Message body
            Text_IO.Open (File, Text_IO.In_File, String (Filename));

            while not Text_IO.End_Of_File (File) loop
               Text_IO.Get_Line (File, Buffer, Last);
               Put_Translated_Line (Sock, Buffer (1 .. Last));
            end loop;

            Text_IO.Close (File);

            Terminate_Mail_Data (Sock);

            Check_Answer (Sock, Answer);

            if Answer.Code /= Requested_Action_Ok then
               Add (Answer, Status);
            end if;
         end if;

         Close (Sock, Status);
      end if;

   exception
      --  Raise Server_Error for all problems encountered

      when E : others =>
         Sockets.Shutdown (Sock);

         if Text_IO.Is_Open (File) then
            Text_IO.Close (File);
         end if;

         Ada.Exceptions.Raise_Exception
           (Server_Error'Identity, Ada.Exceptions.Exception_Information (E));

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
      Sock   : Sockets.Socket_FD;
      Answer : Server_Reply;
   begin
      Open (Server, Sock, Status);

      if Is_Ok (Status) then

         Output_Header (Sock, From, To, Subject, Status);

         if Is_Ok (Status) then
            --  Message body
            Put_Translated_Line (Sock, Message);

            Terminate_Mail_Data (Sock);

            Check_Answer (Sock, Answer);

            if Answer.Code /= Requested_Action_Ok then
               Add (Answer, Status);
            end if;

         end if;

         Close (Sock, Status);
      end if;

   exception
      --  Raise Server_Error for all problems encountered

      when E : others =>
         Sockets.Shutdown (Sock);

         Ada.Exceptions.Raise_Exception
           (Server_Error'Identity, Ada.Exceptions.Exception_Information (E));

   end Send;

   ----------
   -- Send --
   ----------

   procedure Send
     (Server      : in     Receiver;
      From        : in     E_Mail_Data;
      To          : in     Recipients;
      Subject     : in     String;
      Message     : in     String;
      Attachments : in     Attachment_Set;
      Status      :    out SMTP.Status)
   is
      Sock     : Sockets.Socket_FD;
      Answer   : Server_Reply;
      Boundary : Unbounded_String;
   begin
      Open (Server, Sock, Status);

      if Is_Ok (Status) then

         Output_Header (Sock, From, To, Subject, Status, Complete => False);

         if Is_Ok (Status) then
            --  Send MIME header

            Output_MIME_Header (Sock, Boundary);

            --  Message for non-MIME compliant Mail reader

            Sockets.Put_Line (Sock, "This is multipart MIME message");
            Sockets.Put_Line
              (Sock, "If you read this, your mailer does not support MIME");
            Sockets.New_Line (Sock);

            --  Message body as the first MIME content

            Sockets.Put_Line (Sock, "--" & To_String (Boundary));

            Send_MIME_Message (Sock, Message);

            --  Send attachments

            Sockets.New_Line (Sock);

            for K in Attachments'Range loop
               Sockets.Put_Line (Sock, "--" & To_String (Boundary));

               Send_MIME_Attachment (Sock, Attachments (K));
            end loop;

            --  Send termination boundary
            Sockets.New_Line (Sock);
            Sockets.Put_Line (Sock, "--" & To_String (Boundary) & "--");

            Terminate_Mail_Data (Sock);

            Check_Answer (Sock, Answer);

            if Answer.Code /= Requested_Action_Ok then
               Add (Answer, Status);
            end if;

         end if;

         Close (Sock, Status);
      end if;

   exception
      --  Raise Server_Error for all problem encountered

      when E : others =>
         Sockets.Shutdown (Sock);

         Ada.Exceptions.Raise_Exception
           (Server_Error'Identity, Ada.Exceptions.Exception_Information (E));
   end Send;

   --------------------------
   -- Send_MIME_Attachment --
   --------------------------

   procedure Send_MIME_Attachment
     (Sock : in Sockets.Socket_FD; File : in Attachment)
   is
      procedure Send_File;
      --  Send File attachment

      procedure Send_Base64;
      --  Send Base64 attachment content

      Filename      : constant String := To_String (File.Name);
      Base_Filename : constant String
        := GNAT.Directory_Operations.File_Name (Filename);

      -----------------
      -- Send_Base64 --
      -----------------

      procedure Send_Base64 is
         Chunk_Size  : constant := 60;
         Content_Len : constant Positive := Length (File.Data);
         K           : Positive := 1;
      begin
         while K <= Content_Len loop
            if K + Chunk_Size - 1 > Content_Len then
               Sockets.Put_Line (Sock, Slice (File.Data, K, Content_Len));
               K := Content_Len + 1;
            else
               Sockets.Put_Line
                 (Sock, Slice (File.Data, K, K + Chunk_Size - 1));
               K := K + Chunk_Size;
            end if;
         end loop;

         Sockets.New_Line (Sock);
      end Send_Base64;

      ---------------
      -- Send_File --
      ---------------

      procedure Send_File is
         use Streams;

         Buffer_Size : constant := 60;
         --  Note that this size must be a multiple of 3, this is important to
         --  have proper chunk MIME encoding.

         File   : Stream_IO.File_Type;
         Buffer : Stream_Element_Array (1 .. Buffer_Size);
         Last   : Stream_Element_Offset;
      begin
         Stream_IO.Open (File, Stream_IO.In_File, Filename);

         while not Stream_IO.End_Of_File (File) loop
            Stream_IO.Read (File, Buffer, Last);

            Sockets.Put_Line
              (Sock, AWS.Translator.Base64_Encode (Buffer (1 .. Last)));
         end loop;

         Sockets.New_Line (Sock);

         Stream_IO.Close (File);
      end Send_File;

   begin
      --  MIME attachment headers

      Sockets.Put_Line (Sock, "Content-Type: application/octet-stream;");
      Sockets.Put_Line (Sock, "    name =""" & Base_Filename & '"');
      Sockets.Put_Line (Sock, "Content-Transfer-Encoding: base64");
      Sockets.Put_Line (Sock, "Content-Disposition: attachment;");
      Sockets.Put_Line (Sock, "    filename=""" & Base_Filename & '"');
      Sockets.New_Line (Sock);

      --  MIME content

      case File.Kind is
         when Client.File        => Send_File;
         when Base64_Data => Send_Base64;
      end case;
   end Send_MIME_Attachment;

   -----------------------
   -- Send_MIME_Message --
   -----------------------

   procedure Send_MIME_Message
     (Sock : in Sockets.Socket_FD; Message : in String) is
   begin
      --  MIME message headers

      Sockets.Put_Line (Sock, "Content-Type: text/plain");
      Sockets.New_Line (Sock);

      Put_Translated_Line (Sock, Message);
   end Send_MIME_Message;

   -------------------------
   -- Terminate_Mail_Data --
   -------------------------

   procedure Terminate_Mail_Data (Sock : in out Sockets.Socket_FD) is
   begin
      Sockets.New_Line (Sock);
      Sockets.Put (Sock, ".");
      Sockets.New_Line (Sock);
   end Terminate_Mail_Data;

end AWS.SMTP.Client;
