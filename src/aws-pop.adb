------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                       P O P - Post Office Protocol                       --
--                                                                          --
--                            Copyright (C) 2003                            --
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

with Ada.Exceptions;
with AWS.Headers.Set;
with Ada.Streams.Stream_IO;
with Ada.Strings.Fixed;
with Ada.Unchecked_Deallocation;

with AWS.Headers.Values;
with AWS.Messages;
with AWS.Net.Buffered;
with AWS.Resources.Streams.Memory;
with AWS.Translator;
with AWS.Utils;

package body AWS.POP is

   use Ada.Exceptions;

   --  MIME Headers

   subtype Stream_Type is AWS.Resources.Streams.Memory.Stream_Type;

   CRLF : constant String := ASCII.CR & ASCII.LF;

   Content_Transfer_Encoding : constant String := "Content-Transfer-Encoding";
   Content_Disposition       : constant String := "Content-Disposition";

   procedure Check_Response (Response : in String);
   --  Checks server's response, raise POP_Error with server's message

   ------------
   -- Adjust --
   ------------

   procedure Adjust (Attachment : in out POP.Attachment) is
   begin
      Attachment.Ref_Count.all := Attachment.Ref_Count.all + 1;
   end Adjust;

   procedure Adjust (Message : in out POP.Message) is
   begin
      Message.Ref_Count.all := Message.Ref_Count.all + 1;
   end Adjust;

   ----------------------
   -- Attachment_Count --
   ----------------------

   function Attachment_Count (Message : in POP.Message) return Natural is
      Count : Natural := 0;
      Ptr   : Attachment_Access := Message.Attachments;
   begin
      while Ptr /= null loop
         Count := Count + 1;
         Ptr := Ptr.Next;
      end loop;

      return Count;
   end Attachment_Count;

   --------------------
   -- Check_Response --
   --------------------

   procedure Check_Response (Response : in String) is
   begin
      if Response'Length > 3
        and then Response (Response'First .. Response'First + 3) = "-ERR"
      then
         Raise_Exception
           (POP_Error'Identity,
            Response (Response'First + 5 .. Response'Last));
      end if;
   end Check_Response;

   -----------
   -- Close --
   -----------

   procedure Close (Mailbox : in POP.Mailbox) is
   begin
      --  Send command

      Net.Buffered.Put_Line (Mailbox.Sock, "QUIT");

      declare
         Response : constant String
           := Net.Buffered.Get_Line (Mailbox.Sock);
      begin
         Check_Response (Response);
      end;

      Net.Std.Shutdown (Mailbox.Sock);

   exception
      when POP_Error =>
         Net.Std.Shutdown (Mailbox.Sock);
         raise;
   end Close;

   -------------
   -- Content --
   -------------

   function Content (Message : in POP.Message) return Unbounded_String is
   begin
      return Message.Content;
   end Content;

   function Content
     (Attachment : in POP.Attachment)
      return AWS.Resources.Streams.Stream_Access is
   begin
      return Attachment.Content;
   end Content;

   function Content (Attachment : in POP.Attachment) return Unbounded_String is
      use AWS.Resources.Streams;

      Stream : Stream_Type renames Stream_Type (Attachment.Content.all);

      Result : Unbounded_String;
      Buffer : Streams.Stream_Element_Array (1 .. 4_096);
      Last   : Streams.Stream_Element_Offset;

   begin
      if Is_File (Attachment) then
         Raise_Exception
           (Constraint_Error'Identity,
            "This is a file attachment, can't return unbounded_string");
      end if;

      Memory.Reset (Stream);

      while not Memory.End_Of_File (Stream) loop
         Memory.Read (Stream, Buffer, Last);
         Append (Result, Translator.To_Unbounded_String (Buffer (1 .. Last)));
      end loop;

      return Result;
   end Content;

   ----------
   -- Date --
   ----------

   function Date (Message : in POP.Message) return String is
   begin
      return Header (Message, "Date");
   end Date;

   ------------
   -- Delete --
   ------------

   procedure Delete
     (Mailbox : in POP.Mailbox;
      N       : in Positive) is
   begin
      Net.Buffered.Put_Line (Mailbox.Sock, "DELE " & Utils.Image (N));

      declare
         Response : constant String
           := Net.Buffered.Get_Line (Mailbox.Sock);
      begin
         Check_Response (Response);
      end;
   end Delete;

   --------------
   -- Filename --
   --------------

   function Filename (Attachment : in POP.Attachment) return String is
   begin
      return To_String (Attachment.Filename);
   end Filename;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Attachment : in out POP.Attachment) is
      procedure Free is new Unchecked_Deallocation
        (AWS.Resources.Streams.Stream_Type'Class,
         AWS.Resources.Streams.Stream_Access);
   begin
      Attachment.Ref_Count.all := Attachment.Ref_Count.all + 1;

      if Attachment.Ref_Count.all = 0 then
         Headers.Set.Free (Attachment.Headers);
         AWS.Resources.Streams.Memory.Close
           (Stream_Type (Attachment.Content.all));
         Free (Attachment.Content);
      end if;
   end Finalize;

   procedure Finalize (Message : in out POP.Message) is
      A : Attachment_Access := Message.Attachments;
   begin
      Message.Ref_Count.all := Message.Ref_Count.all + 1;

      if Message.Ref_Count.all = 0 then
         Headers.Set.Free (Message.Headers);
      end if;

      while A /= null loop
         Finalize (A.all);
         A := A.Next;
      end loop;
   end Finalize;

   --------------------------
   -- For_Every_Attachment --
   --------------------------

   procedure For_Every_Attachment (Message : in POP.Message) is
      P     : Attachment_Access := Message.Attachments;
      Index : Positive := 1;
      Quit  : Boolean := False;
   begin
      while P /= null loop
         Action (P.all, Index, Quit);
         exit when Quit;
         P := P.Next;
         Index := Index + 1;
      end loop;
   end For_Every_Attachment;

   -----------------------
   -- For_Every_Message --
   -----------------------

   procedure For_Every_Message
     (Mailbox : in POP.Mailbox;
      Remove  : in Boolean     := False)
   is
      Mess : Message;
      Quit : Boolean := False;
   begin
      for K in 1 .. Mailbox.Message_Count loop
         Mess := Get (Mailbox, K, Remove);
         Action (Mess, K, Quit);

         exit when Quit;
      end loop;
   end For_Every_Message;

   ------------------------------
   -- For_Every_Message_Header --
   ------------------------------

   procedure For_Every_Message_Header (Mailbox : in POP.Mailbox) is
      Mess : Message;
      Quit : Boolean := False;
   begin
      for K in 1 .. Mailbox.Message_Count loop
         Mess := Get_Header (Mailbox, K);
         Action (Mess, K, Quit);

         exit when Quit;
      end loop;
   end For_Every_Message_Header;

   ----------
   -- From --
   ----------

   function From (Message : in POP.Message) return String is
   begin
      return Header (Message, "From");
   end From;

   ---------
   -- Get --
   ---------

   function Get
     (Mailbox : in POP.Mailbox;
      N       : in Positive;
      Remove  : in Boolean     := False)
      return Message
   is

      procedure Get
        (Mailbox    : in     POP.Mailbox;
         Boundary   : in     String;
         Attachment :    out POP.Attachment;
         Last       :    out Boolean);

      ---------
      -- Get --
      ---------

      procedure Get
        (Mailbox    : in     POP.Mailbox;
         Boundary   : in     String;
         Attachment :    out POP.Attachment;
         Last       :    out Boolean)
      is
         End_Boundary : constant String := Boundary & "--";
         Base64       : Boolean := False;
      begin
         Attachment.Content := new Stream_Type;

         --  Read headers

         AWS.Headers.Set.Read (Mailbox.Sock, Attachment.Headers);

         --  Check Base64 encoding

         Base64 := Headers.Get
           (Attachment.Headers,
            Content_Transfer_Encoding) = "base64";

         --  Check for filename

         declare
            Filename : constant String
              := Headers.Values.Search
                   (Headers.Get (Attachment.Headers, Content_Disposition),
                   "filename");
         begin
            if Filename /= "" then
               Attachment.Filename := To_Unbounded_String (Filename);
            end if;
         end;

         --  Read content

         loop
            declare
               Response : constant String
                 := Net.Buffered.Get_Line (Mailbox.Sock);
            begin
               Last := Response = End_Boundary;

               exit when Response = Boundary or else Last;

               if Base64 then
                  AWS.Resources.Streams.Memory.Append
                    (Stream_Type (Attachment.Content.all),
                     Translator.Base64_Decode (Response));
               else
                  AWS.Resources.Streams.Memory.Append
                    (Stream_Type (Attachment.Content.all),
                     Translator.To_Stream_Element_Array (Response & CRLF));
               end if;
            end;
         end loop;
      end Get;

      Mess     : Message;
      Boundary : Unbounded_String;

   begin
      --  Send command

      Net.Buffered.Put_Line (Mailbox.Sock, "RETR " & Utils.Image (N));

      declare
         Response : constant String
           := Net.Buffered.Get_Line (Mailbox.Sock);
      begin
         Check_Response (Response);
      end;

      --  Read headers

      AWS.Headers.Set.Read (Mailbox.Sock, Mess.Headers);

      --  Check for MIME message

      declare
         Boundary_Field : constant String
           := Headers.Values.Search
                (Headers.Get (Mess.Headers, Messages.Content_Type_Token),
                 "boundary");
      begin
         if Boundary_Field /= "" then
            Boundary := To_Unbounded_String ("--" & Boundary_Field);
         end if;
      end;

      if Boundary = Null_Unbounded_String then
         --  Read content

         loop
            declare
               Response : constant String
                 := Net.Buffered.Get_Line (Mailbox.Sock);
            begin
               exit when Response = ".";
               Append (Mess.Content, Response & CRLF);
            end;
         end loop;

      else
         --  Skip first boundary

         loop
            declare
               Response : constant String
                 := Net.Buffered.Get_Line (Mailbox.Sock);
            begin
               exit when Response = To_String (Boundary);
               Append (Mess.Content, Response & CRLF);
            end;
         end loop;

         --  Read all attachments

         loop
            declare
               A    : Attachment;
               Last : Boolean;
            begin
               Get (Mailbox, To_String (Boundary), A, Last);

               if Mess.Last = null then
                  Mess.Attachments := new Attachment'(A);
                  Mess.Last        := Mess.Attachments;
               else
                  Mess.Last.Next   := new Attachment'(A);
                  Mess.Last        := Mess.Last.Next;
               end if;

               exit when Last;
            end;
         end loop;

         --  Now read until the end of the message body

         loop
            declare
               Response : constant String
                 := Net.Buffered.Get_Line (Mailbox.Sock);
            begin
               exit when Response = ".";
            end;
         end loop;
      end if;

      --  Remove message from server

      if Remove then
         Delete (Mailbox, N);
      end if;

      return Mess;
   end Get;

   function Get
     (Message    : in POP.Message'Class;
      Attachment : in Positive)
      return Attachment
   is
      P : Attachment_Access := Message.Attachments;
   begin
      for K in 2 .. Attachment loop

         if P = null then
            Raise_Exception
              (Constraint_Error'Identity, "No such attachment");
         end if;

         P := P.Next;
      end loop;

      return P.all;
   end Get;

   ----------------
   -- Get_Header --
   ----------------

   function Get_Header
     (Mailbox : in POP.Mailbox;
      N       : in Positive)
      return Message
   is
      Mess : Message;
   begin
      --  Send command to get the message size

      Net.Buffered.Put_Line (Mailbox.Sock, "LIST " & Utils.Image (N));

      declare
         Response : constant String
           := Net.Buffered.Get_Line (Mailbox.Sock);
         K : Natural;
      begin
         Check_Response (Response);
         K := Strings.Fixed.Index (Response, " ", Strings.Backward);
         Mess.Size := Natural'Value (Response (K + 1 .. Response'Last));
      end;

      --  Send command to get the message header

      Net.Buffered.Put_Line (Mailbox.Sock, "TOP " & Utils.Image (N) & " 0");
      --  Read 0 line from the body

      declare
         Response : constant String
           := Net.Buffered.Get_Line (Mailbox.Sock);
      begin
         Check_Response (Response);
      end;

      --  Read headers

      AWS.Headers.Set.Read (Mailbox.Sock, Mess.Headers);

      --  Now read until the end of the message body, should read a single
      --  line with a dot.

      loop
         declare
            Response : constant String
              := Net.Buffered.Get_Line (Mailbox.Sock);
         begin
            exit when Response = ".";
         end;
      end loop;

      return Mess;
   end Get_Header;

   ------------
   -- Header --
   ------------

   function Header
     (Message : in POP.Message;
      Header  : in String)
      return String is
   begin
      return Headers.Get (Message.Headers, Header);
   end Header;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Message : in out POP.Message) is
   begin
      Message.Ref_Count := new Natural'(0);
   end Initialize;

   procedure Initialize (Attachment : in out POP.Attachment) is
   begin
      Attachment.Ref_Count := new Natural'(0);
   end Initialize;

   function Initialize
     (Server_Name  : in String;
      User         : in String;
      Password     : in String;
      Authenticate : in Authenticate_Mode := Clear_Text;
      Port         : in Positive          := Default_POP_Port)
      return Mailbox
   is
      Timestamp  : Unbounded_String;
      Mailbox    : POP.Mailbox;
   begin
      Mailbox.Name := To_Unbounded_String (Server_Name);
      Mailbox.Sock := Net.Std.Socket_Type (Net.Socket (False).all);

      --  Connect to the server

      Net.Std.Connect (Mailbox.Sock, Server_Name, Port);

      declare
         Response : constant String
           := Net.Buffered.Get_Line (Mailbox.Sock);
      begin
         Check_Response (Response);

         --  Everything ok, let's retreive the timestamp if present

         if Authenticate = APOP then
            declare
               First, Last : Natural;
            begin
               First := Strings.Fixed.Index (Response, "<", Strings.Backward);
               Last  := Strings.Fixed.Index (Response, ">", Strings.Backward);

               if First /= 0 and then Last /= 0 then
                  Timestamp := To_Unbounded_String (Response (First .. Last));
               else
                  Raise_Exception
                    (POP_Error'Identity,
                     "APOP authentication not supported by server.");
               end if;
            end;
         end if;
      end;

      --  Authenticate

      if Authenticate = Clear_Text then

         Net.Buffered.Put_Line (Mailbox.Sock, "USER " & User);

         declare
            Response : constant String
              := Net.Buffered.Get_Line (Mailbox.Sock);
         begin
            Check_Response (Response);
         end;

         Net.Buffered.Put_Line (Mailbox.Sock, "PASS " & Password);

         declare
            Response : constant String
              := Net.Buffered.Get_Line (Mailbox.Sock);
         begin
            Check_Response (Response);
         end;

      else
         Net.Buffered.Put_Line
           (Mailbox.Sock, "APOP " & User
              & AWS.Utils.Get_MD5 (To_String (Timestamp) & Password));

         declare
            Response : constant String
              := Net.Buffered.Get_Line (Mailbox.Sock);
         begin
            Check_Response (Response);
         end;
      end if;

      --  Checks for mailbox's content

      Net.Buffered.Put_Line (Mailbox.Sock, "STAT");

      declare
         Response : constant String
           := Net.Buffered.Get_Line (Mailbox.Sock);
      begin
         Check_Response (Response);

         declare
            K : Natural;
         begin
            K := Strings.Fixed.Index (Response, " ", Strings.Backward);

            Mailbox.Message_Count
              := Natural'Value (Response (Response'First + 4 .. K - 1));
            Mailbox.Size
              := Natural'Value (Response (K + 1 .. Response'Last));
         end;
      end;

      return Mailbox;

   exception
      when POP_Error =>
         Net.Std.Shutdown (Mailbox.Sock);
         raise;
   end Initialize;

   -------------
   -- Is_File --
   -------------

   function Is_File (Attachment : in POP.Attachment) return Boolean is
   begin
      return Attachment.Filename /= Null_Unbounded_String;
   end Is_File;

   -------------------
   -- Message_Count --
   -------------------

   function Message_Count (Mailbox : in POP.Mailbox) return Natural is
   begin
      return Mailbox.Message_Count;
   end Message_Count;

   ----------
   -- Size --
   ----------

   function Size (Mailbox : in POP.Mailbox) return Natural is
   begin
      return Mailbox.Size;
   end Size;

   function Size (Message : in POP.Message) return Natural is
   begin
      return Message.Size;
   end Size;

   -------------
   -- Subject --
   -------------

   function Subject (Message : in POP.Message) return String is
   begin
      return Header (Message, "Subject");
   end Subject;

   ---------------
   -- User_Name --
   ---------------

   function User_Name (Mailbox : in POP.Mailbox) return String is
   begin
      return To_String (Mailbox.User_Name);
   end User_Name;

   -----------
   -- Write --
   -----------

   procedure Write (Attachment : in POP.Attachment; Directory : in String) is
      use Streams;
      use AWS.Resources.Streams;

      Stream : Stream_Type renames Stream_Type (Attachment.Content.all);

      File   : Stream_IO.File_Type;
      Buffer : Streams.Stream_Element_Array (1 .. 4_096);
      Last   : Streams.Stream_Element_Offset;

   begin
      if not Is_File (Attachment) then
         Raise_Exception
           (Constraint_Error'Identity,
            "This is not a file attachment, can't write content to a file.");
      end if;

      Stream_IO.Create
        (File, Stream_IO.Out_File,
         Directory & "/" & To_String (Attachment.Filename));

      Memory.Reset (Stream);

      while not Memory.End_Of_File (Stream) loop
         Memory.Read (Stream, Buffer, Last);
         Stream_IO.Write (File, Buffer (1 .. Last));
      end loop;

      Stream_IO.Close (File);
   end Write;

end AWS.POP;
