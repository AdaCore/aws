------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2003-2017, AdaCore                     --
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

with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;

with AWS.Config;
with AWS.Messages;
with AWS.MIME;
with AWS.Parameters;
with AWS.POP;
with AWS.Resources.Streams;
with AWS.Services.Transient_Pages;
with AWS.Session;
with AWS.SMTP.Client;
with AWS.Templates;
with AWS.Utils;

package body AWS.Services.Web_Mail is

   use Ada;
   use Ada.Strings.Unbounded;

   function Login (Request : Status.Data) return Response.Data;
   --  Get the login data from Request and returns the summary of the mailbox

   function Summary (Request : Status.Data) return Response.Data;
   --  Returns a list of all messages in the mailbox

   function Message (Request : Status.Data) return Response.Data;
   --  Returns a list of all messages in the mailbox

   function Delete (Request : Status.Data) return Response.Data;
   --  Delete a message, returns to the summary page

   function Reply
     (Request : Status.Data;
      To_All  : Boolean) return Response.Data;
   --  Returns a reply form

   function Send (Request : Status.Data) return Response.Data;
   --  Send a message, used by the reply form above

   -------------
   -- Context --
   -------------

   type Context_Data is record
      POP_Server       : Unbounded_String;
      POP_Host         : Unbounded_String;
      POP_Server_Port  : Positive;
      SMTP_Server      : Unbounded_String;
      SMTP_Host        : Unbounded_String;
      SMTP_Server_Port : Positive;
      User_Name        : Unbounded_String;
      Password         : Unbounded_String;
   end record;

   procedure Load_Context
     (SID     : Session.Id;
      Context : out Context_Data);
   --  Load server context as stored in the session SID

   function "+"
     (Str : String)
      return Unbounded_String
      renames To_Unbounded_String;

   function "-"
     (U_Str : Unbounded_String)
      return String
      renames To_String;

   --------------
   -- Callback --
   --------------

   function Callback (Request : Status.Data) return Response.Data is

      WWW_Root   : String renames AWS.Config.WWW_Root (Config.Get_Current);
      WM_Session : constant Session.Id := Status.Session (Request);
      URI        : constant String     := Status.URI (Request);

      procedure Check_Session;
      --  Checks current session data and set corresponding variables,
      --  POP_Server is set to Null_Unbounded_String if no session data found.

      POP_Server : Unbounded_String;

      -------------------
      -- Check_Session --
      -------------------

      procedure Check_Session is
      begin
         if Session.Exist (WM_Session, "WM_POP_SERVER") then
            POP_Server := To_Unbounded_String
              (String'(Session.Get (WM_Session, "WM_POP_SERVER")));
         end if;
      end Check_Session;

   begin
      Check_Session;

      if URI = "/wm_login" then
         return Login (Request);

      elsif URI = "/" or else POP_Server = Null_Unbounded_String then
         return Response.File (MIME.Text_HTML, WWW_Root & "/wm_login.html");

      elsif URI = "/wm_summary" then
         return Summary (Request);

      elsif URI = "/wm_message" then
         return Message (Request);

      elsif URI = "/wm_reply" then
         return Reply (Request, To_All => False);

      elsif URI = "/wm_reply_all" then
         return Reply (Request, To_All => True);

      elsif URI = "/wm_delete" then
         return Delete (Request);

      elsif URI = "/wm_send" then
         return Send (Request);

      else
         --  Check for a transient resource

         declare
            use type Resources.Streams.Stream_Access;
            use type Templates.Translate_Set;

            Stream : constant AWS.Resources.Streams.Stream_Access :=
                       Services.Transient_Pages.Get (URI);
         begin
            if Stream /= null then
               return Response.Stream
                 (Status.Content_Type (Request),
                  Stream,
                  Server_Close => False);

            else
               --  This is not a known resource

               if Utils.Is_Regular_File (WWW_Root & "404.thtml") then
                  --  Here we return the 404.thtml page if found. Note that
                  --  on Microsoft IE this page will be displayed only if
                  --  the total page size is bigger than 512 bytes or if it
                  --  includes at leat one image.

                  return Response.Acknowledge
                    (Messages.S404,
                     Templates.Parse
                       (WWW_Root & "404.thtml",
                        +Templates.Assoc ("PAGE", URI)));

               else
                  return Response.Acknowledge
                    (Messages.S404,
                     "<p>Page '" & URI & "' Not found.");
               end if;
            end if;
         end;
      end if;
   end Callback;

   ------------
   -- Delete --
   ------------

   function Delete (Request : Status.Data) return Response.Data is
      WM_Session : constant Session.Id := Status.Session (Request);
      P_List     : constant Parameters.List := Status.Parameters (Request);

      No_Message : constant Positive :=
                     Positive'Value (Parameters.Get (P_List, "NO_MESSAGE"));

      Context : Context_Data;
      Mailbox : POP.Mailbox;
   begin
      Load_Context (WM_Session, Context);

      Mailbox := POP.Initialize
        (-Context.POP_Host, -Context.User_Name, -Context.Password,
         Port => Context.POP_Server_Port);

      POP.Delete (Mailbox, No_Message);

      POP.Close (Mailbox);

      return Response.URL ("/wm_summary");
   end Delete;

   ------------------
   -- Load_Context --
   ------------------

   procedure Load_Context
     (SID     : Session.Id;
      Context : out Context_Data)
   is
      POP_Server  : constant String  := Session.Get (SID, "WM_POP_SERVER");
      POP_K       : constant Natural := Strings.Fixed.Index (POP_Server, ":");
      SMTP_Server : constant String  := Session.Get (SID, "WM_SMTP_SERVER");
      SMTP_K      : constant Natural := Strings.Fixed.Index (SMTP_Server, ":");
   begin
      Context.User_Name   := +Session.Get (SID, "WM_USER_NAME");
      Context.Password    := +Session.Get (SID, "WM_PASSWORD");
      Context.POP_Server  := +POP_Server;
      Context.SMTP_Server := +SMTP_Server;

      --  POP

      if POP_K = 0 then
         --  No port specifier for the POP server
         Context.POP_Host        := +POP_Server;
         Context.POP_Server_Port := POP.Default_POP_Port;
      else
         Context.POP_Host       :=
           +POP_Server (POP_Server'First .. POP_K - 1);
         Context.POP_Server_Port :=
           Positive'Value (POP_Server (POP_K + 1 .. POP_Server'Last));
      end if;

      --  SMTP

      if SMTP_K = 0 then
         --  No port specifier for the POP server
         Context.SMTP_Host        := +SMTP_Server;
         Context.SMTP_Server_Port := SMTP.Default_SMTP_Port;
      else
         Context.SMTP_Host        :=
           +SMTP_Server (SMTP_Server'First .. SMTP_K - 1);
         Context.SMTP_Server_Port :=
           Positive'Value (SMTP_Server (SMTP_K + 1 .. SMTP_Server'Last));
      end if;
   end Load_Context;

   -----------
   -- Login --
   -----------

   function Login (Request : Status.Data) return Response.Data is
      WM_Session : constant Session.Id      := Status.Session (Request);
      P_List     : constant Parameters.List := Status.Parameters (Request);
   begin
      Session.Set (WM_Session, "WM_SMTP_SERVER",
                   Parameters.Get (P_List, "WM_SMTP_SERVER"));
      Session.Set (WM_Session, "WM_POP_SERVER",
                   Parameters.Get (P_List, "WM_POP_SERVER"));
      Session.Set (WM_Session, "WM_USER_NAME",
                   Parameters.Get (P_List, "WM_USER_NAME"));
      Session.Set (WM_Session, "WM_PASSWORD",
                   Parameters.Get (P_List, "WM_PASSWORD"));
      return Response.URL ("/wm_summary");
   end Login;

   -------------
   -- Message --
   -------------

   function Message (Request : Status.Data) return Response.Data is
      WWW_Root   : String renames Config.WWW_Root (Config.Get_Current);
      WM_Session : constant Session.Id := Status.Session (Request);
      P_List     : constant Parameters.List := Status.Parameters (Request);

      No_Message : constant Positive
        := Positive'Value (Parameters.Get (P_List, "NO_MESSAGE"));

      Context : Context_Data;
      Mailbox : POP.Mailbox;
      Mess    : POP.Message;

      function Get_Content return Templates.Translate_Set;
      --  Returns content and attachments

      -----------------
      -- Get_Content --
      -----------------

      function Get_Content return Templates.Translate_Set is

         Content  : Unbounded_String;
         Att_Name : Templates.Vector_Tag;
         Att_Ref  : Templates.Vector_Tag;

         procedure Add_Attachment
           (Attachment : POP.Attachment;
            Index      : Positive;
            Quit       : in out Boolean);
         --  Add new attachment data

         --------------------
         -- Add_Attachment --
         --------------------

         procedure Add_Attachment
           (Attachment : POP.Attachment;
            Index      : Positive;
            Quit       : in out Boolean)
         is
            pragma Unreferenced (Index, Quit);

            use type Templates.Vector_Tag;
         begin
            if POP.Is_File (Attachment) then
               declare
                  Filename : constant String := POP.Filename (Attachment);
                  URI      : constant String
                    := Services.Transient_Pages.Get_URI & "/" & Filename;
                  --  We add the filename after the uniq URI to tell the
                  --  browser to use this name to save the file on disk.
               begin
                  --  Add a reference to the attachment into the message body

                  Append (Content, ASCII.CR & ASCII.LF);
                  Append (Content, "<" & Filename & ">");

                  --  Add attachment data

                  Att_Name := Att_Name & Filename;
                  Att_Ref  := Att_Ref & URI;

                  Services.Transient_Pages.Register
                    (URI,
                     POP.Content (Attachment),
                     Lifetime => 120.0);
               end;

            else
               Append (Content, ASCII.CR & ASCII.LF);
               Append (Content, Unbounded_String'(POP.Content (Attachment)));
            end if;
         end Add_Attachment;

         ---------------------------
         -- Add_Every_Attachments --
         ---------------------------

         procedure Add_Every_Attachment is
            new POP.For_Every_Attachment (Add_Attachment);

         T : Templates.Translate_Set;

      begin
         --  Add the message content

         Content := POP.Content (Mess);

         --  Analyse all attachments

         Add_Every_Attachment (Mess);

         --  Returns the corresponding translate set

         Templates.Insert (T, Templates.Assoc ("WM_CONTENT", Content));
         Templates.Insert (T, Templates.Assoc ("WM_ATT_NAME_V", Att_Name));
         Templates.Insert (T, Templates.Assoc ("WM_ATT_REF_V", Att_Ref));

         return T;
      end Get_Content;

      use Templates;
      T : Translate_Set;

   begin
      Load_Context (WM_Session, Context);

      Mailbox := POP.Initialize
        (-Context.POP_Server,
         -Context.User_Name, -Context.Password,
         Port => Context.POP_Server_Port);

      Mess := POP.Get (Mailbox, No_Message);

      POP.Close (Mailbox);

      Insert (T, Assoc ("AWS_VERSION", AWS.Version));
      Insert (T, Assoc ("WM_USER_NAME", Context.User_Name));
      Insert (T, Assoc ("WM_POP_SERVER", Context.POP_Server));
      Insert (T, Assoc ("WM_MESS_COUNT", POP.Message_Count (Mailbox)));
      Insert (T, Assoc ("WM_MESSAGE", No_Message));
      Insert (T, Assoc ("WM_SUBJECT", POP.Subject (Mess)));
      Insert (T, Assoc ("WM_DATE", POP.Date (Mess)));
      Insert (T, Assoc ("WM_FROM", POP.From (Mess)));
      Insert (T, Assoc ("WM_CC", POP.CC (Mess)));
      Insert (T, Get_Content);

      return Response.Build
        (MIME.Text_HTML,
         Unbounded_String'
           (Templates.Parse
              (WWW_Root & "/wm_message.thtml", T)));
   end Message;

   -----------
   -- Reply --
   -----------

   function Reply
     (Request : Status.Data;
      To_All  : Boolean) return Response.Data
   is
      WWW_Root   : String renames Config.WWW_Root (Config.Get_Current);
      WM_Session : constant Session.Id := Status.Session (Request);
      P_List     : constant Parameters.List := Status.Parameters (Request);

      No_Message : constant Positive
        := Positive'Value (Parameters.Get (P_List, "NO_MESSAGE"));

      Context : Context_Data;
      Mailbox : POP.Mailbox;
      Mess    : POP.Message;

      function Get_Content return Templates.Association;
      --  Returns content, all lines being prefixed

      -----------------
      -- Get_Content --
      -----------------

      function Get_Content return Templates.Association is
         Prefix  : constant String := "> ";
         Content : Unbounded_String;
         K       : Positive := Prefix'Length + 1;
      begin
         --  Get message content

         Content := POP.Content (Mess);

         --  Prefix first line with "> "

         Content := Prefix & Content;

         --  Prefix every lines with "> "

         while K < Length (Content) loop
            if Element (Content, K) = ASCII.LF then
               Insert (Content, K + 1, Prefix);
            end if;
            K := K + 1;
         end loop;

         return Templates.Assoc ("WM_CONTENT", Content);
      end Get_Content;

      use Templates;
      T : Translate_Set;

   begin
      Load_Context (WM_Session, Context);

      Mailbox := POP.Initialize
        (-Context.POP_Host, -Context.User_Name, -Context.Password,
         Port => Context.POP_Server_Port);

      Mess := POP.Get (Mailbox, No_Message);

      POP.Close (Mailbox);

      Insert (T, Assoc ("AWS_VERSION", AWS.Version));
      Insert (T, Assoc ("WM_USER_NAME", -Context.User_Name));
      Insert (T, Assoc ("WM_SMTP_SERVER", -Context.SMTP_Server));
      Insert (T, Assoc ("WM_POP_SERVER", -Context.POP_Server));
      Insert (T, Assoc ("WM_MESS_COUNT", POP.Message_Count (Mailbox)));
      Insert (T, Assoc ("WM_MESSAGE", No_Message));
      Insert (T, Assoc ("WM_SUBJECT", POP.Subject (Mess)));
      Insert (T, Assoc ("WM_DATE", POP.Date (Mess)));
      Insert (T, Assoc ("WM_FROM", POP.From (Mess)));
      Insert (T, Assoc ("WM_CC", POP.CC (Mess)));
      Insert (T, Assoc ("WM_TO_ALL", To_All));
      Insert (T, Get_Content);

      return Response.Build
        (MIME.Text_HTML,
         Unbounded_String'
           (Templates.Parse (WWW_Root & "/wm_reply.thtml", T)));
   end Reply;

   ----------
   -- Send --
   ----------

   function Send (Request : Status.Data) return Response.Data is

      WWW_Root   : String renames Config.WWW_Root (Config.Get_Current);
      WM_Session : constant Session.Id := Status.Session (Request);
      P_List     : constant Parameters.List := Status.Parameters (Request);

      function Get_From return SMTP.E_Mail_Data;
      --  Build the From e-mail

      Context    : Context_Data;

      --------------
      -- Get_From --
      --------------

      function Get_From return SMTP.E_Mail_Data is
         POP_Host : constant String  := -Context.POP_Host;
         K        : constant Natural := Strings.Fixed.Index (POP_Host, ".");
         E_Mail   : Unbounded_String;
      begin
         if K = 0 then
            --  No domain specified, this is a local machine
            E_Mail := Context.User_Name & '@' & POP_Host;
         else
            --  Get the domain name after the first dot
            E_Mail := Context.User_Name & '@'
                        & POP_Host (K + 1 .. POP_Host'Last);
         end if;

         return SMTP.E_Mail (To_String (E_Mail), To_String (E_Mail));
      end Get_From;

      WM_SMTP : SMTP.Receiver;
      Result  : SMTP.Status;

      use type Templates.Translate_Set;

   begin
      Load_Context (WM_Session, Context);

      WM_SMTP := SMTP.Client.Initialize
        (-Context.SMTP_Host, Port => Context.SMTP_Server_Port);

      SMTP.Client.Send
        (WM_SMTP,
         From    => Get_From,
         To      => SMTP.Parse (Parameters.Get (P_List, "WM_TO")),
         Subject => Parameters.Get (P_List, "WM_SUBJECT"),
         Message => Parameters.Get (P_List, "WM_CONTENT"),
         Status  => Result);

      if SMTP.Is_Ok (Result) then
         return Response.URL ("/wm_summary");
      else
         return Response.Build
           (MIME.Text_HTML,
            String'
              (Templates.Parse
                 (WWW_Root & "/wm_error.thtml",
                  +Templates.Assoc
                     ("ERROR_MESSAGE", SMTP.Status_Message (Result)))));
      end if;

   exception
      when Constraint_Error =>
         return Response.Build
           (MIME.Text_HTML,
            String'
              (Templates.Parse
                 (WWW_Root & "/wm_error.thtml",
                  +Templates.Assoc
                     ("ERROR_MESSAGE", "Can't parse e-mail"))));
   end Send;

   -------------
   -- Summary --
   -------------

   function Summary (Request : Status.Data) return Response.Data is

      WWW_Root   : String renames Config.WWW_Root (Config.Get_Current);
      WM_Session : constant Session.Id := Status.Session (Request);

      procedure Add_Message
        (Message : POP.Message;
         Index   : Positive;
         Quit    : in out Boolean);
      --  Add message information into the vertor tags

      Index_V   : Templates.Vector_Tag;
      Size_V    : Templates.Vector_Tag;
      Date_V    : Templates.Vector_Tag;
      From_V    : Templates.Vector_Tag;
      Subject_V : Templates.Vector_Tag;

      -----------------
      -- Add_Message --
      -----------------

      procedure Add_Message
        (Message : POP.Message;
         Index   : Positive;
         Quit    : in out Boolean)
      is
         pragma Unreferenced (Quit);
         use type Templates.Vector_Tag;
      begin
         Index_V   := Utils.Image (Index)   & Index_V;
         Size_V    := POP.Size (Message)    & Size_V;
         Date_V    := POP.Date (Message)    & Date_V;
         From_V    := POP.From (Message)    & From_V;
         Subject_V := POP.Subject (Message) & Subject_V;
      end Add_Message;

      -------------------------
      -- Add_Message_Headers --
      -------------------------

      procedure Add_Message_Headers is
         new POP.For_Every_Message_Header (Add_Message);

      Context : Context_Data;
      Mailbox : POP.Mailbox;

      use Templates;

      T : Translate_Set;

   begin
      Load_Context (WM_Session, Context);

      Mailbox := POP.Initialize
        (-Context.POP_Host, -Context.User_Name, -Context.Password,
         Port => Context.POP_Server_Port);

      Add_Message_Headers (Mailbox);

      POP.Close (Mailbox);

      Insert (T, Assoc ("AWS_VERSION", AWS.Version));
      Insert (T, Assoc ("WM_MESS_COUNT", POP.Message_Count (Mailbox)));
      Insert (T, Assoc ("WM_MAILBOX_SIZE", POP.Size (Mailbox)));
      Insert (T, Assoc ("WM_MESSAGE_V", Index_V));
      Insert (T, Assoc ("WM_SIZE_V", Size_V));
      Insert (T, Assoc ("WM_FROM_V", From_V));
      Insert (T, Assoc ("WM_DATE_V", Date_V));
      Insert (T, Assoc ("WM_SUBJECT_V", Subject_V));
      Insert (T, Assoc ("WM_USER_NAME", Context.User_Name));
      Insert (T, Assoc ("WM_POP_SERVER", Context.POP_Server));

      return Response.Build
        (MIME.Text_HTML,
         Unbounded_String'
           (Templates.Parse (WWW_Root & "/wm_summary.thtml", T)));
   end Summary;

end AWS.Services.Web_Mail;
