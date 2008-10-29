------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                       Copyright (C) 2008, AdaCore                        --
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

with Ada.Characters.Handling;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Strings.Hash;
with Ada.Text_IO;
with Ada.Unchecked_Deallocation;

with Input_Sources.Strings;
with Sax.Attributes;
with Sax.Readers;
with Unicode.CES.Basic_8bit;

with AWS.Jabber.Digest_Md5;
with AWS.Net.Buffered;
with AWS.Translator;
with AWS.Utils;

package body AWS.Jabber.Client is

   use Ada;

   procedure XMPP_Send (Account : in Client.Account; Message : in String);
   --  Send a XMPP message to the jabber server

   -----------
   -- Close --
   -----------

   procedure Close (Account : in out Client.Account)  is
      procedure Free is new Ada.Unchecked_Deallocation
        (Incoming_Stream, Incoming_Stream_Access);
   begin
      if Account.Is_Running then
         --  Let's annouce that we are going offline

         XMPP_Send (Account,
                    "<presence type='unavailable' from='"
                    & To_String (Account.User.JID) & "'/>");

         --  Send closing stream element

         XMPP_Send (Account, "</stream:stream>");
         Net.Shutdown (Account.Sock.all);

         --  Terminate task Incoming_Stream

         while not Account.Stream'Terminated loop
            delay 1.0;
         end loop;

         Net.Free (Account.Sock);

         Free (Account.Stream);
         Account.Is_Running := False;
      end if;
   end Close;

   -------------
   -- Connect --
   -------------

   procedure Connect (Account : in out Client.Account) is
   begin
      --  Open socket to Jabber Server

      Account.Sock := Net.Socket (Security => False);

      Connection : begin
         Net.Connect
           (Account.Sock.all,
            To_String (Account.Host), Positive (Account.Port));
      exception
         when Net.Socket_Error =>
            raise Server_Error with "Can't connect to "
              & To_String (Account.Host)
              & ':' & Utils.Image (Positive (Account.Port));
      end Connection;

      --  Start Incoming_Stream reader

      Account.Stream := new Incoming_Stream (Account.Self);

      Account.Is_Running := True;

      --  Initialize the Jabber protocol

      XMPP_Send
        (Account,
         "<?xml version='1.0' encoding='UTF-8' ?>"
         & "<stream:stream to=" & Utils.Quote (To_String (Account.Host))
         & " xmlns='jabber:client'"
         & " xmlns:stream='http://etherx.jabber.org/streams' version='1.0'>");

   exception
      when E : others =>
         Text_IO.Put_Line (Exceptions.Exception_Information (E));
         --  We must close the server properly before leaving this routine if
         --  an exception is raised.
         Close (Account);
         raise Server_Error;
   end Connect;

   -----------------
   -- IO_Message ---
   -----------------

   procedure IO_Message
     (From         : in Jabber_ID;
      Message_Type : in Client.Message_Type;
      Subject      : in String;
      Content      : in String) is
   begin
      Text_IO.Put_Line ("From :" & String (From));
      if Message_Type = M_Normal then
         Text_IO.Put_Line ("Subject: " & Subject);
      end if;

      Text_IO.Put_Line ("Body: " & Content);
   end IO_Message;

   -----------------
   -- IO_Presence --
   -----------------

   procedure IO_Presence
     (From   : in Jabber_ID;
      Status : in String) is
   begin
      Text_IO.Put_Line (String (From) & " is " & Status);
   end IO_Presence;

   ----------
   -- Send --
   ----------

   procedure Send
     (Account      : in Client.Account;
      JID          : in Jabber_ID;
      Content      : in String;
      Subject      : in String := "";
      Message_Type : in Client.Message_Type := M_Normal)
   is

      function Send_Type return String;
      --  Returns the message type

      ---------------
      -- Send_Type --
      ---------------

      function Send_Type return String is
         T : constant String := Client.Message_Type'Image (Message_Type);
      begin
         return Characters.Handling.To_Lower (T (T'First + 2 .. T'Last));
      end Send_Type;

   begin
      if Account.Is_Running then
         --  Send Message

         XMPP_Send (Account,
                    "<message xmlns='jabber:client' type='" & Send_Type & "'"
                    & " id='id_msg' to='" & String (JID) & "'>"
                    & " <thread xmlns='jabber:client'>ja_msg</thread>"
                    & " <subject xmlns='jabber:client'>" & Subject
                    & "</subject>"
                    & " <body xmlns='jabber:client'>" & Content & "</body>"
                    & "</message>");
      else
         raise Server_Error with "Not connected to server";
      end if;
   end Send;

   -----------------------------
   -- Set_Authentication_Type --
   -----------------------------

   procedure Set_Authentication_Type
     (Account   : in out Client.Account;
      Auth_Type : in     Authentication_Mechanism) is
   begin
      Account.Auth_Type := Auth_Type;
   end Set_Authentication_Type;

   --------------
   -- Set_Host --
   --------------

   procedure Set_Host
     (Account : in out Client.Account;
      Host    : in     String) is
   begin
      Account.Host := To_Unbounded_String (Host);
   end Set_Host;

   ---------------------------
   -- Set_Login_Information --
   ---------------------------

   procedure Set_Login_Information
     (Account  : in out Client.Account;
      User     : in     String;
      Password : in     String;
      Resource : in     String := "") is
   begin
      Account.User.Name     := To_Unbounded_String (User);
      Account.User.Password := To_Unbounded_String (Password);
      Account.User.Resource := To_Unbounded_String (Resource);
   end Set_Login_Information;

   --------------
   -- Set_Port --
   --------------

   procedure Set_Port
     (Account : in out Client.Account;
      Port    : in Client.Port) is
   begin
      Account.Port := Port;
   end Set_Port;

   -----------------------
   -- Set_Presence_Hook --
   -----------------------

   procedure Set_Presence_Hook
     (Account : in out Client.Account;
      Hook    : in     Presence_Hook) is
   begin
      Account.Hooks.Presence := Hook;
   end Set_Presence_Hook;

   ------------------
   -- To_Jabber_ID --
   ------------------

   function To_Jabber_ID
     (Username : in String;
      Server   : in String;
      Resource : in String := "") return Jabber_ID is
   begin
      if Resource /= "" then
         return Jabber_ID (Username & '@' & Server & '/' & Resource);
      else
         return Jabber_ID (Username & '@' & Server);
      end if;
   end To_Jabber_ID;

   ---------------------
   -- Incoming_Stream --
   ---------------------

   task body Incoming_Stream is

      type Connection_Step is
        (Initialize_Connection, Get_Mechanism, Authentication, Connected);

      type Authentication_Step is
        (First_Challenge, Second_Challenge, Challenge_Result,
         Bind_Requirement, Get_Resource, Get_Ack_Session);

      Connection_Current_Step     : Connection_Step := Initialize_Connection;
      Authentication_Current_Step : Authentication_Step := First_Challenge;

      procedure Get_Message (XML : in String; Start, Stop : in out Positive);
      --  Returns Start and Stop where XML (Start .. Stop) is the next XML
      --  chunk. Start and Stop are initialy set as bound for the previous
      --  slice. The first time this routine is called we have
      --  Start = Stop = XML'First. Returns Start = Stop if this tag must be
      --  skipped and Start > XML'Last when there is nothing more to read.

      procedure Parse_Message (XML : in String);
      --  Parse the XML message and call the appropriate hooks

      -----------------
      -- Get_Message --
      -----------------

      procedure Get_Message (XML : in String; Start, Stop : in out Positive) is
         K : Positive;
         I : Natural;
      begin
         if Start /= Stop or else Start /= XML'First then
            Start := Stop + 1;
         end if;

         if Start > XML'Last then
            return;
         end if;

         --  Look for start tag

         while Start <= XML'Last and then XML (Start) /= '<' loop
            Start := Start + 1;
         end loop;

         K := Start + 1;
         while K <= XML'Last and then XML (K) /= ' ' loop
            K := K + 1;
         end loop;
         K := K - 1;

         --  Look for the end of the current tag

         Stop := Start;
         while Stop <= XML'Last and then XML (Stop) /= '>' loop
            Stop := Stop + 1;
         end loop;

         if Start > XML'Last or else Stop > XML'Last then
            --  We have reached the end of the string
            --  Nothing more to read
            Start := XML'Last + 1;
            return;
         end if;

         --  Check for tag to be skipped

         if XML (Start .. K) = "<?xml" then
            Stop := Start;
            Start := Stop + 1;
            return;
         end if;

         if XML (Start .. K) = "<stream:stream" then
            --  There is no ending element, this tag will be closed when the
            --  server will close the connection.
            return;
         end if;

         --  Look now for the ending element

         I := Strings.Fixed.Index
           (XML (Start .. XML'Last), "</" & XML (Start + 1 .. K) & '>');

         if I = 0 then
            --  No ending element tag, look for "/>"

            I := Strings.Fixed.Index (XML (Start .. XML'Last), "/>");

            if I = 0 then
               Start := XML'Last + 1;
               Stop := Start;
            else
               Stop := I + 1;
            end if;

         else
            Stop := I + K - Start + 2;
         end if;
      end Get_Message;

      -------------------
      -- Parse_Message --
      -------------------

      procedure Parse_Message (XML : in String) is
         use Input_Sources.Strings;

         package XMPP_Parser is

            package Messages_Maps is new Ada.Containers.Indefinite_Hashed_Maps
              (String, String, Strings.Hash, "=", "=");

            type XMPP_Message is new Messages_Maps.Map with null record;
            --  A XMPP_Message, this is just a set of key/value pair. Each key
            --  represent a tag and the associated value is the tag's value.
            --  Tag's attributes are encoded with a key which is the tag
            --  element name catenated with a '.' and the attribute name. For
            --  example with :
            --
            --     <presence from="toto"/>
            --
            --  We have :     Key            Value
            --                -------------  ------
            --                presence       ""
            --                presence.from  "toto"

            type XMPP_Message_Access is access all XMPP_Message;

            procedure Release is
              new Ada.Unchecked_Deallocation
                (XMPP_Message, XMPP_Message_Access);
            --  Release all maemory associated with the response object_access

            type Tree_Reader is new Sax.Readers.Reader with record
               R     : XMPP_Message_Access;
               Key   : Unbounded_String;
               Value : Unbounded_String;
            end record;

            overriding procedure Start_Element
              (Handler       : in out Tree_Reader;
               Namespace_URI : in     Unicode.CES.Byte_Sequence := "";
               Local_Name    : in     Unicode.CES.Byte_Sequence := "";
               Qname         : in     Unicode.CES.Byte_Sequence := "";
               Atts          : in     Sax.Attributes.Attributes'Class);

            overriding procedure End_Element
              (Handler       : in out Tree_Reader;
               Namespace_URI : in     Unicode.CES.Byte_Sequence := "";
               Local_Name    : in     Unicode.CES.Byte_Sequence := "";
               Qname         : in     Unicode.CES.Byte_Sequence := "");

            overriding procedure Characters
              (Handler : in out Tree_Reader;
               Ch      : in     Unicode.CES.Byte_Sequence);

            overriding procedure Ignorable_Whitespace
              (Handler : in out Tree_Reader;
               Ch      : in     Unicode.CES.Byte_Sequence);

            procedure Process
              (Account  : in out Client.Account;
               Message  : in     XMPP_Message_Access);
         end XMPP_Parser;

         function Message_Suffix return String;
         --  Returns the closing stream tag to be able to parse the
         --  stream:stream element. This element will be closed when the
         --  Jabber session will be terminated. We just add this here to be
         --  able to parse this XML message.

         --------------------
         -- Message_Suffix --
         --------------------

         function Message_Suffix return String is
         begin
            if XML (XML'First .. XML'First + 13) = "<stream:stream" then
               return "</stream:stream>";
            elsif XML (XML'First .. XML'First + 15) = "<stream:features" then
               return "</stream:features>";
            else
               return "";
            end if;
         end Message_Suffix;

         -----------------
         -- XMPP_Parser --
         -----------------

         package body XMPP_Parser is

            ----------------
            -- Characters --
            ----------------

            overriding procedure Characters
              (Handler : in out Tree_Reader;
               Ch      : in     Unicode.CES.Byte_Sequence) is
            begin
               Append (Handler.Value, To_Unbounded_String (Ch));
            end Characters;

            -----------------
            -- End_Element --
            -----------------

            overriding procedure End_Element
              (Handler       : in out Tree_Reader;
               Namespace_URI : in     Unicode.CES.Byte_Sequence := "";
               Local_Name    : in     Unicode.CES.Byte_Sequence := "";
               Qname         : in     Unicode.CES.Byte_Sequence := "")
            is
               pragma Unreferenced (Namespace_URI);
               pragma Unreferenced (Local_Name);
               pragma Unreferenced (Qname);
               Cursor : Messages_Maps.Cursor;
               Found  : Boolean;
            begin
               if Handler.Key /= Null_Unbounded_String then
                  if not Contains
                    (Handler.R.all, To_String (Handler.Key))
                  then
                     Insert
                       (Handler.R.all, To_String (Handler.Key),
                        To_String (Handler.Value), Cursor, Found);
                  else
                     --  Set the key value to old_value:new_value
                     Replace (Handler.R.all, To_String (Handler.Key),
                              Element (Handler.R.all, To_String (Handler.Key))
                              & ':' & To_String (Handler.Value));
                  end if;
               end if;

               Handler.Key   := Null_Unbounded_String;
               Handler.Value := Null_Unbounded_String;
            end End_Element;

            --------------------------
            -- Ignorable_Whitespace --
            --------------------------

            overriding procedure Ignorable_Whitespace
              (Handler : in out Tree_Reader;
               Ch      : in     Unicode.CES.Byte_Sequence) is
            begin
               Append (Handler.Value, Ch);
            end Ignorable_Whitespace;

            -------------
            -- Process --
            -------------

            procedure Process
              (Account : in out Client.Account;
               Message : in     XMPP_Message_Access)
            is
               procedure Digest_MD5_Authenticate;

               function Value
                 (M   : in XMPP_Message_Access;
                  Key : in String) return String;
               --  Returns the value for Key in the message M
               --  or The empty string if the key is not found.

               procedure Get_Presence_Hook;
               --  Get the presence status and run the presence hook

               procedure Get_Message_Hook;
               --  Run the message hook

               -----------------------------
               -- Digest_MD5_Authenticate --
               -----------------------------

               procedure Digest_MD5_Authenticate is

                  procedure Next_Step;
                  --  Move Digest_MD5_Current_Step to next step

                  ---------------
                  -- Next_Step --
                  ---------------

                  procedure Next_Step is
                  begin
                     Authentication_Current_Step :=
                       Authentication_Step'Succ (Authentication_Current_Step);
                  end Next_Step;

               begin
                  if Authentication_Current_Step = First_Challenge and then
                    Contains (Message.all, "challenge")
                  then
                     Reply_Challenge : declare
                        Challenge  : constant Digest_Md5.Challenge :=
                                       Digest_Md5.Decode_Challenge
                                         (Value (Message, "challenge"));
                     begin
                        XMPP_Send
                          (Account,
                           "<response "
                           & "xmlns='urn:ietf:params:xml:ns:xmpp-sasl'>"
                           & Digest_Md5.Reply_Challenge
                             (Username => To_String (Account.User.Name),
                              Realm    => To_String (Challenge.Realm),
                              Password => To_String (Account.User.Password),
                              Host     => To_String (Account.Host),
                              Nonce    => To_String (Challenge.Nonce))
                           & "</response>");
                     end Reply_Challenge;

                     Next_Step;

                  elsif Authentication_Current_Step = Second_Challenge
                    and then Contains (Message.all, "challenge")
                  then
                     --  If the authentication succeed, the server will send a
                     --  final challenge that contain a single directive
                     --  "rspauth" base64 encoded. This directive is ignored.
                     --  Simply return an empty response

                     XMPP_Send
                       (Account,
                        "<response "
                        & "xmlns='urn:ietf:params:xml:ns:xmpp-sasl'/>");
                     Next_Step;

                  elsif Authentication_Current_Step = Challenge_Result
                    and then Contains (Message.all, "success")
                  then
                     --  At this point, the server inform the client
                     --  of successful authentication, with:
                     --  <success xmlns='urn:ietf:params:xml:ns:xmpp-sasl'/>

                     --  Start a new stream

                     XMPP_Send
                       (Account,
                        "<stream:stream "
                        &  "xmlns:stream='http://etherx.jabber.org/streams' "
                        & "xmlns='jabber:client' "
                        & "to='" & To_String (Account.Host) & "' "
                        & "version='1.0'>");
                     Next_Step;

                  elsif Authentication_Current_Step = Bind_Requirement
                    and then Contains (Message.all, "bind")
                  then
                     --  Server tells client that resource binding is required

                     --  Request a resource or ask for the desired resource

                     if Account.User.Resource /= "" then
                        XMPP_Send
                          (Account,
                           "<iq type='set' id='bind_2'>"
                           & "<bind xmlns='urn:ietf:params:xml:ns:xmpp-bind'>"
                           & "<resource>"
                           & To_String (Account.User.Resource)
                           & "</resource></bind></iq>");
                     else
                        XMPP_Send
                          (Account,
                           "<iq type='set' id='bind_1'>"
                           & "<bind xmlns='urn:ietf:params:xml:ns:xmpp-bind'/>"
                           & "</iq>");
                     end if;

                     Next_Step;

                  elsif Authentication_Current_Step = Get_Resource
                    and then Contains (Message.all, "jid")
                  then

                     Account.User.JID := To_Unbounded_String
                       (Value (Message, "jid"));

                     --  Server sent the generated (or requested) resource
                     --  The client must now request an IM session

                     XMPP_Send
                       (Account,
                        "<iq type='set' id='sess_1'>"
                        & "<session "
                        & "xmlns='urn:ietf:params:xml:ns:xmpp-session'/>"
                        & "</iq>");
                     Next_Step;

                  elsif Authentication_Current_Step = Get_Ack_Session
                    and then Contains (Message.all, "session")
                  then
                     --  Send our presence, as this is an application and not a
                     --  real user we send an initial dnd (Do Not Disturb)
                     --  status.

                     XMPP_Send (Account,
                           "<presence from='" & To_String (Account.User.JID)
                           & "' id='ja_pres'>"
                           & "<show>dnd</show>"
                           & "<status>AWS Project</status>"
                           & "</presence>");
                     Connection_Current_Step := Connected;

                  end if;
               end Digest_MD5_Authenticate;

               ----------------------
               -- Get_Message_Hook --
               ----------------------

               procedure Get_Message_Hook is
                  Type_Value : constant String :=
                                 Value (Message, "message.type");
                  Get_Type   : Message_Type := M_Normal;

               begin
                  if Type_Value = "chat" then
                     Get_Type := M_Chat;
                  elsif Type_Value = "normal" then
                     Get_Type := M_Normal;
                  elsif Type_Value = "groupchat" then
                     Get_Type := M_Group_Chat;
                  elsif Type_Value = "headline" then
                     Get_Type := M_Headline;
                  elsif Type_Value = "error" then
                     Get_Type := M_Error;
                  end if;

                  Account.Hooks.Message
                    (From         => Jabber_ID
                       (Value (Message, "message.from")),
                     Message_Type => Get_Type,
                     Subject      => Value (Message, "subject"),
                     Content      => Value (Message, "body"));
               end Get_Message_Hook;

               -----------------------
               -- Get_Presence_Hook --
               -----------------------

               procedure Get_Presence_Hook is

                  function Get_Status return String;
                  --  Returns the presence status

                  ----------------
                  -- Get_Status --
                  ----------------

                  function Get_Status return String is
                     Presence_Type : constant String :=
                                       Value (Message, "presence.type");
                  begin
                     if Presence_Type = "error" then
                        return Presence_Type;

                     else
                        if Message.Contains ("presence.show") then
                           return Value (Message, "presence.show");
                        else
                           --  Default is online
                           return "Online";
                        end if;
                     end if;
                  end Get_Status;

               begin
                  Account.Hooks.Presence
                    (From    => Jabber_ID (Value (Message, "presence.from")),
                     Status  => Get_Status);
               end Get_Presence_Hook;

               -----------
               -- Value --
               -----------

               function Value
                 (M   : in XMPP_Message_Access;
                  Key : in String) return String
               is
                  Cursor : Messages_Maps.Cursor;
               begin
                  Cursor := Find (M.all, Key);

                  if Messages_Maps.Has_Element (Cursor) then
                     return Element (M.all, Key);
                  else
                     return "";
                  end if;
               end Value;

            begin
               if Connection_Current_Step = Initialize_Connection then

                  --  Get Session Id from the stream element
                  Account.SID := To_Unbounded_String
                    (Value (Message, "stream.id"));
                  Connection_Current_Step :=
                    Connection_Step'Succ (Connection_Current_Step);

               elsif Connection_Current_Step = Get_Mechanism
                 and then Message.Contains ("mechanism")
               then
                  Check_Mecanism : declare
                     Supported_Mechanism : constant String :=
                                             Value (Message, "mechanism");
                  begin
                     if (Account.Auth_Type = More_Secure_Mechanism
                         or else Account.Auth_Type = Digest_Md5_Mechanism)
                       and then Strings.Fixed.Index
                         (Supported_Mechanism, "DIGEST-MD5") /= 0
                     then
                        XMPP_Send
                          (Account,
                           "<auth xmlns='urn:ietf:params:xml:ns:xmpp-sasl' "
                           & "mechanism='DIGEST-MD5'/>");
                     elsif (Account.Auth_Type = More_Secure_Mechanism
                            or else Account.Auth_Type = Plain_Mechanism)
                       and then Strings.Fixed.Index
                         (Supported_Mechanism, "PLAIN") /= 0
                     then
                        XMPP_Send
                          (Account,
                           "<auth xmlns='urn:ietf:params:xml:ns:xmpp-sasl' "
                           & "mechanism='PLAIN'>" &
                           AWS.Translator.Base64_Encode
                             (ASCII.NUL & To_String (Account.User.Name)
                              & ASCII.NUL & To_String (Account.User.Password))
                           & "</auth>");
                        --  Go directly to challenge result step
                        Authentication_Current_Step := Challenge_Result;
                     else
                        raise Server_Error
                          with "Mechanism is not supported by server";
                     end if;
                  end Check_Mecanism;

                  Connection_Current_Step :=
                    Connection_Step'Succ (Connection_Current_Step);

               elsif Connection_Current_Step = Authentication then
                  Digest_MD5_Authenticate;

               elsif Connection_Current_Step = Connected then
                  if Message.Contains ("presence.from") then
                     Get_Presence_Hook;
                  elsif Message.Contains ("message.from") then
                     Get_Message_Hook;
                  end if;
               end if;
            end Process;

            -------------------
            -- Start_Element --
            -------------------

            overriding procedure Start_Element
              (Handler       : in out Tree_Reader;
               Namespace_URI : in     Unicode.CES.Byte_Sequence := "";
               Local_Name    : in     Unicode.CES.Byte_Sequence := "";
               Qname         : in     Unicode.CES.Byte_Sequence := "";
               Atts          : in     Sax.Attributes.Attributes'Class)
            is
               pragma Unreferenced (Namespace_URI);
               pragma Unreferenced (Qname);

               use Sax.Attributes;

               Cursor : Messages_Maps.Cursor;
               Found  : Boolean;
            begin
               Handler.Key := To_Unbounded_String (Local_Name);

               --  Read all attributes, add a key/value pair for each atributes
               --  into the table with [Local_Name & '.'] added in from of the
               --  key (attribute name)

               for J in 0 .. Get_Length (Atts) - 1 loop
                  declare
                     Key : constant String :=
                             Local_Name & '.' & Get_Qname (Atts, J);
                  begin
                     if not Contains (Handler.R.all, Key) then
                        Insert
                          (Handler.R.all,
                           Key,
                           Get_Value (Atts, J),
                           Cursor, Found);
                     end if;
                  end;
               end loop;
            end Start_Element;

         end XMPP_Parser;

         XML_Message : aliased String := "<stream:stream xmlns='jabber:client'"
           & " xmlns:stream='http://etherx.jabber.org/streams' version='1.0'>"
           & XML & Message_Suffix & "</stream:stream>";

         Source      : String_Input;
         Reader      : XMPP_Parser.Tree_Reader;

      begin
         Reader.R := new XMPP_Parser.XMPP_Message;

         --  Parse the XML message

         Open (XML_Message'Unchecked_Access,
               Unicode.CES.Basic_8bit.Basic_8bit_Encoding,
               Source);

         --  If True, xmlns:* attributes will be reported in Start_Element
         XMPP_Parser.Set_Feature
           (Reader, Sax.Readers.Namespace_Prefixes_Feature, False);
         XMPP_Parser.Set_Feature
           (Reader, Sax.Readers.Validation_Feature, False);

         XMPP_Parser.Parse (Reader, Source);

         Close (Source);

         --  Add message into the Mailbox

         XMPP_Parser.Process (Account.all, Reader.R);
         XMPP_Parser.Release (Reader.R);
      end Parse_Message;

   begin
      loop
         declare
            XML_Response : constant String :=
                             Translator.To_String
                               (Net.Buffered.Read (Account.Sock.all));
            Start, Stop  : Positive := XML_Response'First;
         begin
            loop
               Get_Message (XML_Response, Start, Stop);

               exit when Start > XML_Response'Last;

               if Start < Stop then
                  Parse_Message (XML_Response (Start .. Stop));
               end if;
            end loop;
         end;
      end loop;

   exception
      when Net.Socket_Error =>
         --  We have been deconnected, this is the way Jabber terminate the
         --  session.
         null;
   end Incoming_Stream;

   ---------------
   -- XMPP_Send --
   ---------------

   procedure XMPP_Send (Account : in Client.Account; Message : in String) is
   begin
      Net.Buffered.Put_Line (Account.Sock.all, Message);
      Net.Buffered.Flush (Account.Sock.all);
   end XMPP_Send;

end AWS.Jabber.Client;
