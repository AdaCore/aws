------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                            Copyright (C) 2002                            --
--                               ACT-Europe                                 --
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
with Ada.Strings.Fixed;
with Ada.Text_IO;
with Ada.Unchecked_Deallocation;

with Input_Sources.Strings;
with Sax.Attributes;
with Sax.Readers;
with SHA;
with SHA.Process_Data;
with SHA.Strings;
with Unicode.CES.Basic_8bit;

with AWS.Net.Buffered;
with AWS.Translator;
with AWS.Utils;

package body AWS.Jabber is

   use Ada;

   ----------------
   -- XML Reader --
   ----------------

   --  SAX overloaded routines to parse the incoming XML stream.

   type Tree_Reader is new Sax.Readers.Reader with record
      R     : Message_Access;
      Key   : Unbounded_String;
      Value : Unbounded_String;
   end record;

   procedure Start_Element
     (Handler       : in out Tree_Reader;
      Namespace_URI : in     Unicode.CES.Byte_Sequence       := "";
      Local_Name    : in     Unicode.CES.Byte_Sequence       := "";
      Qname         : in     Unicode.CES.Byte_Sequence       := "";
      Atts          : in     Sax.Attributes.Attributes'Class);

   procedure End_Element
     (Handler       : in out Tree_Reader;
      Namespace_URI : in     Unicode.CES.Byte_Sequence := "";
      Local_Name    : in     Unicode.CES.Byte_Sequence := "";
      Qname         : in     Unicode.CES.Byte_Sequence := "");

   procedure Characters
     (Handler : in out Tree_Reader;
      Ch      : in     Unicode.CES.Byte_Sequence);

   procedure Ignorable_Whitespace
     (Handler : in out Tree_Reader;
      Ch      : in     Unicode.CES.Byte_Sequence);

   ----------------------
   -- Other Local Spec --
   ----------------------

   procedure Release (Message : in out Message_Access);
   --  Release all memory associated with the response object.

   function Jabber_ID (ID : in String) return String;
   pragma Inline (Jabber_ID);
   --  Returns the Jabber ID for ID. It removes the resource if present.

   function To_Presence_Status (Show : in String) return Presence_Status;
   --  Returns the Presence_Status from a show message read on a
   --  presence message. Returns Offline if Status is not recongnized.

   function Value (M : in Message_Access; Key : in String) return String;
   pragma Inline (Value);
   --  Returns the value for Key in the message M or the Empty string if the
   --  Key is not found.

   procedure Raise_Exception (Message : in String);
   pragma No_Return (Raise_Exception);
   --  Raise Server_Error exception with Message.

   procedure Check_Message (Message : in out Message_Access);
   --  Check that Message is ok. If it is an error raise Server_Error with the
   --  Exception_Message and the error's tag value.

   function Digest (Password : in String) return String;
   --  Returns password's digest for the Jabber authentication. This is the
   --  Base64 encoded SHA password's signature.

   function User_JID (Server : in Jabber.Server) return String;
   pragma Inline (User_JID);
   --  Returns the JID of the connected user.

   ----------------
   -- Characters --
   ----------------

   procedure Characters
     (Handler : in out Tree_Reader;
      Ch      : in     Unicode.CES.Byte_Sequence) is
   begin
      Append (Handler.Value, To_Unbounded_String (Ch));
   end Characters;

   -------------------
   -- Check_Message --
   -------------------

   procedure Check_Message (Message : in out Message_Access) is
      Error : constant String := Value (Message, "error");
   begin
      if Error /= "" then
         Raise_Exception (Error);
      end if;
   end Check_Message;

   --------------------
   -- Check_Presence --
   --------------------

   procedure Check_Presence
     (Server : in     Jabber.Server;
      JID    : in     String;
      Status :    out Presence_Status)
   is
      Message : Message_Access;
      Try     : Natural := 3;  -- Number of time to wait for server's response
   begin
      --  Send a presence inquiry message

      Net.Buffered.Put_Line
        (Server.Sock.all,
         "<presence xmlns='jabber:client' type='probe' id='ja_cp'"
           & " from='partage@ada.ldc.edf.fr'"
           & " to='" & JID & "'>"
           & "</presence>");

      Net.Buffered.Flush (Server.Sock.all);

      Check_Presence_Response : loop
         --  Wait for an incoming response

         while Server.MB.Size = 0 loop
            delay 0.5;
            Try := Try - 1;

            exit Check_Presence_Response when Try = 0;
         end loop;

         Server.Self.MB.Get (Message);
         Check_Message (Message);

         exit Check_Presence_Response
           when Jabber_ID (Value (Message, "presence.from")) = JID;

         Release (Message);
      end loop Check_Presence_Response;

      if Try = 0 then
         --  No message have arrived, JID is certainly offline
         Status := Offline;

      else
         Status := To_Presence_Status (Value (Message, "show"));

         Release (Message);
      end if;
   end Check_Presence;

   -----------
   -- Close --
   -----------

   procedure Close (Server : in out Jabber.Server) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Incoming_Stream, Incoming_Stream_Access);
   begin
      if Server.Started then
         --  Let's annouce that we are going offline

         Net.Buffered.Put_Line
           (Server.Sock.all,
            "<presence type='unavailable' from='"
              & To_String (Server.User)
              & '@'
              & To_String (Server.Host) & "'/>");

         Net.Buffered.Flush (Server.Sock.all);

         --  Wait for the message to be received

         delay 1.0;

         --  Release all messages from the Mailbox

         Server.MB.Destroy;

         --  Send closing stream element

         Net.Buffered.Put_Line (Server.Sock.all, "</stream:stream>");
         Net.Buffered.Shutdown (Server.Sock.all);
         Net.Free (Server.Sock);

         --  Terminate task Incoming_Stream

         while not Server.Stream'Terminated loop
            delay 1.0;
         end loop;

         Free (Server.Stream);
      end if;
   end Close;

   -------------
   -- Connect --
   -------------

   procedure Connect
     (Server   : in out Jabber.Server;
      Host     : in     String;
      User     : in     String;
      Password : in     String;
      Port     : in     Positive      := Default_Port)
   is
      Message : Message_Access;
   begin
      Server.User := To_Unbounded_String (User);
      Server.Host := To_Unbounded_String (Host);
      Server.Port := Port;

      --  Open socket to Jabber Server

      Server.Sock := Net.Socket (Security => False);

      begin
         Net.Connect (Server.Sock.all, Host, Port);
      exception
         when Net.Socket_Error =>
            Raise_Exception ("Can't connect to "
                               & Host & ':' & Utils.Image (Port));
      end;

      --  Start Incoming_Stream reader

      Server.Stream := new Incoming_Stream (Server.Self);

      Server.Started := True;

      --  Initialize the Jabber protocol

      Net.Buffered.Put_Line
        (Server.Sock.all,
         "<?xml version='1.0' encoding='UTF-8' ?>"
           & "<stream:stream to=" & Utils.Quote (Host)
           & " xmlns='jabber:client'"
           & " xmlns:stream='http://etherx.jabber.org/streams'>");


      Net.Buffered.Flush (Server.Sock.all);

      Server.MB.Get (Message);
      Check_Message (Message);

      --  Get Session ID from the stream element, record it. This will be used
      --  below for the authentication.

      Server.SID := To_Unbounded_String (Value (Message, "stream.id"));

      Release (Message);

      --  Authentication phase using jabber:iq:auth method

      Net.Buffered.Put_Line
        (Server.Sock.all,
         "<iq xmlns='jabber:client' type='get' id='ja_auth'>"
           & " <query xmlns='jabber:iq:auth'>"
           & "    <username>" & User & "</username>"
           & "</query>"
           & "</iq>");

      Net.Buffered.Flush (Server.Sock.all);

      --  Check which kind of authentication is supported

      Server.MB.Get (Message);
      Check_Message (Message);

      if Containers.Key_Value.Is_Present (Message.all, "digest") then
         --  Digest authentication supported, this is the prefered method if
         --  supported to avoid sending the password in plain ASCII over the
         --  Internet.

         Net.Buffered.Put_Line
           (Server.Sock.all,
            "<iq xmlns='jabber:client' type='set' id='ja_shaauth'>"
              & " <query xmlns='jabber:iq:auth'>"
              & "    <username>" & User & "</username>"
              & "    <digest>"
              & Digest (To_String (Server.SID) & Password)
              & "</digest>"
              & "    <resource>Exodus</resource>"
              & "</query>"
              & "</iq>");

      elsif Containers.Key_Value.Is_Present (Message.all, "password") then
         --  Plain authentication supported, use this one if digest is not
         --  supported by the server.

         Net.Buffered.Put_Line
           (Server.Sock.all,
            "<iq xmlns='jabber:client' type='set' id='ja_sauth'>"
              & " <query xmlns='jabber:iq:auth'>"
              & "    <username>" & User & "</username>"
              & "    <password>" & Password & "</password>"
              & "    <resource>Exodus</resource>"
              & "</query>"
              & "</iq>");
      end if;

      Net.Buffered.Flush (Server.Sock.all);

      Release (Message);

      --  Check that authentication is successful

      Server.MB.Get (Message);
      Check_Message (Message);
      Release (Message);

      --  Send our presence, as this is an application and not a real user we
      --  send an initial dnd (Do Not Disturb) status.

      Net.Buffered.Put_Line
        (Server.Sock.all,
         "<presence from='" & User_JID (Server) & "' id='ja_pres'>"
           & "<show>dnd</show>"
           & "<status>AWS Project</status>"
           & "</presence>");

      Net.Buffered.Flush (Server.Sock.all);

      Server.MB.Get (Message);
      Check_Message (Message);
      Release (Message);

   exception
      when others =>
         --  We must close the server properly before leaving this routine if
         --  an exception is raised.
         Close (Server);
         raise;
   end Connect;

   ------------
   -- Digest --
   ------------

   function Digest (Password : in String) return String is
   begin
      return String (SHA.Strings.Hex_From_SHA
                       (SHA.Process_Data.Digest_A_String (Password)));
   end Digest;

   -----------------
   -- End_Element --
   -----------------

   procedure End_Element
     (Handler       : in out Tree_Reader;
      Namespace_URI : in     Unicode.CES.Byte_Sequence := "";
      Local_Name    : in     Unicode.CES.Byte_Sequence := "";
      Qname         : in     Unicode.CES.Byte_Sequence := "")
   is
      pragma Unreferenced (Namespace_URI);
      pragma Unreferenced (Local_Name);
      pragma Unreferenced (Qname);
   begin
      if Handler.Key /= Null_Unbounded_String then
         Containers.Key_Value.Insert
           (Handler.R.all, To_String (Handler.Key), Handler.Value);
      end if;

      Handler.Key   := Null_Unbounded_String;
      Handler.Value := Null_Unbounded_String;
   exception
      when others =>
         null;
   end End_Element;

   ---------------------
   -- Incoming_Stream --
   ---------------------

   task body Incoming_Stream is

      procedure Get_Message (XML : in String; Start, Stop : in out Positive);
      --  Returns Start and Stop where XML (Start .. Stop) is the next XML
      --  chunk. Start and Stop are initialy set as bound for the previous
      --  slice. The first time this routine is called we have
      --  Start = Stop = XML'First. Returns Start = Stop if this tag must be
      --  skipped and Start > XML'Last when there is nothing more to read.

      procedure Parse_Message (XML : in String);

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
            Start := XML'Last;
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
            else
               return "";
            end if;
         end Message_Suffix;

         XML_Message : aliased String := XML & Message_Suffix;

         Source      : String_Input;
         Reader      : Tree_Reader;
      begin
         Reader.R := new Message;

         --  Parse the XML message

         Open (XML_Message'Unchecked_Access,
               Unicode.CES.Basic_8bit.Basic_8bit_Encoding,
               Source);

         --  If True, xmlns:* attributes will be reported in Start_Element
         Set_Feature (Reader, Sax.Readers.Namespace_Prefixes_Feature, False);
         Set_Feature (Reader, Sax.Readers.Validation_Feature, False);

         Parse (Reader, Source);

         Close (Source);

         --  Add message into the Mailbox

         Server.MB.Add (Reader.R);

      exception
         when others =>
            --  Let's release the message here.
            Release (Reader.R);
      end Parse_Message;

   begin
      loop
         declare
            XML_Response : constant String
              := Translator.To_String (Net.Buffered.Read (Server.Sock.all));

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

      when E : others =>
         Text_IO.Put_Line (Exceptions.Exception_Information (E));
   end Incoming_Stream;

   --------------------------
   -- Ignorable_Whitespace --
   --------------------------

   procedure Ignorable_Whitespace
     (Handler : in out Tree_Reader;
      Ch      : in     Unicode.CES.Byte_Sequence) is
   begin
      Append (Handler.Value, Ch);
   end Ignorable_Whitespace;

   ---------------
   -- Jabber_ID --
   ---------------

   function Jabber_ID (ID : in String) return String is
      K : constant Natural := Strings.Fixed.Index (ID, "/");
   begin
      if K = 0 or else K = ID'First then
         return ID;
      else
         return ID (ID'First .. K - 1);
      end if;
   end Jabber_ID;

   -------------
   -- Mailbox --
   -------------

   protected body Mailbox is

      ---------
      -- Add --
      ---------

      entry Add (M : in Message_Access) when Current_Size < Max_Size is
      begin
         Current_Size := Current_Size + 1;
         Current := Current + 1;

         if Current > Max_Size then
            Current := Buffer'First;
         end if;

         Buffer (Current) := M;
      end Add;

      -------------
      -- Destroy --
      -------------

      procedure Destroy is
      begin
         while Last /= Current loop
            Last := Last + 1;

            if Last > Max_Size then
               Last := Buffer'First;
            end if;

            Release (Buffer (Last));
         end loop;
      end Destroy;

      ---------
      -- Get --
      ---------

      entry Get (M : out Message_Access) when Current_Size > 0 is
      begin
         Current_Size := Current_Size - 1;
         Last := Last + 1;

         if Last > Max_Size then
            Last := Buffer'First;
         end if;

         M := Buffer (Last);
         Buffer (Last) := null;
      end Get;

      ----------
      -- Size --
      ----------

      function Size return Natural is
      begin
         return Current_Size;
      end Size;

   end Mailbox;

   ---------------------
   -- Raise_Exception --
   ---------------------

   procedure Raise_Exception (Message : in String) is
   begin
      Exceptions.Raise_Exception (Server_Error'Identity, "Jabber: " & Message);
   end Raise_Exception;

   -------------
   -- Release --
   -------------

   procedure Release (Message : in out Message_Access) is
      procedure Free is
         new Ada.Unchecked_Deallocation (Jabber.Message, Message_Access);
   begin
      if Message /= null then
         Containers.Key_Value.Destroy (Message.all);
         Free (Message);
      end if;
   end Release;

   ------------------
   -- Send_Message --
   ------------------

   procedure Send_Message
     (Server  : in Jabber.Server;
      JID     : in String;
      Subject : in String;
      Content : in String)
   is
      Message : Message_Access;
   begin
      --  Send Message

      Net.Buffered.Put_Line
        (Server.Sock.all,
         "<message xmlns='jabber:client' type='headline'"
           & " id='id_msg' to='" & JID &"'>"
           & " <thread xmlns='jabber:client'>ja_msg</thread>"
           & " <subject xmlns='jabber:client'>" & Subject & "</subject>"
           & " <body xmlns='jabber:client'>" & Content & "</body>"
           & "</message>");

      Net.Buffered.Flush (Server.Sock.all);

      Server.Self.MB.Get (Message);
      Check_Message (Message);
      Release (Message);
   end Send_Message;

   -------------------
   -- Start_Element --
   -------------------

   procedure Start_Element
     (Handler       : in out Tree_Reader;
      Namespace_URI : in     Unicode.CES.Byte_Sequence       := "";
      Local_Name    : in     Unicode.CES.Byte_Sequence       := "";
      Qname         : in     Unicode.CES.Byte_Sequence       := "";
      Atts          : in     Sax.Attributes.Attributes'Class)
   is
      pragma Unreferenced (Namespace_URI);
      pragma Unreferenced (Qname);

      use Sax.Attributes;
   begin
      Handler.Key := To_Unbounded_String (Local_Name);

      --  Read all attributes, add a key/value pair for each atributes into
      --  the table with [Local_Name & '.'] added in from of the key (attribute
      --  name)

      for J in 0 .. Get_Length (Atts) - 1 loop
         begin
            Containers.Key_Value.Insert
              (Handler.R.all,
               Local_Name & '.' & Get_Qname (Atts, J),
               To_Unbounded_String (Get_Value (Atts, J)));
         exception
            when Containers.Key_Value.Table.Duplicate_Item_Error =>
               null;
         end;
      end loop;
   end Start_Element;

   ------------------------
   -- To_Presence_Status --
   ------------------------

   function To_Presence_Status (Show : in String) return Presence_Status is
   begin
      if Show = "" or else Show = "online" then
         return Available;

      elsif Show = "dnd" then
         return Do_Not_Disturb;

      elsif Show = "away" then
         return Away;

      elsif Show = "xa" then
         return Extended_Away;

      elsif Show = "chat" then
         return Chat;

      else
         --  If the presence/show tag is not recongnized, pretend that it is
         --  Offline. This should happen only if there is some Jabber protocol
         --  error.

         return Offline;
      end if;
   end To_Presence_Status;

   --------------
   -- User_JID --
   --------------

   function User_JID (Server : in Jabber.Server) return String is
   begin
      return To_String (Server.User) & '@' & To_String (Server.Host);
   end User_JID;

   -----------
   -- Value --
   -----------

   function Value (M : in Message_Access; Key : in String) return String is
   begin
      if Containers.Key_Value.Is_Present (M.all, Key) then
         return To_String (Containers.Key_Value.Value (M.all, Key));
      else
         return "";
      end if;
   end Value;

end AWS.Jabber;
