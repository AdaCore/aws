------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2008-2012, AdaCore                     --
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

with Ada.Command_Line;
with Ada.Exceptions;
with Ada.Tags;
with Ada.Text_IO;
with Ada.Strings.Fixed;
with Ada.Unchecked_Deallocation;

with AWS.Containers;
with AWS.Jabber.Client;
with AWS.Net.Acceptors;
with AWS.Net.Buffered;
with AWS.Net.SSL;
with AWS.Net.Std;
with AWS.Utils;

procedure Test_Jabber is

   use type Ada.Tags.Tag;
   use Ada;
   use AWS;
   use AWS.Net;
   use Ada.Exceptions;
   use AWS.Jabber.Client;

   Verbose  : constant Boolean := False;
   Acceptor : Acceptors.Acceptor_Type;

   task type Server_Task is
      entry Started;
      entry Can_Stop;
      entry Shutdown;
      entry Stopped;
   end Server_Task;

   -----------------
   -- Server_Task --
   -----------------

   task body Server_Task is
      use AWS.Containers;

      Sock     : Socket_Access;
      First    : Boolean := True;

      procedure Buffered_Put_Line
        (Socket : Socket_Type'Class; Item : String);
      --  Print Text message and then call Buffered.Put_Line

      -----------------------
      -- Buffered_Put_Line --
      -----------------------

      procedure Buffered_Put_Line
        (Socket : Socket_Type'Class; Item : String) is
      begin
         if Verbose then
            Text_IO.New_Line;
            Text_IO.Put_Line ("Server :: " & Item);
         end if;
         Buffered.Put_Line (Socket, Item);
      end Buffered_Put_Line;

   begin
      Acceptors.Listen (Acceptor, "", 0, 11);
      accept Started;
      Acceptors.Get (Acceptor, Sock);

      Set_Timeout (Sock.all, 4.0);

      --  Client initialize the connection

      declare
         Got_Message : constant String := Buffered.Get_Line (Sock.all);
      begin
         if Verbose then
            Text_IO.New_Line;
            Text_IO.Put_Line ("Client :: [" & Got_Message & "]");
         end if;
      end;

      --  Return new stream.id

      Buffered_Put_Line
        (Sock.all,
         "<?xml version='1.0'?><stream:stream xmlns='jabber:client' "
           & "xmlns:stream='http://etherx.jabber.org/streams' id='3EE948B0' "
           & "from='127.0.0.1' version='1.0' xml:lang='en'>");

      Buffered.Flush (Sock.all);

      --  Advertize mechanisms

      Buffered_Put_Line
        (Sock.all,
         "<stream:features><starttls xmlns='urn:ietf:params:xml:ns:xmpp-tls'/>"
           & "<mechanisms xmlns='urn:ietf:params:xml:ns:xmpp-sasl'>"
           & "<mechanism>DIGEST-MD5</mechanism><mechanism>PLAIN</mechanism>"
           & "</mechanisms>"
           & "<register xmlns='http://jabber.org/features/iq-register'/>"
           & "</stream:features>");

      Buffered.Flush (Sock.all);

      loop
         declare
            Got_Message : constant String := Buffered.Get_Line (Sock.all);
         begin
            if Verbose then
               Text_IO.New_Line;
               Text_IO.Put_Line
                 ("############# Client :: [" & Got_Message & "]");
            end if;

            if Strings.Fixed.Index (Got_Message, "PLAIN") /= 0 then
               --  Client authentication (in PLAIN mode)
               --  Return success

               Buffered_Put_Line
                 (Sock.all,
                  "<success xmlns='urn:ietf:params:xml:ns:xmpp-sasl'/>");
               Buffered.Flush (Sock.all);

            elsif Strings.Fixed.Index (Got_Message, "<stream:stream") /= 0 then
               --  Client create new stream
               --  Return supported features

               Buffered_Put_Line
                 (Sock.all,
                  "<?xml version='1.0'?><stream:stream xmlns='jabber:client' "
                    & "xmlns:stream='http://etherx.jabber.org/streams' "
                    & "id='3EE948B0' from='127.0.0.1'"
                    & " version='1.0' xml:lang='en'>"
                    & "<stream:features>"
                    & "<bind xmlns='urn:ietf:params:xml:ns:xmpp-bind'/>"
                    & "<session xmlns='urn:ietf:params:xml:ns:xmpp-session'/>"
                    & "</stream:features>");
               Buffered.Flush (Sock.all);

            elsif Strings.Fixed.Index (Got_Message,
                                       "<iq type='set' id='bind") /= 0
            then
               --  Client's bind request
               --  Return bind result

               Buffered_Put_Line
                 (Sock.all,
                  "<iq id='bind_1' type='result'>"
                    & "<bind xmlns='urn:ietf:params:xml:ns:xmpp-bind'>"
                    & "<jid>user@192.168.1.4/Resource</jid></bind></iq>");
               Buffered.Flush (Sock.all);

            elsif Strings.Fixed.Index (Got_Message,
                                       "<iq type='set' id='sess") /= 0
            then
               --  Set session
               Buffered_Put_Line
                 (Sock.all,
                  "<iq type='result' id='sess_1'>"
                    & "<session "
                    & "xmlns='urn:ietf:params:xml:ns:xmpp-session'/></iq>");
               Buffered.Flush (Sock.all);

            elsif Strings.Fixed.Index
              (Got_Message, "<presence from=") /= 0
            then
               if First then
                  Ada.Text_IO.Put_Line ("Success.");
                  accept Can_Stop;
                  First := False;
               end if;

            elsif Strings.Fixed.Index
              (Got_Message, "<presence type='unavai") /= 0
            then
               Ada.Text_IO.Put_Line ("Going off line.");

            elsif Strings.Fixed.Index
              (Got_Message, "</stream:stream") /= 0
            then
               Ada.Text_IO.Put_Line ("End now.");
               exit;
            end if;
         end;
      end loop;

      accept Shutdown;

      Acceptors.Give_Back (Acceptor, Sock);
      Acceptors.Shutdown (Acceptor);

      accept Stopped;

   exception
      when E : Socket_Error =>
         Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
         Shutdown (Sock.all);
         Free (Sock);
      when E : others =>
         Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
   end Server_Task;

   Jabber_Server : Server_Task;

begin
   Jabber_Server.Started;
   --  Wait for the server to start

   Run : declare
      Account : Jabber.Client.Account;
      AS : Net.Socket_Type'Class := Acceptors.Server_Socket (Acceptor);
   begin
      Set_Host (Account, Localhost (AS.Is_IPv6));
      Set_Port (Account, Port (AS.Get_Port));

      Set_Login_Information (Account, "user", "passwd");
      Set_Authentication_Type (Account, Plain_Mechanism);

      Connect (Account);

      Jabber_Server.Can_Stop;

      Close (Account);

      Jabber_Server.Shutdown;

      Jabber_Server.Stopped;

      while not Jabber_Server'Terminated loop
         delay 1.0;
      end loop;
   end Run;

exception
   when E : others =>
      Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
      Jabber_Server.Shutdown;
      Jabber_Server.Stopped;
end Test_Jabber;
