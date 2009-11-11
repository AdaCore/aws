------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2008-2009, AdaCore                     --
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

with Ada.Command_Line;
with Ada.Exceptions;
with Ada.Tags;
with Ada.Text_IO;
with Ada.Strings.Fixed;
with Ada.Unchecked_Deallocation;

with AWS.Containers;
with AWS.Jabber.Client;
with AWS.Net.Acceptors;
with AWS.Net.SSL;
with AWS.Net.Buffered;
with AWS.Utils;

with Get_Free_Port;

procedure Test_Jabber is

   use type Ada.Tags.Tag;
   use Ada;
   use AWS;
   use AWS.Net;
   use Ada.Exceptions;
   use AWS.Jabber.Client;

   Verbose   : constant Boolean := False;
   Free_Port : Positive := 8422;

   task type Server_Task is
      entry Started;
   end Server_Task;

   -----------------
   -- Server_Task --
   -----------------

   task body Server_Task is
      use AWS.Containers;

      Acceptor : Acceptors.Acceptor_Type;
      Sock     : Socket_Access;

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
      Acceptors.Listen (Acceptor, "", Free_Port, 11);
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

            elsif Strings.Fixed.Index (Got_Message, "<presence") /= 0 then
               Ada.Text_IO.Put_Line ("Success");
               exit;
            end if;
         end;
      end loop;

      Acceptors.Give_Back (Acceptor, Sock);
      Acceptors.Shutdown (Acceptor);
      Shutdown (Sock.all);
      Free (Sock);

   exception
      when E : Socket_Error =>
         Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
         Shutdown (Sock.all);
         Free (Sock);
      when E : others =>
         Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
   end Server_Task;

begin
   Get_Free_Port (Free_Port);
   Run : declare
      Jabber_Server : Server_Task;
   begin
      Jabber_Server.Started;
      --  Wait for the server to start

      declare
         Account : Jabber.Client.Account;
      begin

         Set_Host (Account, "127.0.0.1");
         Set_Login_Information (Account, "user", "passwd");
         Set_Authentication_Type (Account, Plain_Mechanism);
         Set_Port (Account, Port (Free_Port));

         Connect (Account);
      end;
   end Run;
end Test_Jabber;
