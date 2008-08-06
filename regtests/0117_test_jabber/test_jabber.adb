------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                          Copyright (C) 2008, AdaCore                     --
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
with Ada.Unchecked_Deallocation;

with AWS.Containers;
with AWS.Jabber;
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

   Verbose   : constant Boolean := False;
   Free_Port : Positive := 8421;

   Counter   : Natural := 0;
   State     : Natural := 0;

   Server    : Jabber.Server;

   task type Server_Task is
      entry Started;
   end Server_Task;

   procedure Run;

   -----------------
   -- Server_Task --
   -----------------

   task body Server_Task is
      use AWS.Containers;

      Acceptor : Acceptors.Acceptor_Type;
      Sock     : Socket_Access;

      procedure Buffered_Put_Line
        (Socket : in Socket_Type'Class; Item : in String);
      --  Print Text message and then call Buffered.Put_Line

      -----------------------
      -- Buffered_Put_Line --
      -----------------------

      procedure Buffered_Put_Line
        (Socket : in Socket_Type'Class; Item : in String) is
      begin
         if Verbose then
            Text_IO.Put_Line ("Server :: " & Item);
         end if;
         Buffered.Put_Line (Socket, Item);
      end Buffered_Put_Line;

   begin
      Acceptors.Listen (Acceptor, "", Free_Port, 11);

      accept Started;

      Acceptors.Get (Acceptor, Sock);

      Set_Timeout (Sock.all, 4.0);

      declare
         Got_Message : constant String := Buffered.Get_Line (Sock.all);
      begin
         if Verbose then
            Text_IO.New_Line;
            Text_IO.Put_Line ("Client :: [" & Got_Message & "]");
         end if;
      end;

      Buffered_Put_Line
        (Sock.all,
         "<?xml version='1.0'?>"
         & "<stream:stream"
         & " from='example.com'"
         & " id='3EE948B0'"
         & " xmlns='jabber:client'"
         & " xmlns:stream='http://etherx.jabber.org/streams'"
         & " version='1.0'>");

      Buffered.Flush (Sock.all);

      if Counter = 1 then
         State := 1;
         goto Shutdown_Task;
      end if;

      declare
         Got_Message : constant String := Buffered.Get_Line (Sock.all);
      begin
         if Verbose then
            Text_IO.New_Line;
            Text_IO.Put_Line ("Client :: [" & Got_Message & "]");
         end if;
      end;

      Buffered_Put_Line
        (Sock.all,
         "<iq type='result' id='ja_auth'>"
         & " <query xmlns='jabber:iq:auth'>"
         & " <username/>"
         & " <password/>"
         & " <resource/>"
         & " </query>"
         & " </iq>");

      Buffered.Flush (Sock.all);

      if Counter = 2 then
         State := 2;
         goto Shutdown_Task;
      end if;

      declare
         Got_Message : constant String := Buffered.Get_Line (Sock.all);
      begin
         if Verbose then
            Text_IO.New_Line;
            Text_IO.Put_Line ("Client :: [" & Got_Message & "]");
         end if;
      end;

      Buffered_Put_Line
        (Sock.all,
         "<iq type='result' id='ja_sauth'/>");
      Buffered.Flush (Sock.all);

      if Counter = 3 then
         State := 3;
         goto Shutdown_Task;
      end if;

      declare
         Got_Message : constant String := Buffered.Get_Line (Sock.all);
      begin
         if Verbose then
            Text_IO.New_Line;
            Text_IO.Put_Line ("Client :: [" & Got_Message & "]");
         end if;
      end;

      if Counter = 4 then
         State := 4;
         goto Shutdown_Task;
      end if;

      Buffered_Put_Line
        (Sock.all,
         "<presence from='user@192.168.1.4/Resource'>"
         & "<priority>1</priority></presence>");
      Buffered.Flush (Sock.all);

      if Counter = 5 then
         State := 5;
         goto Shutdown_Task;
      end if;

      declare
         Got_Message : constant String := Buffered.Get_Line (Sock.all);
      begin
         if Verbose then
            Text_IO.New_Line;
            Text_IO.Put_Line ("Message is [" & Got_Message & "]");
         end if;
      end;

      State := 6;

      <<Shutdown_Task>>

      delay 2.0; --  Wait for jabber client timeout

      Acceptors.Give_Back (Acceptor, Sock);
      Acceptors.Shutdown (Acceptor);
      Shutdown (Sock.all);
      Free (Sock);

   exception
      when Socket_Error =>
         Shutdown (Sock.all);
         Free (Sock);
      when E : others =>
         Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
   end Server_Task;

   ---------
   -- Run --
   ---------

   procedure Run is
   begin
      Get_Free_Port (Free_Port);

      if Counter = 0 then
         Test_Connection_Timeout : begin
            --  Try Jabber.Connect to test connection timeout
            Jabber.Connect (Server, "127.0.0.1", "user", "passwd", Free_Port);
         exception
            when Jabber.Server_Error =>
               Text_IO.Put_Line (Natural'Image (State));
               return;
         end Test_Connection_Timeout;

      elsif Counter >= 1 then
         Test_Client_Timeouts : declare
            Jabber_Server : Server_Task;
         begin
            Jabber_Server.Started;
            --  Wait for the server to start

            Jabber.Connect (Server, "127.0.0.1", "user", "passwd", Free_Port);

            if Counter >= 4 then
               Jabber.Send_Message
                 (Server, "test@test.com", "subject", "content");
            end if;
         exception
            when Jabber.Server_Error =>
               Ada.Text_IO.Put_Line (Natural'Image (State));
               return;
         end Test_Client_Timeouts;
      end if;

      Text_IO.Put_Line ("ERROR !");
   end Run;

begin
   for K in 1 .. 6 loop
      Counter := K;
      Run;
   end loop;
end Test_Jabber;
