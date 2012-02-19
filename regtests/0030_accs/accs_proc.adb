------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2005-2012, AdaCore                     --
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

with Ada.Exceptions;
with Ada.Tags;
with Ada.Text_IO;
with Ada.Unchecked_Deallocation;

with AWS.Net.Acceptors;
with AWS.Net.SSL;
with AWS.Net.Buffered;
with AWS.Utils;

with Stack_Size;

procedure Accs_Proc (Security : Boolean) is

   use type Ada.Tags.Tag;
   use AWS.Net;

   Client_Request_Count : constant := 10;

   Acceptor  : Acceptors.Acceptor_Type;
   Counter   : AWS.Utils.Counter (0);
   Semaphore : AWS.Utils.Semaphore;

   task type Server_Task is
      pragma Storage_Size (Stack_Size.Value);
   end Server_Task;

   task type Client_Task is
      pragma Storage_Size (Stack_Size.Value);
      entry Start (Index : Positive);
   end Client_Task;

   -----------------
   -- Client_Task --
   -----------------

   task body Client_Task is
      Sock      : Socket_Type'Class := Socket (Security);
      Index     : Positive;
      Prefix    : constant String := "index";
      Connected : Boolean := False;
   begin
      accept Start (Index : Positive) do
         Client_Task.Index := Index;
      end Start;

      declare
         AS : constant Socket_Type'Class := Acceptors.Server_Socket (Acceptor);
      begin
         Connect (Sock, Localhost (AS.Is_IPv6), AS.Get_Port);
      end;

      Connected := True;

      Set_Timeout (Sock, 6.0);

      for J in 1 .. Client_Request_Count loop
         Buffered.Put_Line
           (Sock, Prefix & Positive'Image (Index) & Positive'Image (J));

         Buffered.Flush (Sock);

         declare
            Response : constant String := Buffered.Get_Line (Sock);
         begin
            if Response /= '(' & Prefix & Positive'Image (Index)
                           & Positive'Image (J) & ')'
            then
               Ada.Text_IO.Put_Line ("Error " & Response);
            end if;
         end;
      end loop;

      Shutdown (Sock);

      Ada.Text_IO.Put_Line ("Client gone.");

   exception
      when E : others =>
         Ada.Text_IO.Put_Line
           ("Client " & Ada.Exceptions.Exception_Information (E));

         if Connected then
            Shutdown (Sock);
         end if;
   end Client_Task;

   -----------------
   -- Server_Task --
   -----------------

   task body Server_Task is
      Sock : Socket_Access;
   begin
      loop
         begin
            Semaphore.Seize;
            Acceptors.Get (Acceptor, Sock);
            Semaphore.Release;

         exception
            when Socket_Error =>
               Semaphore.Release;
               Ada.Text_IO.Put_Line ("Acceptor closed.");
               exit;
            when others =>
               Semaphore.Release;
               raise;
         end;

         if Security and then not (Sock'Tag = SSL.Socket_Type'Tag) then
            declare
               procedure Free is new Ada.Unchecked_Deallocation
                 (Socket_Type'Class, Socket_Access);
               Tmp : Socket_Access := Sock;
            begin
               Sock := new SSL.Socket_Type'(SSL.Secure_Server (Sock.all));
               Free (Tmp);
            end;
         end if;

         Set_Timeout (Sock.all, 4.0);

         begin
            Buffered.Put_Line
              (Sock.all, '(' & Buffered.Get_Line (Sock.all) & ')');
            Buffered.Flush (Sock.all);

            Counter.Increment;

            Acceptors.Give_Back (Acceptor, Sock);

         exception when Socket_Error =>
            Shutdown (Sock.all);
            Free (Sock);
         end;

      end loop;

   exception
      when E : others =>
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
   end Server_Task;

   Clients : array (1 .. 7) of Client_Task;

begin
   Semaphore.Seize;

   Acceptors.Listen (Acceptor, "", 0, 11);

   declare
      Servers : array (1 .. 3) of Server_Task;
   begin
      Semaphore.Release;

      for J in Clients'Range loop
         Clients (J).Start (J);
      end loop;

      for J in Clients'Range loop
         while not Clients (J)'Terminated loop
            delay 0.25;
         end loop;
      end loop;

      Acceptors.Shutdown (Acceptor);

      --  Wait for servers termination only for console output ordering

      for J in Servers'Range loop
         while not Servers (J)'Terminated loop
            delay 0.25;
         end loop;
      end loop;

      if Counter.Value = Client_Request_Count * Clients'Length then
         Ada.Text_IO.Put_Line ("Done.");
      else
         Ada.Text_IO.Put_Line
           ("Regression" & Integer'Image (Counter.Value)
            & " /=" & Integer'Image (Client_Request_Count)
            & " *" & Integer'Image (Clients'Length));
      end if;
   end;

exception
   when E : others =>
      Ada.Text_IO.Put_Line
        ("Main task " & Ada.Exceptions.Exception_Information (E));
end Accs_Proc;
