------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2005-2008, AdaCore                     --
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

with Ada.Exceptions;
with Ada.Tags;
with Ada.Text_IO;
with Ada.Unchecked_Deallocation;

with AWS.Net.Acceptors;
with AWS.Net.SSL;
with AWS.Net.Buffered;
with AWS.Utils;

with Get_Free_Port;

procedure Accs_Proc (Security : in Boolean) is

   use type Ada.Tags.Tag;
   use AWS.Net;

   Client_Request_Count : constant := 10;

   Free_Port : Positive := 8421;
   Acceptor  : Acceptors.Acceptor_Type;
   Counter   : Natural := 0;
   pragma Atomic (Counter);

   Semaphore : AWS.Utils.Semaphore;

   task type Server_Task;

   task type Client_Task is
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

      Connect (Sock, "127.0.0.1", Free_Port);
      Connected := True;

      Set_Timeout (Sock, 3.0);

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
               raise;
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

         Set_Timeout (Sock.all, 2.0);

         begin
            Buffered.Put_Line
              (Sock.all, '(' & Buffered.Get_Line (Sock.all) & ')');
            Buffered.Flush (Sock.all);

            Counter := Counter + 1;

            Acceptors.Give_Back (Acceptor, Sock);

         exception when Socket_Error =>
            Shutdown (Sock.all);
            Free (Sock);
         end;

      end loop;

   exception
      when Socket_Error =>
         null;
      when E : others =>
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
   end Server_Task;

   Clients : array (1 .. 7) of Client_Task;

begin
   Semaphore.Seize;

   Get_Free_Port (Free_Port);
   Acceptors.Listen (Acceptor, "", Free_Port, 11);

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

      if Counter = Client_Request_Count * Clients'Length then
         Ada.Text_IO.Put_Line ("Done.");
      else
         Ada.Text_IO.Put_Line
           ("Regression" & Integer'Image (Counter)
            & " /=" & Integer'Image (Client_Request_Count)
            & " *" & Integer'Image (Clients'Length));
      end if;
   end;

exception
   when E : others =>
      Ada.Text_IO.Put_Line
        ("Main task " & Ada.Exceptions.Exception_Information (E));
end Accs_Proc;
