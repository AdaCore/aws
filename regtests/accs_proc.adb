------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                            Copyright (C) 2005                            --
--                                ACT-Europe                                --
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
with Ada.Text_IO;

with AWS.Net.Acceptors;
with AWS.Net.Buffered;
with AWS.Utils;

with Get_Free_Port;

procedure Accs_Proc (Security : in Boolean) is
   use AWS.Net;
   Free_Port : Positive := 8421;
   Acceptor  : Acceptors.Acceptor_Type;

   Semaphore : AWS.Utils.Semaphore;

   task type Server_Task;

   task type Client_Task is
      entry Start (Index : Positive);
   end Client_Task;

   task body Client_Task is
      Sock   : Socket_Type'Class := Socket (Security);
      Index  : Positive;
      Prefix : constant String := "index";
   begin
      accept Start (Index : Positive) do
         Client_Task.Index := Index;
      end Start;

      Connect (Sock, "127.0.0.1", Free_Port);

      delay 0.15;

      Set_Timeout (Sock, 3.0);

      for J in 1 .. 10 loop
         Ada.Text_IO.Put_Line ("send"); -- Ada.Text_IO.Flush;

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
      Free (Sock);

      Ada.Text_IO.Put_Line ("Client gone " & Positive'Image (Index));

   exception
      when E : others =>
         Ada.Text_IO.Put_Line
           ("Client " & Ada.Exceptions.Exception_Information (E));

         Shutdown (Sock);
         Free (Sock);
   end Client_Task;

   task body Server_Task is
      Sock : Socket_Access;
   begin
      loop
         begin
            --  Ada.Text_IO.Put_Line
            --  ("server ___ " & Ada.Task_Identification.Image
            --                   (Ada.Task_Identification.Current_Task));
            Semaphore.Seize;
            Acceptors.Get (Acceptor, Sock);
            Semaphore.Release;
         exception when E : Socket_Error =>
            Semaphore.Release;
            Ada.Text_IO.Put_Line ("Acceptor closed.");
            raise;
         end;

         Set_Timeout (Sock.all, 2.0);

         declare
            Request : constant String := Buffered.Get_Line (Sock.all);
         begin
            Buffered.Put_Line (Sock.all, '(' & Request & ')');
            Buffered.Flush (Sock.all);
         end;

         Acceptors.Give_Back (Acceptor, Sock);
      end loop;

   exception
      when E : Socket_Error => Ada.Text_IO.Put_Line ("Server done");
      when E : others =>
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
   end Server_Task;

   Clients : array (1 .. 2) of Client_Task;

begin
   Semaphore.Seize;

   Get_Free_Port (Free_Port);
   Acceptors.Listen (Acceptor, "", Free_Port, 11);

   declare
      Servers : array (1 .. 1) of Server_Task;
      pragma Unreferenced (Servers);
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
   end;
end Accs_Proc;
