------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2004-2008, AdaCore                     --
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

--  Common code for test non blocking connection

with Ada.Exceptions;
with Ada.Streams;
with Ada.Text_IO;

with GNAT.OS_Lib;

with AWS.Net.Buffered;
with AWS.Net.SSL;
with Get_Free_Port;

procedure NBConn_Proc (Security : in Boolean) is

   use Ada.Exceptions;
   use Ada.Text_IO;
   use GNAT.OS_Lib;
   use AWS;

   Queue_Size : constant := 5;
   Port       : Positive := 7000;
   Server     : Net.Socket_Type'Class := Net.Socket (False);
   Clients    : array (1 .. Queue_Size + 2) of Net.Socket_Access;
   Sample     : constant Ada.Streams.Stream_Element_Array (1 .. 100)
     := (others => 1);

   task Server_Task is
      entry Get (N : in Positive);
      entry Done;
   end Server_Task;

   -----------------
   -- Server_Task --
   -----------------

   task body Server_Task is
      Peer : Net.Socket_Access;
   begin
      for J in Clients'Range loop
         accept Get (N : in Positive) do
            Peer := Net.Socket (Security);
         end Get;

         Net.Accept_Socket (Server, Peer.all);

         declare
            use Ada.Streams;
            Buffer : Stream_Element_Array (Sample'Range);
         begin
            Net.Buffered.Read (Peer.all, Buffer);

            if Buffer /= Sample then
               Put_Line ("Data distortion.");
            end if;
         end;

         Net.Shutdown (Peer.all);
         Net.Free (Peer);
      end loop;

      accept Done;

   exception
      when E : others =>
         Put_Line ("Server task " & Ada.Exceptions.Exception_Information (E));

         select
           accept Done;
         or
            delay 1.0;
         end select;
   end Server_Task;

begin
   Get_Free_Port (Port);

   Net.Bind (Server, Port);
   Net.Listen (Server, Queue_Size);

   Put_Line ("Server ready.");

   for J in Clients'Range loop
      Clients (J) := Net.Socket (Security);
      Net.Connect (Clients (J).all, "localhost", Port, Wait => False);
      Put_Line ("connect" & Integer'Image (J));
   end loop;

   for J in Clients'Range loop
      declare
         Events : constant Net.Event_Set
           := Net.Wait
                (Clients (J).all,
                 (Net.Output => True, others => False));
      begin
         if Events (Net.Output) then
            Put ("connected");
         else
            Put ("error");
         end if;

         Put_Line (Integer'Image (J));

         Server_Task.Get (J);

         if Security then
            Net.SSL.Do_Handshake (Net.SSL.Socket_Type (Clients (J).all));
         end if;

         begin
            Net.Send (Clients (J).all, Sample);
         exception
            when E : others =>
               Put_Line ("Error sending data " & Exception_Message (E));
               --  If we get there, there is something really wrong and
               --  the server will wait on Accept_Socket. So we do an
               --  hard exit.
               OS_Exit (1);
         end;
      end;
   end loop;

   Server_Task.Done;

   for J in Clients'Range loop
      Net.Shutdown (Clients (J).all);
      Net.Free (Clients (J));
   end loop;

   Net.Shutdown (Server);

exception
   when E : others =>
      Put_Line ("Main task " & Exception_Information (E));

      select
        Server_Task.Done;
      or
         delay 1.0;
      end select;
end NBConn_Proc;
