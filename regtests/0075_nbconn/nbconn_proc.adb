------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2004-2012, AdaCore                     --
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

--  Common code for test non blocking connection

with Ada.Exceptions;
with Ada.Streams;
with Ada.Text_IO;

with GNAT.OS_Lib;

with AWS.Net.Buffered;
with AWS.Net.SSL;

procedure NBConn_Proc (Security : Boolean) is

   use Ada.Exceptions;
   use Ada.Text_IO;
   use GNAT.OS_Lib;
   use AWS;

   Queue_Size : constant := 5;
   Server     : Net.Socket_Type'Class := Net.Socket (False);
   Clients    : array (1 .. Queue_Size + 2) of Net.Socket_Access;
   Sample     : constant Ada.Streams.Stream_Element_Array (1 .. 100) :=
                  (others => 1);

   task Server_Task is
      entry Start;
      entry Done;
   end Server_Task;

   -----------------
   -- Server_Task --
   -----------------

   task body Server_Task is
      Peer : Net.Socket_Access;
   begin
      if Net.IPv6_Available then
         Server.Bind (0, Family => Net.Family_Inet6);
      else
         Server.Bind (0, Family => Net.Family_Inet);
      end if;

      Server.Listen (Queue_Size);

      Put_Line ("Server ready.");

      accept Start;

      for J in Clients'Range loop
         Peer := Net.Socket (Security);

         Server.Accept_Socket (Peer.all);

         declare
            use Ada.Streams;
            Buffer : Stream_Element_Array (Sample'Range);
         begin
            Net.Buffered.Read (Peer.all, Buffer);

            if Buffer /= Sample then
               Put_Line ("Data distortion.");
            end if;
         end;

         Peer.Shutdown;
         Net.Free (Peer);
      end loop;

      accept Done;

      Server.Shutdown;

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
   Server_Task.Start;

   for J in Clients'Range loop
      Clients (J) := Net.Socket (Security);
      Clients (J).Connect
        (Net.Localhost (Net.IPv6_Available), Server.Get_Port, Wait => False);
      Put_Line ("connect" & Integer'Image (J));
   end loop;

   for J in Clients'Range loop
      declare
         Events : constant Net.Event_Set :=
                    Net.Wait
                      (Clients (J).all,
                        (Net.Output => True, others => False));
      begin
         if Events (Net.Output) then
            Put ("connected");
         else
            Put ("error");
         end if;

         Put_Line (Integer'Image (J));

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

exception
   when E : others =>
      Put_Line ("Main task " & Exception_Information (E));

      select
        Server_Task.Done;
      or
         delay 1.0;
      end select;
end NBConn_Proc;
