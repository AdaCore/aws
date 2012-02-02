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

--  Test for output buffer overflow

with Ada.Exceptions;
with Ada.Streams;
with Ada.Text_IO;

with AWS.Net;

with Get_Free_Port;

procedure SockOver_Proc (Security : Boolean) is

   use AWS;
   use Ada;
   use Ada.Streams;

   One   : constant Stream_Element := 1;
   Two   : constant Stream_Element := 2;
   Three : constant Stream_Element := 3;

   Server : Net.Socket_Type'Class := Net.Socket (False);
   Peer   : Net.Socket_Type'Class := Net.Socket (Security);
   Client : Net.Socket_Type'Class := Net.Socket (Security);

   Free_Port : Positive := 8080;

   --------------
   -- Transmit --
   --------------

   procedure Transmit (Bound : Stream_Element_Offset) is
      Sample : constant Stream_Element_Array :=
        (Bound => One) & Two & Three & One & Two & Three & One & Two & Three
         & One & Two & Three & One & Two & Three & One & Two & Three
         & One & Two & Three & One & Two & Three & One & Two & Three
         & One & Two & Three & One & Two & Three & One & Two & Three
         & One & Two & Three & One & Two & Three;
      Single : Stream_Element_Array (1 .. 1) := (others => 32);
      Last   : Stream_Element_Offset;
      Buffer : Stream_Element_Array (1 .. 1024);
   begin
      --  Fill output buffer

      loop
         Net.Send (Client, Sample, Last);
         exit when not (Last in Sample'Range);
      end loop;

      --  provoke to timeout

      begin
         loop
            --  Loop because some data could be transferred from send buffer
            --  into receive buffer internally.

            Net.Send (Client, Single);
         end loop;
      exception
         when E : Net.Socket_Error =>
            --  Expected timeout exception message

            Text_IO.Put_Line (Exceptions.Exception_Message (E));
      end;

      begin
         loop
            Net.Receive (Peer, Buffer, Last);
            exit when Last < Buffer'Last;
         end loop;
      exception
         when Net.Socket_Error => null; -- Ignore input timeout
      end;
   end Transmit;

begin
   Text_IO.Put_Line ("Start.");

   Get_Free_Port (Free_Port);

   Net.Bind (Server, Free_Port, "localhost");
   Net.Listen (Server);

   if Security then
      --  Need 2 tasks for handshake

      declare
         task Connector;

         task body Connector is
         begin
            Net.Connect (Client, "localhost", Free_Port);
         end Connector;

      begin
         Net.Accept_Socket (Server, Peer);
      end;

   else
      Net.Connect (Client, "localhost", Free_Port);
      Net.Accept_Socket (Server, Peer);
   end if;

   Net.Set_Timeout (Client, 0.1);
   Net.Set_Timeout (Peer,   0.1);

   Transmit (0);
   Transmit (1);
   Transmit (-2);

   Net.Shutdown (Server);
   Net.Shutdown (Client);
   Net.Shutdown (Peer);

   Text_IO.Put_Line ("Done.");

exception
   when E : others =>
      Text_IO.Put_Line (Exceptions.Exception_Information (E));
end SockOver_Proc;
