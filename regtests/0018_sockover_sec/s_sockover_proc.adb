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

procedure S_SockOver_Proc (Security : Boolean) is

   use AWS;
   use Ada;
   use Ada.Streams;

   One   : constant Stream_Element := 1;
   Two   : constant Stream_Element := 2;
   Three : constant Stream_Element := 3;

   Server : Net.Socket_Type'Class := Net.Socket (False);
   Peer   : Net.Socket_Type'Class := Net.Socket (Security);
   Client : Net.Socket_Type'Class := Net.Socket (Security);

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
         Client.Send (Sample, Last);
         exit when not (Last in Sample'Range);
      end loop;

      --  provoke to timeout

      begin
         loop
            --  Loop because some data could be transferred from send buffer
            --  into receive buffer internally.

            Client.Send (Single);
         end loop;
      exception
         when E : Net.Socket_Error =>
            --  Expected timeout exception message

            Text_IO.Put_Line (Exceptions.Exception_Message (E));
      end;

      begin
         loop
            Peer.Receive (Buffer, Last);
            exit when Last < Buffer'Last;
         end loop;
      exception
         when Net.Socket_Error => null; -- Ignore input timeout
      end;
   end Transmit;

begin
   Text_IO.Put_Line ("Start.");

   Server.Bind (0, "localhost");
   Server.Listen;

   if Security then
      --  Need 2 tasks for handshake

      declare
         task Connector;

         task body Connector is
         begin
            Client.Connect (Server.Get_Addr, Server.Get_Port);
         end Connector;

      begin
         Server.Accept_Socket (Peer);
      end;

   else
      Client.Connect (Server.Get_Addr, Server.Get_Port);
      Server.Accept_Socket (Peer);
   end if;

   Client.Set_Timeout (0.1);
   Peer.Set_Timeout (0.1);

   Transmit (0);
   Transmit (1);
   Transmit (-2);

   Server.Shutdown;
   Client.Shutdown;
   Peer.Shutdown;

   Text_IO.Put_Line ("Done.");

exception
   when E : others =>
      Text_IO.Put_Line (Exceptions.Exception_Information (E));
end S_SockOver_Proc;
