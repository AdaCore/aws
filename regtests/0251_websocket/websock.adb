------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                        Copyright (C) 2014, AdaCore                       --
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

with Ada.Text_IO;
with AWS.Config.Set;
with AWS.Net.WebSocket.Registry.Control;
with AWS.Server.Status;

with WebSock_CB;

procedure WebSock is

   use Ada;
   use AWS;
   use AWS.Config;
   use type AWS.Net.Socket_Access;

   Rcp : constant Net.WebSocket.Registry.Recipient :=
          Net.WebSocket.Registry.Create (URI => "/echo");
   --  The recipient targets all clients (any Origin) whose URI is /echo

   M      : String (1 .. 255);
   WS     : Server.HTTP;
   Config : AWS.Config.Object;
begin
   AWS.Config.Set.Reuse_Address (Config, True);
   AWS.Config.Set.Server_Host (Config, "127.0.0.1");
   AWS.Config.Set.Server_Port (Config, 0);

   Server.Start
     (WS,
      Config   => Config,
      Callback => WebSock_CB.HW_CB'Access);

   --  Start the WebSocket server, this is needed only to receive message
   --  from the WebClient. It is always possible to send messages.

   Net.WebSocket.Registry.Control.Start;

   Net.WebSocket.Registry.Register ("/echo", WebSock_CB.Create'Access);

   for K in M'Range loop
      M (K) := Character'Val ((Character'Pos ('0') + K mod 10));
   end loop;

   Text_IO.Put_Line ("PORT: " & Positive'Image (Server.Status.Port (WS)));

   --  Wait for at least a WebSocket to be created, no need to send a
   --  message into the void.

   WebSock_CB.Wait.Start;

   --  First send a large message (message with length > 125, see RFC 6455)

   Net.WebSocket.Registry.Send (Rcp, "Server large message " & M);

   delay 0.01;

   --  Then send some messages

   for K in 1 .. 5 loop
      Net.WebSocket.Registry.Send (Rcp, "Server short message " & K'Img);
      delay 0.01;
   end loop;

   WebSock_CB.Wait.Stop;

   Server.Shutdown (WS);
end WebSock;
