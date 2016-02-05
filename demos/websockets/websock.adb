------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2012-2016, AdaCore                     --
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

--  A simple WebSocket demo using AWS framework

with Ada.Text_IO;

with AWS.Config.Set;
with AWS.Default;
with AWS.Net.Log;
with AWS.Net.WebSocket.Registry.Control;
with AWS.Server;
with AWS.Status;
with AWS.Templates;

with WebSock_CB;

procedure WebSock is

   use Ada;
   use AWS;
   use AWS.Config;
   use type AWS.Net.Socket_Access;

   Rcp : Net.WebSocket.Registry.Recipient :=
          Net.WebSocket.Registry.Create (URI => "/echo");
   --  The recipient targets all clients (any Origin) whose URI is /echo

   M   : String (1 .. 255);
   L   : Natural;

   WS     : Server.HTTP;
   Config : AWS.Config.Object;

begin
   AWS.Config.Set.Reuse_Address (Config, True);

   Text_IO.Put_Line
     ("Call me on port" & Positive'Image (AWS.Default.Server_Port));

   --  To analyse the send/received data uncomment the line below
   --  Net.Log.Start (WebSock_CB.W_Log'Access);

   --  Start the WebSocket server, this is needed only to receive message
   --  from the WebClient. It is always possible to send messages.

   Net.WebSocket.Registry.Control.Start;

   Net.WebSocket.Registry.Register ("/echo", WebSock_CB.Create'Access);

   --  Now we can safely start the server

   Server.Start
     (WS,
      Config   => Config,
      Callback => WebSock_CB.HW_CB'Access);

   for K in M'Range loop
      M (K) := Character'Val ((Character'Pos ('0') + K mod 10));
   end loop;

   --  Wait for at least a WebSocket to be created, no need to send a
   --  message into the void.

   while not WebSock_CB.Created loop
      delay 1.0;
   end loop;

   --  First send a large message (message with length > 125, see RFC 6455)

   Net.WebSocket.Registry.Send (Rcp, "Large message:" & M & ':');

   delay 1.0;

   --  Then send some messages

   for K in 1 .. 5 loop
      Net.WebSocket.Registry.Send (Rcp, "My reply " & K'Img);
      delay 1.0;
   end loop;

   --  Get a message from user

   Text_IO.Put ("Enter a message: ");
   Text_IO.Get_Line (M, L);

   --  Finally return an XML actions document. The format of this document
   --  is identical to the one used by the Ajax framework.

   Net.WebSocket.Registry.Send
     (Rcp,
      String'(Templates.Parse
        ("resp.xml", (1 => Templates.ASSOC ("MESSAGE", M (M'First .. L))))));

   Text_IO.Put_Line ("You can now press Q to exit.");

   Server.Wait (Server.Q_Key_Pressed);

   --  Now shuthdown the servers (HTTP and WebClient)

   Server.Shutdown (WS);
end WebSock;
