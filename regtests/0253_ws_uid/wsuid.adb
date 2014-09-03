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
with AWS.Status;

with WSUID_CB;

procedure WSUID is

   use Ada;
   use AWS;
   use AWS.Config;
   use type AWS.Net.Socket_Access;

   WS     : Server.HTTP;
   Config : AWS.Config.Object;
begin
   AWS.Config.Set.Reuse_Address (Config, True);
   AWS.Config.Set.Server_Host (Config, "127.0.0.1");
   AWS.Config.Set.Server_Port (Config, 0);

   Server.Start
     (WS,
      Config   => Config,
      Callback => WSUID_CB.HW_CB'Access);

   --  Start the WebSocket server, this is needed only to receive message
   --  from the WebClient. It is always possible to send messages.

   Net.WebSocket.Registry.Control.Start;

   Net.WebSocket.Registry.Register ("/echo", WSUID_CB.Create'Access);

   Text_IO.Put_Line ("PORT: " & Positive'Image (Server.Status.Port (WS)));

   --  Wait for at least a WebSocket to be created, no need to send a
   --  message into the void.

   while not WSUID_CB.Created loop
      delay 1.0;
   end loop;

   --  Now that a WebSocket is created send a message to its UID

   declare
      Rcp : Net.WebSocket.Registry.Recipient :=
              Net.WebSocket.Registry.Create (Id => WSUID_CB.UID_Store);
   begin
      Net.WebSocket.Registry.Send (Rcp, "A simple message!");
   end;

   while WSUID_CB.Created loop
      delay 0.2;
   end loop;

   Server.Shutdown (WS);
end WSUID;
