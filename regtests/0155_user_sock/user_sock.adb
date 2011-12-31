------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2006-2012, AdaCore                     --
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

with AWS.Client;
with AWS.MIME;
with AWS.Server;
with AWS.Utils;

with USock;
with Get_Free_Port;

procedure User_Sock is

   use Ada;
   use AWS;

   WS   : Server.HTTP;
   Port : Natural := 1234;

begin
   Get_Free_Port (Port);

   Server.Set_Socket_Constructor (WS, USock.Socket'Access);

   Server.Start
     (WS, "User Socket", USock.CB'Access, Port => Port);
   Text_IO.Put_Line ("started"); Ada.Text_IO.Flush;

   Server.Shutdown (WS);
   Text_IO.Put_Line ("shutdown");
end User_Sock;
