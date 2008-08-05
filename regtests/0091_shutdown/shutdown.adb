------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2003-2008, AdaCore                     --
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

with Ada.Text_IO;
with AWS.Server;

with Srv;
with Get_Free_Port;

procedure Shutdown is
   use Ada;
   use AWS;

   WS : Server.HTTP;
   Port1 : Natural := 1262;
   Port2 : Natural := 1263;

begin
   Text_IO.Put_Line ("start"); Text_IO.Flush;

   Get_Free_Port (Port1);
   Server.Start (Srv.WS, "demo", Srv.CB'Access, Port => Port1);

   Get_Free_Port (Port2);
   Server.Start (WS, "demo", Srv.CB'Access, Port => Port2);

   delay 2.0;

   Text_IO.Put_Line ("shutdown"); Text_IO.Flush;
   Server.Shutdown (Srv.WS);
   Server.Shutdown (WS);

   Text_IO.Put_Line ("wait..."); Text_IO.Flush;
   Server.Wait (Server.No_Server);
end Shutdown;
