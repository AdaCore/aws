------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                            Copyright (C) 2000                            --
--                      Dmitriy Anisimov - Pascal Obry                      --
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

--  This is a way to build a simple HTML page server. Just a part of a full
--  Web server.

with Ada.Text_IO;

with AWS.Server;
with WS_CB;

procedure WS is

   use Ada;

   WS : AWS.Server.HTTP (5);

begin
   Text_IO.Put_Line ("AWS " & AWS.Version);
   Text_IO.Put_Line ("Kill me when you want me to stop...");
   Text_IO.Put_Line ("I will stop in 5 minutes anyway !");

   AWS.Server.Start (WS, "WS Demo",
                     Port     => 1234,
                     Callback => WS_CB.Service'Access);

   delay 5 * 60.0;

   Text_IO.Put_Line ("ok, let's shutdown...");
   AWS.Server.Shutdown (WS);
end WS;
