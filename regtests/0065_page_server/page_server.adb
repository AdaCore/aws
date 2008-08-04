------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2007-2008, AdaCore                     --
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

with AWS.Client;
with AWS.Config.Set;
with AWS.Messages;
with AWS.Response;
with AWS.Server;
with AWS.Services.Page_Server;
with AWS.Utils;

with Get_Free_Port;

procedure Page_Server is
   use Ada;
   use AWS;
   use type Messages.Status_Code;

   Conf : Config.Object;
   WS   : Server.HTTP;
   R    : Response.Data;
   Port : Natural := 1946;

begin
   Get_Free_Port (Port);

   Config.Set.WWW_Root (Conf, "icons");
   Config.Set.Server_Port (Conf, Port);

   Server.Start (WS, Services.Page_Server.Callback'Access, Conf);

   R := Client.Get ("http://localhost:" & Utils.Image (Port) & "/sound1.gif");

   if Response.Status_Code (R) = Messages.S200  then
      Text_IO.Put_Line ("OK for sound1.gif");
   else
      Text_IO.Put_Line ("NOK for sound1.gif");
   end if;

   R := Client.Get ("http://localhost:" & Utils.Image (Port) & "/nothere.gif");

   if Response.Status_Code (R) = Messages.S404  then
      Text_IO.Put_Line ("OK for nothere.gif");
   else
      Text_IO.Put_Line ("NOK for nothere.gif");
   end if;

   Server.Shutdown (WS);
end Page_Server;
