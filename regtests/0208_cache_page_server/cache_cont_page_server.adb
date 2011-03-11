------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2010-2011, AdaCore                     --
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

procedure Cache_Cont_Page_Server is
   use Ada;
   use AWS;

   Conf : Config.Object;
   WS   : Server.HTTP;
   R    : Response.Data;
   Port : Natural := 1946;

   CC   : constant Messages.Cache_Data :=
            (Messages.Response, Max_Age => 4,
             Proxy_Revalidate => True, others => <>);
begin
   Get_Free_Port (Port);

   Config.Set.WWW_Root (Conf, ".");
   Config.Set.Server_Port (Conf, Port);

   Server.Start (WS, Services.Page_Server.Callback'Access, Conf);

   Services.Page_Server.Set_Cache_Control (CC);

   R := Client.Get ("http://localhost:" & Utils.Image (Port) & "/test.txt");

   Text_IO.Put_Line
     ("CC: " & String (Messages.Cache_Option'(Response.Cache_Control (R))));

   Server.Shutdown (WS);
end Cache_Cont_Page_Server;
