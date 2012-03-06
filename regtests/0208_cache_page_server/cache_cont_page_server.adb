------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2010-2012, AdaCore                     --
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
with AWS.Config.Set;
with AWS.Messages;
with AWS.Response;
with AWS.Server.Status;
with AWS.Services.Page_Server;
with AWS.Utils;

procedure Cache_Cont_Page_Server is
   use Ada;
   use AWS;

   Conf : Config.Object;
   WS   : Server.HTTP;
   R    : Response.Data;

   CC   : constant Messages.Cache_Data :=
            (Messages.Response, Max_Age => 4,
             Proxy_Revalidate => True, others => <>);
begin
   Config.Set.WWW_Root (Conf, ".");
   Config.Set.Server_Host (Conf, "localhost");
   Config.Set.Server_Port (Conf, 0);

   Server.Start (WS, Services.Page_Server.Callback'Access, Conf);

   Services.Page_Server.Set_Cache_Control (CC);

   R := Client.Get (Server.Status.Local_URL (WS) & "/test.txt");

   Text_IO.Put_Line
     ("CC: " & String (Messages.Cache_Option'(Response.Cache_Control (R))));

   Server.Shutdown (WS);
end Cache_Cont_Page_Server;
