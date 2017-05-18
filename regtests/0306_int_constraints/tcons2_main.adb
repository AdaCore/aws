------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                       Copyright (C) 2017, AdaCore                        --
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
with AWS.Server.Status;
with AWS.Utils;

with SOAP.Dispatchers.Callback;

with TCONS2;
with TCONS2_Server;
with TCONS2_Service.Client;

procedure TCONS2_Main is

   use Ada;
   use AWS;
   use type TCONS2.T2;
   use type TCONS2.T3;

   WS   : Server.HTTP;

   H    : TCONS2_Server.Handler;

   Conf : Config.Object := Config.Get_Current;

begin
   H := SOAP.Dispatchers.Callback.Create
     (TCONS2_Server.HTTP_CB'Access,
      TCONS2_Server.SOAP_CB'Access,
      TCONS2_Service.Schema);

   Config.Set.Server_Host (Conf, "localhost");
   Config.Set.Server_Port (Conf, 0);

   Server.Start (WS, H, Conf);

   TCONS2_Service.Client.Print
     ((12, -13, -20, 1.1), Endpoint => Server.Status.Local_URL (WS));

   Server.Shutdown (WS);
end TCONS2_Main;
