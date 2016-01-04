------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                      Copyright (C) 2016, AdaCore                         --
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

with SOAP.Dispatchers.Callback;

with LSO;
with LSO_Server;
with LSO_Service.Client;

procedure LSO_Main is

   use Ada;
   use AWS;

   WS   : Server.HTTP;

   H    : LSO_Server.Handler;

   Conf : Config.Object := Config.Get_Current;

   A, B : LSO.Set_Of_Int (1 .. 5000);

begin
   H := SOAP.Dispatchers.Callback.Create
     (LSO_Server.HTTP_CB'Access,
      LSO_Server.SOAP_CB'Access,
      Schema => LSO_Service.Schema);

   Config.Set.Server_Host (Conf, "localhost");
   Config.Set.Server_Port (Conf, 0);

   Server.Start (WS, H, Conf);

   for K in A'Range loop
      A (K) := K;
   end loop;

   B := LSO_Service.Client.Echo (A, Endpoint => Server.Status.Local_URL (WS));

   for K in B'Range loop
      if B (K) /= K then
         Text_IO.Put_Line ("wrong value!" & Positive'Image (K));
      end if;
   end loop;

   Server.Shutdown (WS);
end LSO_Main;
