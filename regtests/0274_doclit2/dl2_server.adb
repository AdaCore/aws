------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                       Copyright (C) 2015, AdaCore                        --
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
with AWS.Response;
with AWS.Server.Log;
with AWS.Server.Status;

with testingservice.Server;
with testingservice.Types;

with Dl2_Client;
with Dl2_Server_CB;

procedure Dl2_Server is

   use Ada;
   use AWS;

   use testingservice.Server;
   use testingservice.Types;

   H_Server : AWS.Server.HTTP;
   H_Client : AWS.Client.HTTP_Connection;

begin
   AWS.Server.Start
     (H_Server, "getQueueStatus",
      Dl2_Server_CB.CB'Access,
      Port => testingservice.Server.Port);

   AWS.Server.Log.Start_Error (H_Server);

   AWS.Client.Create (H_Client, AWS.Server.Status.Local_URL (H_Server));

   Dl2_Client (H_Client);

   declare
      Exec_Mes : aliased String :=
        "<soapenv:Envelope "
        & "xmlns:soapenv=""http://schemas.xmlsoap.org/soap/envelope/"" "
        & "xmlns:abc=""http://aurn.here.org/abc"" "
        & "xmlns:spat=""http://aurn.here.org/spatial""><soapenv:Header/>"
        & "<soapenv:Body><abc:executeRequest>"
        & "<abc:executionTime>2015-05-27T11:01:39Z</abc:executionTime>"
        & "<abc:valueA><abc:latLong><spat:latitudeDegrees>12.20000000000000"
        & "</spat:latitudeDegrees><spat:longitudeDegrees>12.30000000000000"
        & "</spat:longitudeDegrees></abc:latLong><abc:field7>"
        & "100.00000000000000</abc:field7><abc:field8>100.00000000000000"
        & "</abc:field8><abc:field9>100</abc:field9>"
        & "<abc:field12>1.00000000000000</abc:field12><abc:field10>1"
        & "</abc:field10><abc:field11>1.00000000000000</abc:field11>"
        & "</abc:valueA></abc:executeRequest></soapenv:Body>"
        & "</soapenv:Envelope>";
      Resp : AWS.Response.Data;
   begin
      AWS.Client.SOAP_Post
        (H_Client, Resp, "http://aurn.here.org/abc/execute", Exec_Mes);
      Text_IO.Put_Line (Response.Message_Body (Resp));
   end;

   AWS.Server.Shutdown (H_Server);
end Dl2_Server;
