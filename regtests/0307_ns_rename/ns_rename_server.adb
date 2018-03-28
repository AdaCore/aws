------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                    Copyright (C) 2017-2018, AdaCore                      --
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
with AWS.Server.Status;

with api_service.Server;
with api_service.Types;

with NS_Rename_Client;
with NS_Rename_Server_CB;

procedure NS_Rename_Server is

   use Ada;
   use AWS;

   use api_service.Server;
   use api_service.Types;

   H_Server : AWS.Server.HTTP;
   H_Client : AWS.Client.HTTP_Connection;

begin
   AWS.Server.Start
     (H_Server, "NS_Rename",
      NS_Rename_Server_CB.CB'Access,
      Port => 0);

   Text_IO.Put_Line ("from client:");
   AWS.Client.Create (H_Client, AWS.Server.Status.Local_URL (H_Server));
   NS_Rename_Client (H_Client);

   declare
      Exec_Mes : aliased String :=
        "<?xml version='1.0' encoding='UTF-8'?>"
        & "<soapenv:Envelope "
        & "soapenv:encodingStyle=""http://schemas.xmlsoap.org/soap/encoding/"""
        & " xmlns:soapenc=""http://schemas.xmlsoap.org/soap/encoding/"""
        & " xmlns:soapenv=""http://schemas.xmlsoap.org/soap/envelope/"""
        & " xmlns:xsd=""http://www.w3.org/2001/XMLSchema"""
        & " xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance"">"
        & "<soapenv:Body"
        & " xmlns:tns=""http://soapaws/API_def/"""
        & " xmlns:n0=""http://soapaws/API_pkg/"">"
        & "<n0:Parameters_Call>"
        & "   <n0:P>"
        & "      <n0:A>9</n0:A>"
        & "      <n0:B>7</n0:B>"
        & "   </n0:P>"
        & "</n0:Parameters_Call>"
        & "</soapenv:Body>"
        & "</soapenv:Envelope>";
      Resp : AWS.Response.Data;
   begin
      Text_IO.Put_Line ("from http/post:");
      AWS.Client.SOAP_Post (H_Client, Resp, "Call", Exec_Mes);
   end;

   AWS.Server.Shutdown (H_Server);
end NS_Rename_Server;
