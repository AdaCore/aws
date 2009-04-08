------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2006-2009, AdaCore                     --
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
pragma Elaborate_All (AWS.Config.Set);
with AWS.Server;
pragma Elaborate_All (AWS.Server);
with AWS.Status;
pragma Elaborate_All (AWS.Status);
with AWS.Response;
pragma Elaborate_All (AWS.Response);
with SOAP.Dispatchers.Callback;
pragma Elaborate_All (SOAP.Dispatchers.Callback);

with WSDL_9_Service.CB;
pragma Elaborate_All (WSDL_9_Service.CB);
with WSDL_9_Service.Server;
pragma Elaborate_All (WSDL_9_Service.Server);

procedure WSDL_9_Main is

   use Ada;
   use AWS;

   URL  : constant String := WSDL_9_Service.URL;
   CRLF : constant String := ASCII.CR & ASCII.LF;

   function CB (Request : Status.Data) return Response.Data is
      R : Response.Data;
   begin
      return R;
   end CB;

   WS   : AWS.Server.HTTP;
   Conf : Config.Object;
   Disp : WSDL_9_Service.CB.Handler;

   R    : AWS.Response.Data;

begin
   Config.Set.Server_Port
      (Conf, WSDL_9_Service.Server.Port);
   Disp := SOAP.Dispatchers.Callback.Create
     (CB'Unrestricted_Access,
      WSDL_9_Service.CB.SOAP_CB'Access);

   AWS.Server.Start (WS, Disp, Conf);

   R := AWS.Client.SOAP_Post
     (URL, "<?xml version=""1.0"" encoding=""UTF-8"" ?>" & CRLF & "<SOAP-ENV:Envelope xmlns:SOAP-ENV=""http://schemas.xmlsoap.org/soap/envelope/"" xmlns:SOAP-ENC=""http://schemas.xmlsoap.org/soap/encoding/"" xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance"" xmlns:xsd=""http://www.w3.org/2001/XMLSchema"" xmlns:ns2=""http://soapaws/MG_SOAP_pkg/"" xmlns:ns1=""http://soapaws/MG_SOAP_def/""><SOAP-ENV:Body SOAP-ENV:encodingStyle=""http://schemas.xmlsoap.org/soap/encoding/""><ns1:Send_SMS><User>test</User><Password>test</Password><Mobile>0626985825</Mobile><Content>Test ns1</Content></ns1:Send_SMS></SOAP-ENV:Body></SOAP-ENV:Envelope>", "Send_SMS");

   Text_IO.Put_Line (Response.Message_Body (R));

   R := AWS.Client.SOAP_Post
     (URL, "<?xml version=""1.0"" encoding=""UTF-8"" ?>" & CRLF & "<SOAP-ENV:Envelope xmlns:SOAP-ENV=""http://schemas.xmlsoap.org/soap/envelope/"" xmlns:SOAP-ENC=""http://schemas.xmlsoap.org/soap/encoding/"" xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance"" xmlns:xsd=""http://www.w3.org/2001/XMLSchema"" xmlns:ns2=""http://soapaws/MG_SOAP_pkg/"" xmlns:ns1=""http://soapaws/MG_SOAP_def/""><SOAP-ENV:Body SOAP-ENV:encodingStyle=""http://schemas.xmlsoap.org/soap/encoding/""><ns1:Send_SMS><User href=""#_1""/><Password href=""#_1""/><Mobile>0626985825</Mobile><Content>Test ns1</Content></ns1:Send_SMS><multiRef id=""_1"" xsi:type=""xsd:string"">test</multiRef></SOAP-ENV:Body></SOAP-ENV:Envelope>", "Send_SMS");

   Text_IO.Put_Line (Response.Message_Body (R));

   R := AWS.Client.SOAP_Post
     (URL, "<?xml version=""1.0"" encoding=""UTF-8"" ?>" & CRLF & "<SOAP-ENV:Envelope xmlns:SOAP-ENV=""http://schemas.xmlsoap.org/soap/envelope/"" xmlns:SOAP-ENC=""http://schemas.xmlsoap.org/soap/encoding/"" xmlns:xsi=""http://www.w3.org/2001/XMLSchema-instance"" xmlns:xsd=""http://www.w3.org/2001/XMLSchema"" xmlns:ns2=""http://soapaws/MG_SOAP_pkg/"" xmlns:ns1=""http://soapaws/MG_SOAP_def/""><SOAP-ENV:Body SOAP-ENV:encodingStyle=""http://schemas.xmlsoap.org/soap/encoding/""><ns1:Send_SMS><User href=""#_1""/><Password href=""#_1""/><Mobile>0626985825</Mobile><Content>Test ns1</Content></ns1:Send_SMS><id id=""_1"" xsi:type=""xsd:string"">test</id></SOAP-ENV:Body></SOAP-ENV:Envelope>", "Send_SMS");

   Text_IO.Put_Line (Response.Message_Body (R));

   Server.Shutdown (WS);
end WSDL_9_Main;
