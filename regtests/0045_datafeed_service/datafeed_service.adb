------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2004-2012, AdaCore                     --
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

--  SOAP/WSDL test

with Ada.Text_IO;

with AWS.Config.Set;
with AWS.MIME;
with AWS.Response;
with AWS.Server.Log;
with AWS.Status;
with AWS.Utils;
with SOAP.Utils;

with DatafeedService.Client;
with DatafeedService.Server;

procedure Datafeed_Service is

   use AWS;

   H_Server : Server.HTTP;
   CNF      : Config.Object;

   procedure WSDL_Demo_Client is
      use Ada;
   begin
      Text_IO.Put_Line
        (Integer'Image (datafeedService.Client.Ping (Value => 10)));
   end WSDL_Demo_Client;

   function Ping (Value : Integer) return Integer;

   -------------
   -- SOAP_CB --
   -------------

   function SOAP_CB is new DatafeedService.Server.ping_CB (Ping);

   function SOAP_Wrapper is new SOAP.Utils.SOAP_Wrapper (SOAP_CB);

   --------
   -- CB --
   --------

   function CB (Request : Status.Data) return Response.Data is
      SOAPAction : constant String := Status.SOAPAction (Request);
   begin
      if SOAPAction = "" then
         return SOAP_Wrapper (Request);
      else
         return Response.Build (MIME.Text_HTML, "<p>Not a valid SOAP request");
      end if;
   end CB;

   ----------
   -- Ping --
   ----------

   function Ping (Value : Integer) return Integer is
   begin
      return Value;
   end Ping;

begin
   Config.Set.Server_Name     (CNF, "WSDL demo");
   Config.Set.Server_Port     (CNF, DatafeedService.Server.Port);
   Config.Set.Protocol_Family (CNF, "FAMILY_INET");

   Server.Start (H_Server, CB'Unrestricted_Access, CNF);

   WSDL_Demo_Client;

   Server.Shutdown (H_Server);
end Datafeed_Service;
