------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2004-2008, AdaCore                     --
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

--  SOAP/WSDL test

with Ada.Text_IO;

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

   procedure WSDL_Demo_Client is
      use Ada;
   begin
      Text_IO.Put_Line
        (Integer'Image (datafeedService.Client.Ping (Value => 10)));
   end WSDL_Demo_Client;

   function Ping (Value : in Integer) return Integer;

   -------------
   -- SOAP_CB --
   -------------

   function SOAP_CB is new DatafeedService.Server.ping_CB (Ping);

   function SOAP_Wrapper is new SOAP.Utils.SOAP_Wrapper (SOAP_CB);

   --------
   -- CB --
   --------

   function CB (Request : in Status.Data) return Response.Data is
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

   function Ping (Value : in Integer) return Integer is
   begin
      return Value;
   end Ping;

begin
   Server.Start
     (H_Server, "WSDL demo",
      CB'Unrestricted_Access,
      Port => DatafeedService.Server.Port);

   WSDL_Demo_Client;

   Server.Shutdown (H_Server);
end Datafeed_Service;
