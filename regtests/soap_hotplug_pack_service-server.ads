
--  wsdl2aws SOAP Generator v1.4
--
--  AWS 2.1w - SOAP 1.3
--  This file was generated on Sunday 31 October 2004 at 10:58:27
--
--  $ wsdl2aws -f -cvs -cb -types soap_hotplug_pack.ads soap_hotplug_pack.ads.wsdl

--  $Id$

pragma Warnings (Off);

with Ada.Calendar;

with AWS.Status;
with AWS.Response;

with SOAP.Message.Payload;
with SOAP.Types;

with SOAP_Hotplug_Pack_Service.Types;

package SOAP_Hotplug_Pack_Service.Server is

   use SOAP_Hotplug_Pack_Service.Types;

   Port : constant := 80;

   generic
      with function Job1
        (X        : in Integer;
         Y        : in Integer)
         return Integer;
   function Job1_CB
     (SOAPAction : in String;
      Payload    : in SOAP.Message.Payload.Object;
      Request    : in AWS.Status.Data)
      return AWS.Response.Data;

   generic
      with function Job2
        (X        : in Integer;
         Y        : in Integer)
         return Integer;
   function Job2_CB
     (SOAPAction : in String;
      Payload    : in SOAP.Message.Payload.Object;
      Request    : in AWS.Status.Data)
      return AWS.Response.Data;

end SOAP_Hotplug_Pack_Service.Server;
