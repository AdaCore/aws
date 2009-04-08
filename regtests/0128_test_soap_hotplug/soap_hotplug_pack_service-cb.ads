
--  wsdl2aws SOAP Generator v1.4
--
--  AWS 2.1w - SOAP 1.3
--  This file was generated on Sunday 31 October 2004 at 10:58:27
--
--  $ wsdl2aws -f -cvs -cb -types soap_hotplug_pack.ads soap_hotplug_pack.ads.wsdl

with AWS.Response;
with AWS.Status;

with SOAP.Dispatchers.Callback;
with SOAP.Message.Payload;

package SOAP_Hotplug_Pack_Service.CB is

   use AWS;
   use SOAP;

   pragma Style_Checks (Off);

   subtype Handler is SOAP.Dispatchers.Callback.Handler;

   function SOAP_CB
     (SOAPAction : String;
      Payload    : Message.Payload.Object;
      Request    : AWS.Status.Data)
      return Response.Data;

   function SOAP_Hotplug_CB
     (SOAPAction : String;
      Payload    : Message.Payload.Object;
      Request    : AWS.Status.Data)
      return Response.Data;
   --  Use by the hotplug mode, invert call to Job1 and Job2

end SOAP_Hotplug_Pack_Service.CB;
