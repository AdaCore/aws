
--  wsdl2aws SOAP Generator v1.4
--
--  AWS 2.1w - SOAP 1.3
--  This file was generated on Sunday 31 October 2004 at 10:58:27
--
--  $ wsdl2aws -f -cvs -cb -types soap_hotplug_pack.ads soap_hotplug_pack.ads.wsdl

pragma Warnings (Off);

with Ada.Calendar;

with SOAP.Types;

with SOAP_Hotplug_Pack_Service.Types;

package SOAP_Hotplug_Pack_Service.Client is

   use SOAP_Hotplug_Pack_Service.Types;

   function Job1
     (X        : Integer;
      Y        : Integer;
      Endpoint : String := SOAP_Hotplug_Pack_Service.URL)
      return Integer;
   --  Raises SOAP.SOAP_Error if the procedure fails

   function Job2
     (X        : Integer;
      Y        : Integer;
      Endpoint : String := SOAP_Hotplug_Pack_Service.URL)
      return Integer;
   --  Raises SOAP.SOAP_Error if the procedure fails

end SOAP_Hotplug_Pack_Service.Client;
