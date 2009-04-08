
--  wsdl2aws SOAP Generator v1.4
--
--  AWS 2.1w - SOAP 1.3
--  This file was generated on Sunday 31 October 2004 at 10:58:27
--
--  $ wsdl2aws -f -cvs -cb -types soap_hotplug_pack.ads soap_hotplug_pack.ads.wsdl

with Ada.Calendar;
with Ada.Strings.Unbounded;

with SOAP.Types;
with SOAP.Utils;

with SOAP_hotplug_pack;

package SOAP_Hotplug_Pack_Service.Types is

   pragma Warnings (Off, Ada.Calendar);
   pragma Warnings (Off, Ada.Strings.Unbounded);
   pragma Warnings (Off, SOAP.Types);
   pragma Warnings (Off, SOAP.Utils);
   pragma Warnings (Off, soap_hotplug_pack);


   pragma Style_Checks (Off);

   pragma Elaborate_Body;

   use Ada.Strings.Unbounded;

    function "+"
      (Str : String)
       return Unbounded_String
       renames To_Unbounded_String;

end SOAP_Hotplug_Pack_Service.Types;
