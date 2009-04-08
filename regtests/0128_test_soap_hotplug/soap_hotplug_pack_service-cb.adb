
--  wsdl2aws SOAP Generator v1.4
--
--  AWS 2.1w - SOAP 1.3
--  This file was generated on Sunday 31 October 2004 at 10:58:27
--
--  $ wsdl2aws -f -cvs -cb -types soap_hotplug_pack.ads soap_hotplug_pack.ads.wsdl

with Ada.Exceptions;

with SOAP.Message.Response.Error;

with SOAP_hotplug_pack;

with SOAP_Hotplug_Pack_Service.Server;
with SOAP_Hotplug_Pack_Service.Types;

package body SOAP_Hotplug_Pack_Service.CB is

   use Ada.Exceptions;
   use SOAP;

   pragma Warnings (Off, SOAP_Hotplug_Pack_Service.Server);
   pragma Warnings (Off, SOAP_Hotplug_Pack_Service.Types);

   pragma Style_Checks (Off);

   function Job1_CB is
     new SOAP_Hotplug_Pack_Service.Server.Job1_CB (SOAP_hotplug_pack.Job1);

   function Job2_CB is
     new SOAP_Hotplug_Pack_Service.Server.Job2_CB (SOAP_hotplug_pack.Job2);

   -------------
   -- SOAP_CB --
   -------------

   function SOAP_CB
     (SOAPAction : String;
      Payload    : Message.Payload.Object;
      Request    : AWS.Status.Data)
      return Response.Data is
   begin
      if SOAPAction = "Job1" then
         return Job1_CB (SOAPAction, Payload, Request);

      elsif SOAPAction = "Job2" then
         return Job2_CB (SOAPAction, Payload, Request);

      else
         return Message.Response.Build
           (Message.Response.Error.Build
             (Message.Response.Error.Client,
              "Wrong SOAP action " & SOAPAction));
      end if;
   exception
      when E : others =>
         return Message.Response.Build
           (Message.Response.Error.Build
             (Message.Response.Error.Client,
              "Error in SOAP_CB for SOAPAction " & SOAPAction
                & " (" & Exception_Information (E) & ")"));
   end SOAP_CB;

   ---------------------
   -- SOAP_Hotplug_CB --
   ---------------------

   function SOAP_Hotplug_CB
     (SOAPAction : String;
      Payload    : Message.Payload.Object;
      Request    : AWS.Status.Data)
      return Response.Data
   is
      P : Message.Payload.Object := Payload;
   begin
      if SOAPAction = "Job1" then
         Message.Payload.Set_Procedure_Name (P, "Job2");
         return Job2_CB ("Job2", P, Request);

      elsif SOAPAction = "Job2" then
         Message.Payload.Set_Procedure_Name (P, "Job1");
         return Job1_CB ("Job1", P, Request);

      else
         return Message.Response.Build
           (Message.Response.Error.Build
             (Message.Response.Error.Client,
              "Wrong SOAP action " & SOAPAction));
      end if;
   exception
      when E : others =>
         return Message.Response.Build
           (Message.Response.Error.Build
             (Message.Response.Error.Client,
              "Error in SOAP_CB for SOAPAction " & SOAPAction
                & " (" & Exception_Information (E) & ")"));
   end SOAP_Hotplug_CB;

end SOAP_Hotplug_Pack_Service.CB;
