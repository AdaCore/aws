
--  wsdl2aws SOAP Generator v1.4
--
--  AWS 2.1w - SOAP 1.3
--  This file was generated on Sunday 31 October 2004 at 10:58:27
--
--  $ wsdl2aws -f -cvs -cb -types soap_hotplug_pack.ads soap_hotplug_pack.ads.wsdl

pragma Warnings (Off);

with Ada.Exceptions;

with SOAP.Client;
with SOAP.Message.Payload;
with SOAP.Message.Response;
with SOAP.Name_Space;
with SOAP.Parameters;
with SOAP.Utils;

package body SOAP_Hotplug_Pack_Service.Client is

   use SOAP.Types;
   use type SOAP.Parameters.List;

   pragma Style_Checks (Off);

   ----------
   -- Job1 --
   ----------

   function Job1
     (X        : Integer;
      Y        : Integer;
      Endpoint : String := SOAP_Hotplug_Pack_Service.URL)
      return Integer
   is
      P_Set   : SOAP.Parameters.List;
      Payload : SOAP.Message.Payload.Object;
   begin
      --  Set parameters
      P_Set := +SOAP.Types.I (X, "X")
         & SOAP.Types.I (Y, "Y");
      Payload := SOAP.Message.Payload.Build
        ("Job1", P_Set,
         SOAP.Name_Space.Create ("awsns", "urn:aws:SOAP_Hotplug_Pack"));

      declare
         Response : constant SOAP.Message.Response.Object'Class
           := SOAP.Client.Call
                (Endpoint, Payload, "Job1");
         R_Param  : constant SOAP.Parameters.List
           := SOAP.Message.Parameters (Response);
      begin
         if SOAP.Message.Response.Is_Error (Response) then
            Ada.Exceptions.Raise_Exception
              (SOAP.SOAP_Error'Identity,
               SOAP.Parameters.Get (R_Param, "faultstring"));
         else
            declare
               Result : constant Integer
                 := SOAP.Parameters.Get (R_Param, "Result");
            begin
               return Result;
            end;
         end if;
      end;
   end Job1;

   ----------
   -- Job2 --
   ----------

   function Job2
     (X        : Integer;
      Y        : Integer;
      Endpoint : String := SOAP_Hotplug_Pack_Service.URL)
      return Integer
   is
      P_Set   : SOAP.Parameters.List;
      Payload : SOAP.Message.Payload.Object;
   begin
      --  Set parameters
      P_Set := +SOAP.Types.I (X, "X")
         & SOAP.Types.I (Y, "Y");
      Payload := SOAP.Message.Payload.Build
        ("Job2", P_Set,
         SOAP.Name_Space.Create ("awsns", "urn:aws:SOAP_Hotplug_Pack"));

      declare
         Response : constant SOAP.Message.Response.Object'Class
           := SOAP.Client.Call
                (Endpoint, Payload, "Job2");
         R_Param  : constant SOAP.Parameters.List
           := SOAP.Message.Parameters (Response);
      begin
         if SOAP.Message.Response.Is_Error (Response) then
            Ada.Exceptions.Raise_Exception
              (SOAP.SOAP_Error'Identity,
               SOAP.Parameters.Get (R_Param, "faultstring"));
         else
            declare
               Result : constant Integer
                 := SOAP.Parameters.Get (R_Param, "Result");
            begin
               return Result;
            end;
         end if;
      end;
   end Job2;

end SOAP_Hotplug_Pack_Service.Client;
