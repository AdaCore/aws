
with Ada.Strings.Unbounded;

with SOAP.Parameters;
with SOAP.Message.XML;
with SOAP.Message.Payload;
with SOAP.Types;

with AWS.Client;
with AWS.Response;
with AWS.URL;

package body SOAP.Client is

   use Ada.Strings.Unbounded;

   ----------
   -- Call --
   ----------

   function Call
     (URL        : in String;
      P          : in Message.Payload.Object;
      SOAPAction : in String         := Not_Specified)
     return Message.Response.Object'Class
   is

      procedure RPC_Call;
      --  Does the actual RPC over HTTP call.

      Message_Body : Unbounded_String;
      Response     : AWS.Response.Data;

      --------------
      -- RPC_Call --
      --------------

      procedure RPC_Call is
      begin
         if SOAPAction = Not_Specified then
            declare
               URL_Object : AWS.URL.Object := AWS.URL.Parse (URL);
            begin
               Response := AWS.Client.SOAP_Post
                 (URL,
                  To_String (Message_Body),
                  AWS.URL.URI (URL_Object));
            end;

         else
            Response := AWS.Client.SOAP_Post
              (URL,
               To_String (Message_Body),
               SOAPAction);
         end if;
      end RPC_Call;

   begin
      Message_Body := SOAP.Message.XML.Image (P);

      RPC_Call;

      return Message.Response.Object
        (Message.XML.Load_Response (AWS.Response.Message_Body (Response)));
   end Call;

end SOAP.Client;
