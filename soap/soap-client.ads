
with SOAP.Message.Payload;
with SOAP.Message.Response;

package SOAP.Client is

   Not_Specified : constant String;

   function Call
     (URL        : in String;
      P          : in Message.Payload.Object;
      SOAPAction : in String         := Not_Specified)
     return Message.Response.Object'Class;

private

   Not_Specified : constant String := "";

end SOAP.Client;
