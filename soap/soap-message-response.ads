
with AWS.Response;

with SOAP.Message.Payload;
with SOAP.Parameters;

package SOAP.Message.Response is

   subtype Object is Message.Payload.Object;

   function Build (O : in Object) return AWS.Response.Data;

   function From (P : in Message.Payload.Object) return Object;
   --  Returns a Response object, initialized from a payload object.

   function Parameters
     (P : in Object'Class)
     return SOAP.Parameters.Set
     renames SOAP.Message.Payload.Parameters;

   procedure Set_Parameters
     (P     : in out Object'Class;
      P_Set : in     SOAP.Parameters.Set)
     renames SOAP.Message.Payload.Set_Parameters;

end SOAP.Message.Response;
