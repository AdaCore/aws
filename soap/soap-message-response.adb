
with AWS.MIME;

with SOAP.Message.XML;

package body SOAP.Message.Response is

   -----------
   -- Build --
   -----------

   function Build (O : in Object) return AWS.Response.Data is
   begin
      return AWS.Response.Build
        (AWS.MIME.Text_XML, String'(SOAP.Message.XML.Image (O)));
   end Build;

   ----------
   -- From --
   ----------

   function From (P : in Message.Payload.Object) return Object is
      NP : Object := P;
   begin
      Message.Payload.Set_Procedure_Name
        (NP, Message.Payload.Procedure_Name (P) & "Response");
      return NP;
   end From;

end SOAP.Message.Response;
