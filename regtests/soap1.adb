
with Ada.Text_IO;

with SOAP.Message.Response;
with SOAP.Message.XML;

procedure SOAP1 is

   use Ada;

   Mess : constant String :=
     "<?xml version=""1.0"" encoding=""UTF-8"" standalone=""no"" ?>"
     & "<SOAP-ENV:Envelope "
     & "xmlns:SOAP-ENV=""http://schemas.xmlsoap.org/soap/envelope/"">"
     & "<SOAP-ENV:Body>"
     & "<m:MultiplyResponse xmlns:m=""http://tempuri.org/message/"">"
     & "<Result>12 13</Result>"
     & "</m:MultiplyResponse>"
     & "</SOAP-ENV:Body>"
     & "</SOAP-ENV:Envelope>";

   Resp : constant SOAP.Message.Response.Object'Class
     := SOAP.Message.XML.Load_Response (Mess);

begin
   Text_IO.Put_Line (SOAP.Message.XML.Image (Resp));
end SOAP1;
