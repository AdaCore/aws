
--  $Id$

with Ada.Text_IO;      use Ada.Text_IO;
with Ada.Calendar;     use Ada.Calendar;
with SOAP.Types;       use SOAP.Types;
with SOAP.Parameters;  use SOAP.Parameters;
with SOAP.Message;
with SOAP.Message.Payload;
with SOAP.Message.XML;

procedure SOAP3 is

   CRLF : constant String := ASCII.CR & ASCII.LF;

   Payload1 : constant SOAP.Message.Payload.Object'Class :=
     SOAP.Message.Payload.Build
     ("Workorder",
      +B64 ("", "file"), "http://dummyns.org");

   Payload2 : constant SOAP.Message.Payload.Object'Class :=
     SOAP.Message.Payload.Build
     ("Workorder",
      +S ("", "string"));

   Img1 : constant String := SOAP.Message.XML.Image (Payload1);
   Img2 : constant String := SOAP.Message.XML.Image (Payload2);

   B_Payload1 : constant SOAP.Message.Payload.Object'Class
     := SOAP.Message.XML.Load_Payload (Img1);

   B_Payload2 : constant SOAP.Message.Payload.Object'Class
     := SOAP.Message.XML.Load_Payload (Img2);

begin
   Put_Line ("Ok, payloads parsed");
   New_Line;
   Put_Line (Img1);
   Put_Line (Img2);
end SOAP3;
