
--  $Id$

with Ada.Text_IO;      use Ada.Text_IO;
with Ada.Calendar;     use Ada.Calendar;
with SOAP.Types;       use SOAP.Types;
with SOAP.Parameters;  use SOAP.Parameters;
with SOAP.Message;
with SOAP.Message.Payload;
with SOAP.Message.XML;

procedure SOAP2 is

   Payload : SOAP.Message.Payload.Object'Class :=
     SOAP.Message.Payload.Build
     ("Workorder",
      +R ((+S ("Wo1", "name1"),
           +S ("idle", "name2"),
           +A ((1 => +R ((+S ("idle", "name3"),
                          +S ("Wo1", "name4")),
                         "my_record")),
               "my_array")),
          "my_record_out"));

   Parms    : List := SOAP.Message.Parameters (Payload);
   Obj      : Object'Class := Argument (Parms, "my_record_out");
   Img      : String := SOAP.Message.XML.Image (Payload);
   Payload2 : SOAP.Message.Payload.Object'Class
     := SOAP.Message.XML.Load_Payload (Img);

   Parms2   : List := SOAP.Message.Parameters (Payload2);
   Obj2     : Object'Class := Argument (Parms2, "my_record_out");
   Img2     : String := SOAP.Message.XML.Image (Payload2);

begin
   Put_Line (Img);
   New_Line;

   if Img = Img2 then
      Put_Line ("Payload coding/decoding is ok");
   else
      Put_Line ("Payload coding/decoding failed");
      Put_Line (Img2);
      Put_Line (XML_Image (Obj));
      Put_Line (XML_Image (Obj2));
   end if;
end SOAP2;
