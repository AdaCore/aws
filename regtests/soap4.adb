
--  $Id$

with Ada.Text_IO;      use Ada.Text_IO;
with Ada.Calendar;     use Ada.Calendar;
with SOAP.Types;       use SOAP.Types;
with SOAP.Parameters;  use SOAP.Parameters;
with SOAP.Message;
with SOAP.Message.Payload;
with SOAP.Message.XML;

procedure SOAP4 is

   Rec : SOAP_Record
     := R ((+S ("val1", "name1"), +S ("val2", "name2")), "rec");

   Rec2 : SOAP_Record
     := R ((+S ("val1", "name1"), +S ("val2", "name2")), "rec2");

   Arr : SOAP_Array
     := A ((+Rec, +Rec, +Rec), "arr");

   Arr2 : SOAP_Array
     := A ((+Rec, +Rec, +Rec2, +Rec), "arr2");

   Arr3 : SOAP_Array
     := A ((+Rec, +Rec, +S ("value", "rec"), +Rec), "arr3");

   Payload : SOAP.Message.Payload.Object'Class :=
     SOAP.Message.Payload.Build ("Arr_Rec", +Arr);

   Payload2 : SOAP.Message.Payload.Object'Class :=
     SOAP.Message.Payload.Build ("Arr_Rec2", +Arr2);

   Payload3 : SOAP.Message.Payload.Object'Class :=
     SOAP.Message.Payload.Build ("Arr_Rec3", +Arr3);

   Parms    : List := SOAP.Message.Parameters (Payload);
   Img      : String := SOAP.Message.XML.Image (Payload);

   Parms2   : List := SOAP.Message.Parameters (Payload2);
   Img2     : String := SOAP.Message.XML.Image (Payload2);

   Parms3   : List := SOAP.Message.Parameters (Payload3);
   Img3     : String := SOAP.Message.XML.Image (Payload3);

begin
   Put_Line (Img);
   New_Line;

   Put_Line (Img2);
   New_Line;

   Put_Line (Img3);
   New_Line;
end SOAP4;
