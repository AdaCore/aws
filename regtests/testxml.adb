
--  Test from Wiljan Derks. It tests if AWS/SOAP does handle null string.
--  $Id$

with Ada.Text_IO;
with SOAP.Message.Payload;
with SOAP.Types; use SOAP.Types;
with SOAP.Parameters; use SOAP.Parameters;
with SOAP.Message.XML; use SOAP.Message.XML;

procedure TestXML is

   Soap_Wo        : constant String := "Workorder";
   Soap_WoId      : constant String := "Woid";
   Soap_Orderline : constant String := "Orderline";
   Soap_Produced  : constant String := "Produced";

   Parm : List :=
            +R ((+S ("Wo1", Soap_WoId),
                 +A ((1 => +R ((+S ("", Soap_WoId),
                                +I (0, Soap_Produced)
                              ), Soap_Orderline)
                     )
                    , Soap_Orderline)
                )
               , Soap_Wo);

   Payload : SOAP.Message.Payload.Object'Class :=
      SOAP.Message.Payload.Build ("Test", Parm);
   Pimg : String := Image (Payload);

begin
   Ada.Text_IO.Put_Line ("Pimg : " & Pimg);
   declare
      P2 : SOAP.Message.Payload.Object'Class := Load_Payload (Pimg);

      P  : SOAP.Parameters.List;
   begin
      P := SOAP.Message.Parameters (P2);
   end;
end TestXML;
