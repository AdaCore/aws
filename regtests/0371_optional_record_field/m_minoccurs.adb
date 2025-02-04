
with Ada.Text_IO;
with Ada.Strings.Unbounded;

with Myservice;

with MyTestingSchema.MainElementOneType_Type_Pkg;

with SOAP.Parameters;
with SOAP.Message.Payload;
with SOAP.Message.Xml;
with SOAP.Types;

procedure M_MinOccurs is

   use Ada;
   use Ada.Strings.Unbounded;

   --  With the option FieldTree
   V_XML_With : aliased String :=
     "<?xml version=""1.0"" encoding=""utf-8""?>" &
     "<soap:Envelope xmlns:soap=""http://schemas.xmlsoap.org/soap/envelope/""><soap:Body>" &
     "<MainElementOne>"&
     "<SecondaryElementOne>" &
     "<FieldOne>One</FieldOne>" &
     "<FieldTwo>Two</FieldTwo>" &
     "<FieldThree>Three</FieldThree>" &
     "<FieldFour>Four</FieldFour>" &
     "<FieldFive>2024-04-12T08:30:00Z</FieldFive>" &
     "</SecondaryElementOne>"&
     "<SecondaryElementTwo>" &
     "<FieldOne>One-2</FieldOne>" &
     "<FieldTwo>Two-2</FieldTwo>" &
     "<FieldThree>Three-2</FieldThree>" &
     "<FieldFour>Four-2</FieldFour>" &
     "<FieldFive>2024-04-12T08:32:00Z</FieldFive>" &
     "</SecondaryElementTwo>"&
     "</MainElementOne>" &
     "</soap:Body></soap:Envelope>";

   --  Without the option FieldTree
   V_XML_Without : aliased String :=
     "<?xml version=""1.0"" encoding=""utf-8""?>" &
     "<soap:Envelope xmlns:soap=""http://schemas.xmlsoap.org/soap/envelope/""><soap:Body>" &
     "<MainElementOne>"&
     "<SecondaryElementOne>" &
     "<FieldOne>One</FieldOne>" &
     "<FieldTwo>Two</FieldTwo>" &
     "<FieldFour>Four</FieldFour>" &
     "<FieldFive>2024-04-12T08:30:00Z</FieldFive>" &
     "</SecondaryElementOne>"&
     "<SecondaryElementTwo>" &
     "<FieldOne>One-2</FieldOne>" &
     "<FieldTwo>Two-2</FieldTwo>" &
     "<FieldFour>Four-2</FieldFour>" &
     "<FieldFive>2024-04-12T08:32:00Z</FieldFive>" &
     "</SecondaryElementTwo>"&
     "</MainElementOne>" &
     "</soap:Body></soap:Envelope>";

   V_MainElement : MyTestingSchema.MainElementOneType_Type_Pkg.MainElementOneType_Type;

begin
   V_MainElement :=
     MyTestingSchema.MainElementOneType_Type_Pkg.To_MainElementOneType_Type
       (SOAP.Types.SOAP_Record'
          (SOAP.Parameters.Get
             (P    => SOAP.Message.Parameters
                (SOAP.Message.XML.Load_Payload
                   (XML      => V_XML_With,
                    Envelope => True,
                    Schema   => MyService.Schema)),
              Name => "MainElementOne")));

   Text_IO.Put_Line ("=== with ");
   Text_IO.Put_Line ("One: " & V_MainElement.SecondaryElementOne'Image);
   Text_IO.Put_Line ("Two: " & V_MainElement.SecondaryElementTwo'Image);

   V_MainElement :=
     MyTestingSchema.MainElementOneType_Type_Pkg.To_MainElementOneType_Type
       (SOAP.Types.SOAP_Record'
          (SOAP.Parameters.Get
             (P    => SOAP.Message.Parameters
                (SOAP.Message.XML.Load_Payload
                   (XML      => V_XML_Without,
                    Envelope => True,
                    Schema   => MyService.Schema)),
              Name => "MainElementOne")));

   Text_IO.Put_Line ("=== without ");
   Text_IO.Put_Line ("One: " & V_MainElement.SecondaryElementOne'Image);
   Text_IO.Put_Line ("Two: " & V_MainElement.SecondaryElementTwo'Image);
end M_MinOccurs;
