
--  $Id$

with SOAP.Types; use SOAP.Types;

package Tc_Soap_Names is

   Soap_Ws        : constant String := "Workstation";
   Soap_Updated   : constant String := "Updated";
   Soap_Wo        : constant String := "Workorder";
   Soap_WoId      : constant String := "Woid";
   Soap_State     : constant String := "State";
   Soap_Package   : constant String := "Package";
   Soap_Recipe    : constant String := "Recipe";
   Soap_Orderline : constant String := "Orderline";
   Soap_TwelveNc  : constant String := "Twelvenc";
   Soap_Type      : constant String := "Type";
   Soap_Priority  : constant String := "Priority";
   Soap_Label     : constant String := "Label";
   Soap_Quantity  : constant String := "Quantity";
   Soap_Packing   : constant String := "Packing";
   Soap_Marking   : constant String := "Marking";
   Soap_Orient    : constant String := "Orientation";
   Soap_Produced  : constant String := "Produced";

   function Soap_Image
     (Obj    : in SOAP.Types.Object'Class;
      Indent : in Integer                 := 0)
     return String;

end Tc_Soap_Names;
