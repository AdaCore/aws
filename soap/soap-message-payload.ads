
with Ada.Strings.Unbounded;

with SOAP.Parameters;

package SOAP.Message.Payload is

   use Ada.Strings.Unbounded;

   type Object is new Message.Object with private;

   function Image (P : in Object) return Unbounded_String;

   function Procedure_Name (P : in Object'Class) return String;
   function Parameters     (P : in Object'class) return SOAP.Parameters.Set;

   procedure Set_Procedure_Name (P : in out Object'Class; Name  : in String);

   procedure Set_Parameters
     (P     : in out Object'Class;
      P_Set : in     SOAP.Parameters.Set);

   procedure Set
     (P              : in out Object'Class;
      Procedure_Name : in     String;
      P_Set          : in     SOAP.Parameters.Set;
      Name_Space     : in     String              := "");

   function Build
     (Procedure_Name : in String;
      P_Set          : in SOAP.Parameters.Set;
      Name_Space     : in String              := "")
     return Object;

private

   type Object is new Message.Object with record
      P              : SOAP.Parameters.Set;
      Procedure_Name : Unbounded_String;
   end record;

end SOAP.Message.Payload;
