
with Ada.Strings.Unbounded;

with SOAP.Parameters;

package SOAP.Message is

   use Ada.Strings.Unbounded;

   type Object is tagged private;

   function Image (P : in Object) return Unbounded_String;

   function Name_Space (P : in Object'Class) return String;

   procedure Set_Name_Space (P : in out Object'Class; Name  : in String);

private

   type Object is tagged record
      Name_Space : Unbounded_String;
   end record;

end SOAP.Message;
