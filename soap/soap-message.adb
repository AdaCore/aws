
package body SOAP.Message is

   -----------
   -- Image --
   -----------

   function Image (P : in Object) return Unbounded_String is
   begin
      return To_Unbounded_String ("???? PROBLEM ???");
   end Image;

   ----------------
   -- Name_Space --
   ----------------

   function Name_Space (P : in Object'Class) return String is
   begin
      return To_String (P.Name_Space);
   end Name_Space;

   --------------------
   -- Set_Name_Space --
   --------------------

   procedure Set_Name_Space (P : in out Object'Class; Name  : in String) is
   begin
      P.Name_Space := To_Unbounded_String (Name);
   end Set_Name_Space;

end SOAP.Message;

