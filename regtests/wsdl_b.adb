
package body WSDL_B is

   function Echo_Complex_Rec (C_Rec : in Complex_Rec) return Complex_Rec is
   begin
      return C_Rec;
   end Echo_Complex_Rec;

end WSDL_B;
