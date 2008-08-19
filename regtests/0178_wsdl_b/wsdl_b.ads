
with WSDL_B_Pck;

package WSDL_B is

   type Toto is array (Positive range 1 .. 12) of Integer;

   type Set_Of_Int is array (Positive range <>) of Integer;

   type XYZ is new Set_Of_Int;

   type ABC is new Set_Of_Int (1 .. 2);

   type Set_Of_Int_10 is new Set_Of_Int (1 .. 10);

   subtype Set_Of_Int_20 is Set_Of_Int (1 .. 20);

   type Rec is record
      C : ABC;
      D : WSDL_B_Pck.Arr1;
      E : WSDL_B_Pck.Arr2;
   end record;

   type Complex_Rec is record
      R   : Wsdl_B_Pck.Rec;
      SI  : Set_Of_Int (1 .. 10);
      SI2 : Set_Of_Int_10;
      SI3 : Set_Of_Int_20;
   end record;

   function Echo_Complex_Rec (C_Rec : in Complex_Rec) return Complex_Rec;

end WSDL_B;
