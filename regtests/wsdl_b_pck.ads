
package WSDL_B_Pck is

   type Arr1 is array (Positive range 1 .. 23) of Integer;
   type Arr2 is array (1 .. 32) of Integer;

   type Rec is record
      A, B : Integer;
   end record;

end WSDL_B_Pck;
