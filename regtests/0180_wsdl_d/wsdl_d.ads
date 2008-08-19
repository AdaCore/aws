
with SOAP.Utils;

package WSDL_D is

   use SOAP;

   type Name_Set is array (Positive range <>) of Integer;
   type Name_Set_Access is access Name_Set;

   package Name_Set_Safe_Pointer is
      new SOAP.Utils.Safe_Pointers (T => Name_Set, T_Access => Name_Set_Access);

   type Name_Set1 is array (Positive range <>) of Integer;
   type Name_Set1_Access is access Name_Set1;

   package Name_Set1_Safe_Pointer is
      new SOAP.Utils.Safe_Pointers (Name_Set1, Name_Set1_Access);

   type Name_Set2 is array (Positive range <>) of Integer;
   type Name_Set2_Access is access Name_Set2;

   package Name_Set2_Safe_Pointer is
      new SOAP.Utils.Safe_Pointers (Name_Set2, T_Access => Name_Set2_Access);

end WSDL_D;
