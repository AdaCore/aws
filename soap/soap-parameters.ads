
with SOAP.Types;

package SOAP.Parameters is

   Max_Parameters : constant := 50;

   type Set is private;

   function Argument_Count (P : in Set) return Natural;
   --  Returns the number of parameters in P.

   function Argument (P : in Set; Name : in String) return Types.Object'Class;
   --  Returns parameters named Name in P. Raises Types.Data_Error if not
   --  found.

   function Argument (P : in Set; N : in Positive) return Types.Object'Class;
   --  Returns Nth parameters in P. Raises Types.Data_Error if not found.

   function Get (P : in Set; Name : in String) return Integer;
   --  Returns parameter named Name in P as an Integer value. Raises
   --  Types.Data_Error if this parameter does not exist or is not an Integer.

   function Get (P : in Set; Name : in String) return Float;
   --  Returns parameter named Name in P as a Float value. Raises
   --  Types.Data_Error if this parameter does not exist or is not a Float.

   ------------------
   -- Constructors --
   ------------------

   function "&" (P : in Set; O : in Types.Object'Class) return Set;
   function "+" (O : in Types.Object'Class) return Set;

   ----------------
   -- Validation --
   ----------------

   procedure Check (P : in Set; N : in Natural);
   --  Checks that there is exactly N parameters or raise Types.Data_Error.

   procedure Check_Int (P : in Set; Name : in String);
   --  Checks that parameter named Name exist and is an Integer value.

   procedure Check_Float (P : in Set; Name : in String);
   --  Checks that parameter named Name exist and is a Float value.

private

   type Object_Access is access all Types.Object'Class;

   type Object_Set is array (1 .. Max_Parameters) of Object_Access;

   type Set is record
      V : Object_Set;
      N : Natural := 0;
   end record;

end SOAP.Parameters;
