
with Ada.Tags;
with Ada.Exceptions;

with SOAP.Types;

package body SOAP.Parameters is

   use Ada;

   ---------
   -- "&" --
   ---------

   function "&" (P : in Set; O : in Types.Object'Class) return Set is
      NP : Set := P;
   begin
      NP.N := NP.N + 1;
      NP.V (NP.N) := new Types.Object'Class'(O);
      return NP;
   end "&";

   ---------
   -- "+" --
   ---------

   function "+" (O : in Types.Object'Class) return Set is
      P : Set;
   begin
      P.V (1) := new Types.Object'Class'(O);
      P.N := 1;
      return P;
   end "+";

   --------------
   -- Argument --
   --------------

   function Argument
     (P    : in Set;
      Name : in String)
      return Types.Object'Class is
   begin
      for K in 1 .. P.N loop
         if Types.Name (P.V (K).all) = Name then
            return P.V (K).all;
         end if;
      end loop;

      Exceptions.Raise_Exception
        (Types.Data_Error'Identity,
         "Argument named '" & Name & "' not found.");
   end Argument;

   --------------
   -- Argument --
   --------------

   function Argument
     (P : in Set;
      N : in Positive)
      return Types.Object'Class is
   begin
      return P.V (N).all;
   end Argument;

   --------------------
   -- Argument_Count --
   --------------------

   function Argument_Count (P : in Set) return Natural is
   begin
      return P.N;
   end Argument_Count;

   -----------
   -- Check --
   -----------

   procedure Check (P : in Set; N : in Natural) is
   begin
      if P.N /= N then
         Exceptions.Raise_Exception
           (Types.Data_Error'Identity,
            "(check) Too many arguments.");
      end if;
   end Check;

   procedure Check_Int (P : in Set; Name : in String) is
      O : Types.Object'Class := Argument (P, Name);
   begin
      if O not in Types.Integer then
         Exceptions.Raise_Exception
           (Types.Data_Error'Identity,
            "(check) Integer expected, found object "
            & Ada.Tags.Expanded_Name (O'Tag));
      end if;
   end Check_Int;

   procedure Check_Float (P : in Set; Name : in String) is
      O : Types.Object'Class := Argument (P, Name);
   begin
      if O not in Types.Float then
         Exceptions.Raise_Exception
           (Types.Data_Error'Identity,
            "(check) Float expected, found object "
            & Ada.Tags.Expanded_Name (O'Tag));
      end if;
   end Check_Float;

   ---------
   -- Get --
   ---------

   function Get (P : in Set; Name : in String) return Integer is
   begin
      return Types.Get (Argument (P, Name));
   end Get;

   function Get (P : in Set; Name : in String) return Float is
   begin
      return Types.Get (Argument (P, Name));
   end Get;

end SOAP.Parameters;

