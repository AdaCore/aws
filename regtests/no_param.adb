
--  $Id$

with Ada.Strings.Unbounded;
with Ada.Text_IO;

with AWS.Parameters;
with AWS.Parameters.Set;

procedure No_Param is

   use Ada.Strings.Unbounded;
   use Ada.Text_IO;
   use AWS;

   pragma warnings (Off);
   Empty : Parameters.List;
   pragma warnings (On);

begin
   Parameters.Set.Reset (Empty);

   Put_Line ("Count      " & Natural'Image (Parameters.Count (Empty)));
   Put_Line ("Count-2    " & Natural'Image (Parameters.Count (Empty, "a")));
   Put_Line ("Name_Count " & Natural'Image (Parameters.Name_Count (Empty)));
   Put_Line ("Get        " & Parameters.Get (Empty, "toto"));
   Put_Line ("Get_Name   " & Parameters.Get_Name (Empty, 1));
   Put_Line ("Get_Value  " & Parameters.Get_Value (Empty, 1));

   declare
      N : constant Parameters.Vstring_Array := Parameters.Get_Names (Empty);
   begin
      for K in N'Range loop
         Put_Line (" N " & Natural'Image (K) & " = " & To_String (N (K)));
      end loop;
   end;

   declare
      V : constant Parameters.Vstring_Array
        := Parameters.Get_Values (Empty, "titi");
   begin
      for K in V'Range loop
         Put_Line (" V " & Natural'Image (K) & " = " & To_String (V (K)));
      end loop;
   end;
end No_Param;
