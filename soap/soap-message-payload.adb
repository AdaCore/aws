
with SOAP.Types;

package body SOAP.Message.Payload is

   -----------
   -- Build --
   -----------

   function Build
      (Procedure_Name : in     String;
       P_Set          : in     SOAP.Parameters.Set;
       Name_Space     : in     String              := "")
      return Object is
   begin
      return (To_Unbounded_String (Name_Space),
              P_Set,
              To_Unbounded_String (Procedure_Name));
   end Build;

   -----------
   -- Image --
   -----------

   function Image (P : in Object) return Unbounded_String is
      NL           : constant String := ASCII.CR & ASCII.LF;
      Message_Body : Unbounded_String;

   begin
      --  Procedure

      Append (Message_Body,
              "<awsns:" & Procedure_Name (P)
              & " xmlns:awsns=""http://mns.org/"">" & NL);

      --  Procedure's parameters

      declare
         Pars : constant SOAP.Parameters.Set := Parameters (P);
      begin
         for K in 1 .. SOAP.Parameters.Argument_Count (Pars) loop
            Append
              (Message_Body,
               "   "
               & Types.Payload_Image (SOAP.Parameters.Argument (Pars, K))
               & NL);
         end loop;
      end;

      --  Close payload objects.

      Append (Message_Body, "</awsns:"
              & Procedure_Name (P) & '>' & NL);

      return Message_Body;
   end Image;

   ----------------
   -- Parameters --
   ----------------

   function Parameters (P : in Object'Class) return SOAP.Parameters.Set is
   begin
      return P.P;
   end Parameters;

   --------------------
   -- Procedure_Name --
   --------------------

   function Procedure_Name (P : in Object'Class) return String is
   begin
      return To_String (P.Procedure_Name);
   end Procedure_Name;

   ---------
   -- Set --
   ---------

   procedure Set
     (P              : in out Object'Class;
      Procedure_Name : in     String;
      P_Set          : in     SOAP.Parameters.Set;
      Name_Space     : in     String              := "") is
   begin
      P.Name_Space     := To_Unbounded_String (Name_Space);
      P.P              := P_Set;
      P.Procedure_Name := To_Unbounded_String (Procedure_Name);
   end Set;

   --------------------
   -- Set_Parameters --
   --------------------

   procedure Set_Parameters
     (P : in out Object'Class;
      P_Set : in SOAP.Parameters.Set) is
   begin
      P.P := P_Set;
   end Set_Parameters;

   ------------------------
   -- Set_Procedure_Name --
   ------------------------

   procedure Set_Procedure_Name (P : in out Object'Class; Name  : in String) is
   begin
      P.Procedure_Name := To_Unbounded_String (Name);
   end Set_Procedure_Name;

end SOAP.Message.Payload;
