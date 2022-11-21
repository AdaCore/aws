------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2003-2022, AdaCore                     --
--                                                                          --
--  This library is free software;  you can redistribute it and/or modify   --
--  it under terms of the  GNU General Public License  as published by the  --
--  Free Software  Foundation;  either version 3,  or (at your  option) any --
--  later version. This library is distributed in the hope that it will be  --
--  useful, but WITHOUT ANY WARRANTY;  without even the implied warranty of --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                    --
--                                                                          --
--  As a special exception under Section 7 of GPL version 3, you are        --
--  granted additional permissions described in the GCC Runtime Library     --
--  Exception, version 3.1, as published by the Free Software Foundation.   --
--                                                                          --
--  You should have received a copy of the GNU General Public License and   --
--  a copy of the GCC Runtime Library Exception along with this program;    --
--  see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see   --
--  <http://www.gnu.org/licenses/>.                                         --
--                                                                          --
--  As a special exception, if other files instantiate generics from this   --
--  unit, or you link this unit with other files to produce an executable,  --
--  this  unit  does not  by itself cause  the resulting executable to be   --
--  covered by the GNU General Public License. This exception does not      --
--  however invalidate any other reasons why the executable file  might be  --
--  covered by the  GNU Public License.                                     --
------------------------------------------------------------------------------

with AWS.URL;

separate (WSDL2AWS.Generator)
package body Stub is

   use Templates;

   Template_Stub_Ads : constant String := "s-stub.tads";
   Template_Stub_Adb : constant String := "s-stub.tadb";

   -----------------
   -- End_Service --
   -----------------

   procedure End_Service
     (O    : in out Object;
      Name : String)
   is
      LL_Name : constant String :=
                  Characters.Handling.To_Lower (Format_Name (O, Name))
                  & "-client";
   begin
      --  Text_IO.Put_Line ("Generate : " & LL_Name);

      Generate (O, LL_Name & ".ads", Template_Stub_Ads, O.Stub_S_Trans);
      Generate (O, LL_Name & ".adb", Template_Stub_Adb, O.Stub_B_Trans);
   end End_Service;

   -------------------
   -- New_Procedure --
   -------------------

   procedure New_Procedure
     (O             : in out Object;
      Proc          : String;
      Documentation : String;
      SOAPAction    : String;
      Wrapper_Name  : String;
      Namespace     : SOAP.Name_Space.Object;
      Input         : WSDL.Parameters.P_Set;
      Output        : WSDL.Parameters.P_Set;
      Fault         : WSDL.Parameters.P_Set)
   is
      pragma Unreferenced (Documentation, SOAPAction, Wrapper_Name, Fault);
      use all type SOAP.WSDL.Parameter_Type;

      use type WSDL.Parameters.P_Set;
      use type WSDL.Types.Kind;

      procedure Input_Parameters
        (Input : WSDL.Parameters.P_Set);

      Decl_Name         : Templates.Tag;
      Decl_Type         : Templates.Tag;
      Decl_Prefix_Name  : Templates.Tag;
      Decl_Field_Name   : Templates.Tag;
      Decl_Field_Kind   : Templates.Tag;
      From_SOAP         : Templates.Tag;

      ----------------------
      -- Input_Parameters --
      ----------------------

      procedure Input_Parameters
        (Input : WSDL.Parameters.P_Set)
      is
         N           : WSDL.Parameters.P_Set := Input;
         P_Decl      : Templates.Tag;
         P_Name      : Templates.Tag;
         P_Kind      : Templates.Tag;
         P_Type      : Templates.Tag;
         P_SOAP_Type : Templates.Tag;
         P_Q_Name    : Templates.Tag;
         P_NS_Name   : Templates.Tag;
         P_NS_Value  : Templates.Tag;
      begin
         while N /= null loop
            P_Decl      := P_Decl & Format_Name (O, To_String (N.Name));
            P_Name      := P_Name & To_String (N.Name);
            P_Kind      := P_Kind & WSDL.Types.Kind'Image (N.Mode);
            P_Type      := P_Type & WSDL.Types.Name (N.Typ, True);
            P_Q_Name    := P_Q_Name
                         & SOAP.Utils.To_Name (WSDL.Types.Name (N.Typ, True));

            if N.Mode = WSDL.Types.K_Simple then
               P_SOAP_Type := P_SOAP_Type
                                & SOAP.WSDL.Set_Type
                                    (SOAP.WSDL.To_Type
                                       (WSDL.Types.Name (N.Typ)));
            else
               P_SOAP_Type := P_SOAP_Type & "";
            end if;

            declare
               NS     : constant SOAP.Name_Space.Object :=
                          SOAP.WSDL.Name_Spaces.Get
                            (SOAP.Utils.NS (To_String (N.Elmt_Name)));
            begin
               P_NS_Name  := P_NS_Name & SOAP.Name_Space.Name (NS);
               P_NS_Value := P_NS_Value & SOAP.Name_Space.Value (NS);
            end;

            N := N.Next;
         end loop;

         Add_TagV (O.Stub_B_Trans, "P_DECL", P_Decl);
         Add_TagV (O.Stub_B_Trans, "P_NAME", P_Name);
         Add_TagV (O.Stub_B_Trans, "P_KIND", P_Kind);
         Add_TagV (O.Stub_B_Trans, "P_TYPE", P_Type);
         Add_TagV (O.Stub_B_Trans, "P_SOAP_TYPE", P_SOAP_Type);
         Add_TagV (O.Stub_B_Trans, "P_Q_NAME", P_Q_Name);
         Add_TagV (O.Stub_B_Trans, "P_NS_NAME", P_NS_Name);
         Add_TagV (O.Stub_B_Trans, "P_NS_VALUE", P_NS_Value);
      end Input_Parameters;

      use type SOAP.Name_Space.Object;

   begin
      if Is_Simple_Wrapped_Parameter (O, Input) then
         Decl_Name := Decl_Name & Format_Name (O, To_String (Input.Name));
         Decl_Type := Decl_Type
           & Format_Name (O, WSDL.Types.Name (Input.Typ) & "_Type");
      end if;

      Add_TagV (O.Stub_B_Trans, "DECL_NAME", Decl_Name);
      Add_TagV (O.Stub_B_Trans, "DECL_TYPE", Decl_Type);

      --  Set parameters

      if Is_Simple_Wrapped_Parameter (O, Input) then
         --  Set individual fields of the input record

         declare
            N : WSDL.Parameters.P_Set := Input.P;
         begin
            while N /= null loop
               Decl_Prefix_Name := Decl_Prefix_Name
                 & Format_Name (O, To_String (Input.Name));
               Decl_Field_Name := Decl_Field_Name
                 & Format_Name (O, To_String (N.Name));
               Decl_Field_Kind := Decl_Field_Kind
                 & (if Is_String (N)
                    or else N.Mode = WSDL.Types.K_Array
                    then "ARRAY"
                    else  "OTHER");

               N := N.Next;
            end loop;
         end;
      end if;

      Add_TagV (O.Stub_B_Trans, "DECL_PREFIX_NAME", Decl_Prefix_Name);
      Add_TagV (O.Stub_B_Trans, "DECL_FIELD_NAME", Decl_Field_Name);
      Add_TagV (O.Stub_B_Trans, "DECL_FIELD_KIND", Decl_Field_Kind);

      Input_Parameters (Input);

      if Namespace /= SOAP.Name_Space.No_Name_Space then
         Add_TagV
           (O.Stub_B_Trans,
            "PROC_NAME_SPACE_NAME",
            SOAP.Name_Space.Name (Namespace));
         Add_TagV
           (O.Stub_B_Trans,
            "PROC_NAME_SPACE_VALUE",
            SOAP.Name_Space.Value (Namespace));
      end if;

      if Output /= null then
         if WSDL.Parameters.Length (Output) = 1 then
            --  A single parameter is returned

            declare
               T_Name : constant String := WSDL.Types.Name (Output.Typ);
            begin
               case Output.Mode is

                  when WSDL.Types.K_Simple =>
                     if SOAP.WSDL.To_Type (T_Name) = P_B64 then
                        From_SOAP := From_SOAP
                          & ("V (SOAP_Base64'(SOAP.Parameters.Get "
                             & "(R_Param, """
                             & To_String (Output.Name) & """)))");

                     elsif SOAP.WSDL.To_Type (T_Name) = P_Character then
                        From_SOAP := From_SOAP
                          & ("SOAP.Utils.Get "
                             & "(SOAP.Parameters.Argument (R_Param, """
                             & To_String (Output.Name) & """))");

                     else
                        From_SOAP := From_SOAP
                          & ("SOAP.Parameters.Get"
                             & " (R_Param, """
                             & To_String (Output.Name) & """)");
                     end if;

                  when WSDL.Types.K_Derived =>
                     From_SOAP := From_SOAP
                       & ("To_" & Format_Name (O, Proc) & "_Result (R_Param)");

                  when WSDL.Types.K_Enumeration =>
                     From_SOAP := From_SOAP
                       & (Result_Type (O, Proc, Output)
                          & "("
                          & T_Name & "_Type'Value"
                          & " (SOAP.Utils.Get"
                          & " (SOAP.Parameters.Argument (R_Param, """
                          & To_String (Output.Name)
                          & """))))");

                  when WSDL.Types.K_Array =>
                     From_SOAP := From_SOAP
                       & ("To_" & Format_Name (O, T_Name) & "_Type"
                          & "(V (SOAP_Array'(SOAP.Parameters.Get (R_Param, """
                          & To_String (Output.Name)
                          & """))))");

                  when WSDL.Types.K_Record =>
                     From_SOAP := From_SOAP
                       & ("To_" & Format_Name (O, T_Name) & "_Type"
                          & " (SOAP_Record'(SOAP.Parameters.Get (R_Param, """
                          & To_String (Output.Name)
                          & """)))");
               end case;
            end;

         else
            From_SOAP := From_SOAP
              & ("To_" & Format_Name (O, Proc) & "_Result (R_Param)");
         end if;

         if Is_Simple_Wrapped_Parameter (O, Output)
           and then WSDL.Parameters.Length (Output.P) = 1
           and then Output.P.Mode /= WSDL.Types.K_Array
         then
            --  A simple field as result for a wrapped output, we want to
            --  return the internal record field only as result.

            declare
               N : constant WSDL.Parameters.P_Set := Output.P;
            begin
               Add_TagV (O.Stub_B_Trans,
                         "OUT_PARAMETER_IS_STRING",
                         Is_String (N));

               Add_TagV
                 (O.Stub_B_Trans,
                  "RETURN_RESULT",
                  ("Result." & To_String (N.Name)));
               Add_TagV
                 (O.Skel_B_Trans,
                  "RETURN_RESULT",
                  ("Result." & To_String (N.Name)));
            end;

         else
            Add_TagV (O.Stub_B_Trans,
                      "OUT_PARAMETER_IS_STRING",
                      False);

            Add_TagV
              (O.Stub_B_Trans,
               "RETURN_RESULT",
               ("Result"));
            Add_TagV
              (O.Skel_B_Trans,
               "RETURN_RESULT",
               ("Result"));
         end if;

      else
         Add_TagV
           (O.Stub_B_Trans,
            "RETURN_RESULT",
            ("None_Is_Procedure"));
         Add_TagV
           (O.Skel_B_Trans,
            "RETURN_RESULT",
            ("None_Is_Procedure"));
         Add_TagV (O.Stub_B_Trans,
                   "OUT_PARAMETER_IS_STRING",
                   False);
      end if;

      Add_TagV
        (O.Stub_B_Trans,
         "RESULT_FROM_PARAMS",
         From_SOAP);
   end New_Procedure;

   -------------------
   -- Start_Service --
   -------------------

   procedure Start_Service
     (O                  : in out Object;
      Name               : String;
      Root_Documentation : String;
      Documentation      : String;
      Location           : String)
   is
      pragma Unreferenced (Root_Documentation);

      use type SOAP.WSDL.Schema.Binding_Style;

      U_Name : constant String := To_Unit_Name (Format_Name (O, Name));
      URL    : constant AWS.URL.Object :=
                 AWS.URL.Parse (Get_Endpoint (O, Location));
      S_With : Templates.Tag;
      B_With : Templates.Tag;
   begin
      --  Spec

      O.Stub_S_Trans := O.Stub_S_Trans
        & Templates.Assoc ("UNIT_NAME", U_Name)
        & Templates.Assoc ("SERVER_PORT", Positive'(AWS.URL.Port (URL)))
        & Templates.Assoc ("SERVICE_DOCUMENTATION", Documentation);

      S_With := S_With
        & "Ada.Calendar"
        & "SOAP.Types";

      O.Stub_S_Trans := O.Stub_S_Trans
        & Templates.Assoc ("WITHED_UNITS", S_With);

      --  Body

      O.Stub_B_Trans := O.Stub_B_Trans
        & Templates.Assoc ("UNIT_NAME", U_Name)
        & Templates.Assoc ("SERVER_PORT", Positive'(AWS.URL.Port (URL)))
        & Templates.Assoc ("DEBUG", O.Debug)
        & Templates.Assoc ("IS_RPC", O.Style = SOAP.WSDL.Schema.RPC);

      B_With := B_With
        & "Ada.Exceptions"
        & "SOAP.Client"
        & "SOAP.Message.Payload"
        & "SOAP.Message.Response"
        & "SOAP.Message.XML"
        & "SOAP.Name_Space"
        & "SOAP.Parameters"
        & "SOAP.Utils";

      O.Stub_B_Trans := O.Stub_B_Trans
        & Templates.Assoc ("WITHED_UNITS", B_With);
   end Start_Service;

end Stub;
