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
with AWS.Utils;

separate (WSDL2AWS.Generator)
package body Skel is

   use Templates;

   Template_Skel_Ads : constant String := "s-skel.tads";
   Template_Skel_Adb : constant String := "s-skel.tadb";

   -----------------
   -- End_Service --
   -----------------

   procedure End_Service
     (O    : in out Object;
      Name : String)
   is
      LL_Name : constant String :=
                  Characters.Handling.To_Lower (Format_Name (O, Name))
                  & "-server";
   begin
      Generate (O, LL_Name & ".ads", Template_Skel_Ads, O.Skel_S_Trans);
      Generate (O, LL_Name & ".adb", Template_Skel_Adb, O.Skel_B_Trans);
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
      pragma Unreferenced
        (Wrapper_Name, Namespace, Fault, Documentation, SOAPAction, Proc);

      use all type SOAP.WSDL.Parameter_Type;

      use type WSDL.Parameters.P_Set;
      use type WSDL.Types.Kind;

      procedure Output_Parameters (N : WSDL.Parameters.P_Set);
      --  Output parameters

      N                 : WSDL.Parameters.P_Set;

      Parameter_B_Name  : Templates.Tag;
      Parameter_B_Type  : Templates.Tag;
      Parameter_Get     : Templates.Tag;
      Decl_Name         : Templates.Tag;
      Decl_Kind         : Templates.Tag;
      Decl_P_Name       : Templates.Tag;
      CB_Parameter_Name : Templates.Tag;

      -----------------------
      -- Output_Parameters --
      -----------------------

      procedure Output_Parameters (N : WSDL.Parameters.P_Set) is
         T_Name : constant String := WSDL.Types.Name (N.Typ);
      begin
         case N.Mode is
            when WSDL.Types.K_Simple =>
               --  Check for Base64 case

               if SOAP.WSDL.To_Type (T_Name) = P_B64 then
                  Parameter_Get := Parameter_Get
                    & ("V (SOAP_Base64'(SOAP.Parameters.Get (Params, """
                       & To_String (N.Name)
                       & """)))");

               elsif SOAP.WSDL.To_Type (T_Name) = P_Character then
                  Parameter_Get := Parameter_Get
                    & ("SOAP.Utils.Get (SOAP.Parameters.Argument (Params, """
                       & To_String (N.Name)
                       & """))");

               else
                  Parameter_Get := Parameter_Get
                    & ("SOAP.Parameters.Get (Params, """
                       & To_String (N.Name)
                       & """)");
               end if;

            when WSDL.Types.K_Derived =>
               Parameter_Get := Parameter_Get
                 & WSDL.Parameters.From_SOAP
                     (N.all,
                      Object =>
                         "SOAP.Parameters.Get (Params, """
                         & To_String (N.Name) & """)",
                      Is_SOAP_Type => True);

            when WSDL.Types.K_Enumeration =>
               Parameter_Get := Parameter_Get
                 & (T_Name & "_Type'Value"
                    & "(SOAP.Utils.Get (SOAP.Parameters.Argument (Params, """
                    & To_String (N.Name)
                    & """)))");

            when WSDL.Types.K_Array =>
               raise Constraint_Error;

            when WSDL.Types.K_Record =>
               Parameter_Get := Parameter_Get
                 & "Not_Yet_Supported";
         end case;
      end Output_Parameters;

   begin
      N := Input;

      while N /= null loop
         declare
            Q_Name : constant String :=
                       SOAP.Utils.To_Name
                         (WSDL.Types.Name (N.Typ, NS => True));
            T_Name : constant String := WSDL.Types.Name (N.Typ);
         begin
            if N.Mode = WSDL.Types.K_Array then
               Decl_Kind := Decl_Kind & "ARRAY";
               Decl_Name := Decl_Name
                 & (To_String (N.Name)
                    & "_" & Format_Name (O, T_Name) & "_Array");
               Decl_P_Name := Decl_P_Name
                 & To_String (N.Name);

            elsif N.Mode = WSDL.Types.K_Record then
               Decl_Name := Decl_Name
                 & (To_String (N.Name)
                    & "_" & Format_Name (O, T_Name) & "_Record");
               Decl_P_Name := Decl_P_Name
                 & To_String (N.Name);

               if N.P = null then
                  Decl_Kind := Decl_Kind & "NULL_RECORD";
               else
                  Decl_Kind := Decl_Kind & "RECORD";
               end if;
            end if;

            Parameter_B_Name := Parameter_B_Name
              & Format_Name (O, To_String (N.Name));

            case N.Mode is
               when WSDL.Types.K_Simple =>
                  Parameter_B_Type := Parameter_B_Type
                    & SOAP.WSDL.To_Ada (SOAP.WSDL.To_Type (T_Name));

                  Output_Parameters (N);

               when WSDL.Types.K_Enumeration =>
                  Parameter_B_Type := Parameter_B_Type
                    & (T_Name & "_Type");

                  Output_Parameters (N);

               when WSDL.Types.K_Derived =>
                  Parameter_B_Type := Parameter_B_Type
                    & (Q_Name & "_Type");

                  Output_Parameters (N);

               when WSDL.Types.K_Array =>
                  Parameter_B_Type := Parameter_B_Type
                    & (Format_Name (O, T_Name) & "_Type");

                  Parameter_Get := Parameter_Get
                    & ("To_"
                       & Format_Name (O, T_Name) & "_Type (V ("
                       & To_String (N.Name) & "_"
                       & Format_Name (O, T_Name) & "_Array))");

               when WSDL.Types.K_Record =>
                  Parameter_B_Type := Parameter_B_Type
                    & (Format_Name (O, T_Name) & "_Type");

                  Parameter_Get := Parameter_Get
                    & ("To_"
                       & Format_Name (O, T_Name) & "_Type ("
                       & To_String (N.Name) & "_"
                       & Format_Name (O, T_Name) & "_Record)");
            end case;
         end;

         N := N.Next;
      end loop;

      Add_TagV (O.Skel_B_Trans, "DECL_C_NAME", Decl_Name);
      Add_TagV (O.Skel_B_Trans, "P_NAME", Decl_P_Name);
      Add_TagV (O.Skel_B_Trans, "DECL_C_KIND", Decl_Kind);

      Add_TagV (O.Skel_B_Trans, "DECL_P_NAME", Parameter_B_Name);
      Add_TagV (O.Skel_B_Trans, "DECL_P_TYPE", Parameter_B_Type);
      Add_TagV (O.Skel_B_Trans, "DECL_P_GET", Parameter_Get);

      --  Output parameters

      if Output = null then
         Add_TagV (O.Skel_B_Trans, "OUT_CB_FIELD_NAME", "");

      else
         if Is_Simple_Wrapped_Parameter (O, Output) then
            --  A simple wrapped output, assign the result here

            if WSDL.Parameters.Length (Output.P) = 1
              and then Output.P.Mode /= WSDL.Types.K_Array
            then
               Add_TagV
                 (O.Skel_B_Trans,
                  "OUT_CB_FIELD_NAME", To_String (Output.P.Name));

            else
               Add_TagV (O.Skel_B_Trans, "OUT_CB_FIELD_NAME", "");
            end if;
         else
            Add_TagV (O.Skel_B_Trans, "OUT_CB_FIELD_NAME", "");
         end if;
      end if;

      --  Input parameters for callback

      if Is_Simple_Wrapped_Parameter (O, Input) then
         --  A simple wrapped input as parameters, inline all fields as
         --  parameter for the callback.

         N := Input.P;

         while N /= null loop
            declare
               Name : constant String :=
                        Format_Name (O, To_String (Input.Name))
                        & "."
                        & Format_Name (O, To_String (N.Name));
            begin
               if Is_String (N) then
                  CB_Parameter_Name := CB_Parameter_Name
                    & ('-' & Name);
               elsif N.Mode = WSDL.Types.K_Array and then O.Sp then
                  CB_Parameter_Name := CB_Parameter_Name
                    & (Name & ".Item.all");
               else
                  CB_Parameter_Name := CB_Parameter_Name
                    & Name;
               end if;
            end;

            N := N.Next;
         end loop;

      else
         N := Input;

         while N /= null loop
            CB_Parameter_Name := CB_Parameter_Name
              & Format_Name (O, To_String (N.Name));

            N := N.Next;
         end loop;
      end if;

      Add_TagV (O.Skel_B_Trans, "CB_PARAMETER_NAME", CB_Parameter_Name);
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

      U_Name : constant String         := To_Unit_Name (Format_Name (O, Name));
      URL    : constant AWS.URL.Object :=
                 AWS.URL.Parse (Get_Endpoint (O, Location));
      S_With : Templates.Tag;
      B_With : Templates.Tag;
   begin
      --  Spec

      O.Skel_S_Trans := O.Skel_S_Trans
        & Templates.Assoc ("UNIT_NAME", U_Name)
        & Templates.Assoc ("SERVER_PORT", Positive'(AWS.URL.Port (URL)))
        & Templates.Assoc ("SERVICE_DOCUMENTATION", Documentation);

      S_With := S_With
        & "Ada.Calendar"
        & "System.Assertions"
        & "AWS.Status"
        & "AWS.Response"
        & "SOAP.Message.Payload"
        & "SOAP.Types";

      O.Skel_S_Trans := O.Skel_S_Trans
        & Templates.Assoc ("WITHED_UNITS", S_With);

      --  Body

      O.Skel_B_Trans := O.Skel_B_Trans
        & Templates.Assoc ("UNIT_NAME", U_Name)
        & Templates.Assoc ("SERVER_PORT", Positive'(AWS.URL.Port (URL)));

      B_With := B_With
        & "Ada.Exceptions"
        & "SOAP.Message.Response.Error"
        & "SOAP.Name_Space"
        & "SOAP.Parameters"
        & "SOAP.Utils";

      O.Skel_B_Trans := O.Skel_B_Trans
        & Templates.Assoc ("WITHED_UNITS", B_With);
   end Start_Service;

end Skel;
