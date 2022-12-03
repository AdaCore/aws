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
      pragma Unreferenced
        (Documentation, SOAPAction, Wrapper_Name, Fault, Proc);

      use type WSDL.Parameters.P_Set;
      use type WSDL.Types.Kind;

      Decl_Name         : Templates.Tag;
      Decl_Type         : Templates.Tag;
      Decl_Prefix_Name  : Templates.Tag;
      Decl_Field_Name   : Templates.Tag;
      Decl_Field_Kind   : Templates.Tag;

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

      U_Name : constant String := To_Unit_Name (Format_Name (O, Name));
      URL    : constant AWS.URL.Object :=
                 AWS.URL.Parse (Get_Endpoint (O, Location));
   begin
      --  Spec

      O.Stub_S_Trans := O.Stub_S_Trans
        & Templates.Assoc ("UNIT_NAME", U_Name)
        & Templates.Assoc ("SERVER_PORT", Positive'(AWS.URL.Port (URL)))
        & Templates.Assoc ("SERVICE_DOCUMENTATION", Documentation);

      --  Body

      O.Stub_B_Trans := O.Stub_B_Trans
        & Templates.Assoc ("UNIT_NAME", U_Name)
        & Templates.Assoc ("SERVER_PORT", Positive'(AWS.URL.Port (URL)));
   end Start_Service;

end Stub;
