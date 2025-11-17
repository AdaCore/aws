------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2003-2025, AdaCore                     --
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
                  Characters.Handling.To_Lower (Format_Name (O, Name, True))
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

      use type WSDL.Parameters.P_Set;
      use type WSDL.Types.Kind;

      N                 : WSDL.Parameters.P_Set;
      CB_Parameter_Name : Templates.Tag;

   begin
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

      U_Name : constant String := To_Unit_Name (Format_Name (O, Name, True));
      URL    : constant AWS.URL.Object :=
                 AWS.URL.Parse (Get_Endpoint (O, Location));
   begin
      --  Spec

      O.Skel_S_Trans := O.Skel_S_Trans
        & Templates.Assoc ("UNIT_NAME", U_Name)
        & Templates.Assoc ("SERVER_PORT", Positive'(AWS.URL.Port (URL)))
        & Templates.Assoc ("SERVICE_DOCUMENTATION", Documentation);

      --  Body

      O.Skel_B_Trans := O.Skel_B_Trans
        & Templates.Assoc ("UNIT_NAME", U_Name)
        & Templates.Assoc ("SERVER_PORT", Positive'(AWS.URL.Port (URL)));
   end Start_Service;

end Skel;
