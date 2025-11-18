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

separate (WSDL2AWS.Generator)
package body CB is

   Template_CB_Ads : constant String := "s-skel-cb.tads";
   Template_CB_Adb : constant String := "s-skel-cb.tadb";

   -----------------
   -- End_Service --
   -----------------

   procedure End_Service
     (O    : in out Object;
      Name : String)
   is
      use type Templates.Translate_Set;

      LL_Name : constant String :=
                  Characters.Handling.To_Lower (Format_Name (O, Name, True))
                  & "-cb";
      U_Name  : constant String := To_Unit_Name (Format_Name (O, Name, True));
   begin
      O.CB_S_Trans := O.CB_S_Trans
        & Templates.Assoc ("UNIT_NAME", U_Name);
      O.CB_B_Trans := O.CB_B_Trans
        & Templates.Assoc ("UNIT_NAME", U_Name);

      Generate
        (O,
         LL_Name & ".ads",
         Template_CB_Ads,
         O.CB_S_Trans);
      Generate
        (O,
         LL_Name & ".adb",
         Template_CB_Adb,
         O.CB_B_Trans);
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
      pragma Unreferenced (Wrapper_Name, Namespace, Documentation);
      pragma Unreferenced (Input, Output, Fault);

      L_Proc : constant String := Format_Name (O, Proc);
   begin
      Add_TagV (O.CB_B_Trans, "PROC", Proc);
      Add_TagV (O.CB_B_Trans, "FORMATTED_PROC", L_Proc);
      Add_TagV (O.CB_B_Trans, "SOAP_ACTION",
                To_String (O.Prefix) & SOAPAction);
      Add_TagV (O.CB_B_Trans, "PROC_SPEC", Procs_Spec (O));
      Add_TagV (O.CB_B_Trans, "CB_UNIT", To_String (O.Unit));
      Add_TagV (O.CB_B_Trans, "PREFIX", To_String (O.Prefix));
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
      pragma Unreferenced (Location, Root_Documentation, Documentation);

      U_Name : constant String := To_Unit_Name (Format_Name (O, Name, True));
   begin
      Add_TagV
        (O.CB_B_Trans, "WITHED_UNITS", Types_Spec (O, With_Clause => True));

      if Types_Spec (O) /= Procs_Spec (O) then
         Add_TagV
           (O.CB_B_Trans, "WITHED_UNITS", Procs_Spec (O, With_Clause => True));
      end if;

      Add_TagV (O.CB_B_Trans, "WITHED_UNITS", U_Name & ".Server");
      Add_TagV (O.CB_B_Trans, "WITHED_UNITS", U_Name & ".Types");
   end Start_Service;

end CB;
