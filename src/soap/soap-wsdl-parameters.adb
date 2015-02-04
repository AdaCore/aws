------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2003-2015, AdaCore                     --
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

with Ada.Text_IO;
with Ada.Unchecked_Deallocation;

with SOAP.Name_Space;
with SOAP.Utils;

package body SOAP.WSDL.Parameters is

   ------------
   -- Append --
   ------------

   procedure Append (P : in out P_Set; Param : Parameter) is
      N : P_Set;
   begin
      if P = null then
         P := new Parameter'(Param);

      else
         N := P;

         while N.Next /= null loop
            N := N.Next;
         end loop;

         N.Next := new Parameter'(Param);
      end if;
   end Append;

   ---------------
   -- From_SOAP --
   ---------------

   function From_SOAP
     (P         : Parameter;
      Object    : String;
      Type_Name : String := "";
      Is_SOAP_Type : Boolean := False) return String
   is
      function For_Derived
        (Def : WSDL.Types.Definition; Code : String) return String;
      --  ??

      -----------------
      -- For_Derived --
      -----------------

      function For_Derived
        (Def  : WSDL.Types.Definition;
         Code : String) return String
      is
         use type SOAP.Name_Space.Object;
      begin
         if Types.NS (Def.Ref) = Name_Space.XSD then
            if Is_SOAP_Type then
               return Code;
            else
               return WSDL.V_Routine
                 (WSDL.To_Type
                    (Types.Name (Def.Ref)),
                  (if WSDL.Types.Is_Constrained (Def)
                   then WSDL.Parameter else WSDL.Component))
                 & " ("
                 & WSDL.Set_Type (To_Type (Types.Name (Def.Ref)))
                 & " (" & Code & "))";
            end if;

         else
            declare
               P_Name : constant String :=
                          Utils.No_NS (Types.Name (Def.Parent));
            begin
               return "From_" & P_Name & "_Type"
                 & " ("
                 & For_Derived (WSDL.Types.Find (Def.Parent), Code) & ')';
            end;
         end if;
      end For_Derived;

      Def : constant WSDL.Types.Definition :=  WSDL.Types.Find (P.Typ);

   begin
      case P.Mode is
         when WSDL.Types.K_Derived =>
            return For_Derived (WSDL.Types.Find (P.Typ), Object);

         when WSDL.Types.K_Enumeration =>
            return Types.Name (Def.Ref) & "_Type'Value ("
              & "SOAP.Types.V (SOAP.Types.SOAP_Enumeration ("
              & Object & ")))";

         when WSDL.Types.K_Array =>
            return "+To_" & Utils.No_NS (Type_Name)
              & "_Type (SOAP.Types.V (SOAP.Types.SOAP_Array ("
              & Object & ")))";

         when WSDL.Types.K_Record =>
            return "To_" & Utils.No_NS (Type_Name)
              & " (SOAP.Types.SOAP_Record (" & Object & "))";

         when WSDL.Types.K_Simple =>
            declare
               P_Type : constant WSDL.Parameter_Type :=
                          WSDL.To_Type (Types.Name (P.Typ));
               I_Type : constant String := WSDL.Set_Type (P_Type);
            begin
               return WSDL.V_Routine (P_Type, WSDL.Component)
                 & " (" & I_Type & " ("
                 & Object & "))";
            end;

         when others =>
            return "";
      end case;
   end From_SOAP;

   ------------
   -- Length --
   ------------

   function Length (P : access Parameter) return Natural is
      N      : access Parameter := P;
      Result : Natural := 0;
   begin
      while N /= null loop
         Result := Result + 1;
         N := N.Next;
      end loop;
      return Result;
   end Length;

   ------------
   -- Output --
   ------------

   procedure Output (P : access Parameter) is

      use Ada;
      use type Parameters.P_Set;
      use type Types.Kind;

      procedure Output (P : access Parameter; K : Natural);

      ------------
      -- Output --
      ------------

      procedure Output (P : access Parameter; K : Natural) is
      begin
         if P /= null then
            Text_IO.Put (String'(1 .. K => ' '));

            if P.Mode = Types.K_Simple then
               Text_IO.Put ("[simple] ");
               Text_IO.Put_Line
                 (To_String (P.Name) & " ; "
                  & To_Ada (To_Type (Types.Name (P.Typ))));

            else
               declare
                  Def : constant WSDL.Types.Definition :=
                          WSDL.Types.Find (P.Typ);
               begin
                  Text_IO.Put ('[' & Types.Image (Def) & "] ");

                  Text_IO.Put (To_String (P.Name) & " ; ");
                  WSDL.Types.Output (Def);
                  Text_IO.New_Line;

                  if P.Mode in WSDL.Types.Compound_Type then
                     Output (P.P, K + 3);
                  end if;
               end;
            end if;

            Output (P.Next, K);
         end if;
      end Output;

   begin
      Output (P, 6);
   end Output;

   -------------
   -- Release --
   -------------

   procedure Release (P : in out P_Set) is

      procedure Unchecked_Free is
        new Ada.Unchecked_Deallocation (Parameter, P_Set);

   begin
      if P /= null then
         if P.Mode in Types.Compound_Type then
            Release (P.P);
         end if;

         Release (P.Next);
         Unchecked_Free (P);
      end if;
   end Release;

   -------------
   -- To_SOAP --
   -------------

   function To_SOAP
     (P            : Parameter;
      Object, Name : String;
      Type_Name    : String := "") return String
   is

      function For_Derived
        (Def : WSDL.Types.Definition; Code : String) return String;
      --  ??

      function Set_Routine (P : WSDL.Parameters.Parameter) return String;
      --  ??

      -----------------
      -- For_Derived --
      -----------------

      function For_Derived
        (Def  : WSDL.Types.Definition;
         Code : String) return String
      is
         use type SOAP.Name_Space.Object;
      begin
         if Types.NS (Def.Ref) = Name_Space.XSD then
            return Set_Routine (Types.Name (Def.Ref))
              & " (" & Code & ", """ & Name & """)";
         else
            declare
               P_Name : constant String :=
                          Utils.No_NS (Types.Name (Def.Parent));
            begin
               return For_Derived
                 (WSDL.Types.Find (Def.Parent),
                  "To_" & P_Name & "_Type"
                  & " (" & Code & ')');
            end;
         end if;
      end For_Derived;

      -----------------
      -- Set_Routine --
      -----------------

      function Set_Routine (P : WSDL.Parameters.Parameter) return String is
         Def    : constant WSDL.Types.Definition := WSDL.Types.Find (P.Typ);
         T_Name : constant String := Types.Name (P.Typ);
      begin
         case P.Mode is
            when WSDL.Types.K_Simple =>
               return WSDL.Set_Routine
                 (WSDL.To_Type (T_Name), Context => WSDL.Component);

            when WSDL.Types.K_Derived =>
               return WSDL.Set_Routine
                 (Types.Name (Def.Parent),
                  Context => WSDL.Component);

            when WSDL.Types.K_Enumeration =>
               return WSDL.Set_Routine
                 (WSDL.P_String, Context => WSDL.Component);

            when WSDL.Types.K_Array =>
               declare
                  E_Type : constant String := To_String (Def.E_Type);
               begin
                  if WSDL.Is_Standard (E_Type) then
                     return WSDL.Set_Routine
                       (WSDL.To_Type (E_Type), Context => WSDL.Component);
                  else
                     return "To_SOAP_Object";
                  end if;
               end;

            when WSDL.Types.K_Record =>
               return "To_SOAP_Object";
         end case;
      end Set_Routine;

   begin
      case P.Mode is
         when WSDL.Types.K_Simple | WSDL.Types.K_Record =>
            return Set_Routine (P) & " (" & Object & ", """ & Name & """)";

         when WSDL.Types.K_Derived =>
            return For_Derived
              (WSDL.Types.Find (P.Typ), Object);

         when WSDL.Types.K_Enumeration =>
            return "SOAP.Types.E (Image (" & Object & "), """ & Type_Name
              & """, """ & Name & """)";

         when WSDL.Types.K_Array =>
            return "SOAP.Types.A (To_Object_Set (" & Object
              & "), """ & Name & """)";

      end case;
   end To_SOAP;

end SOAP.WSDL.Parameters;
