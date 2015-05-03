------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                        Copyright (C) 2015, AdaCore                       --
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

with Ada.Containers.Indefinite_Vectors;
with Ada.Text_IO;

with SOAP.Utils;

package body SOAP.WSDL.Types is

   use Ada;

   function "-" (S : Unbounded_String) return String renames To_String;

   package Types_Store is
     new Containers.Indefinite_Vectors (Positive, Definition);

   Store : Types_Store.Vector;

   function Contains (O : Object) return Boolean;
   --  Returns True if the type is already registered

   --------------
   -- Contains --
   --------------

   function Contains (O : Object) return Boolean is
   begin
      for D of Store loop
         if O = D.Ref then
            return True;
         end if;
      end loop;

      return False;
   end Contains;

   -----------
   -- Count --
   -----------

   function Count return Natural is
   begin
      return Natural (Store.Length);
   end Count;

   ------------
   -- Create --
   ------------

   function Create (Name : String; NS : Name_Space.Object) return Object is
   begin
      return Object'(To_Unbounded_String (Name), NS);
   end Create;

   ----------
   -- Find --
   ----------

   function Find (O : Object) return Definition is
      use type Name_Space.Object;
   begin
      --  First check for a known definition, including a possible type with
      --  name Character which is not an Ada character.

      for D of Store loop
         if O = D.Ref then
            return D;
         end if;
      end loop;

      --  If not definition found, check for a standard type

      if WSDL.Is_Standard (Name (O)) then
         return Definition'(K_Simple, Ref => (O.Name, Name_Space.XSD));
      else
         return No_Definition;
      end if;
   end Find;

   ---------------
   -- From_SOAP --
   ---------------

   function From_SOAP
     (Def          : WSDL.Types.Definition;
      Object       : String;
      Type_Name    : String := "";
      Is_SOAP_Type : Boolean := False;
      Is_Uniq      : Boolean := True) return String
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
                  not WSDL.Types.Is_Constrained (Def))
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

   begin
      case Def.Mode is
         when WSDL.Types.K_Derived =>
            return For_Derived (Def, Object);

         when WSDL.Types.K_Enumeration =>
            return Types.Name (Def.Ref) & "_Type'Value ("
              & "SOAP.Types.V (SOAP.Types.SOAP_Enumeration ("
              & Object & ")))";

         when WSDL.Types.K_Array =>
            if Is_Uniq then
               return "+To_" & Utils.No_NS (Type_Name)
                 & "_Type (SOAP.Types.V (SOAP.Types.SOAP_Array ("
                 & Object & ")))";
            else
               return "+To_" & Utils.No_NS (Type_Name)
                 & "_Type (" & Object & ")";
            end if;

         when WSDL.Types.K_Record =>
            return "To_" & Utils.No_NS (Type_Name)
              & " (SOAP.Types.SOAP_Record (" & Object & "))";

         when WSDL.Types.K_Simple =>
            declare
               P_Type : constant WSDL.Parameter_Type :=
                          WSDL.To_Type (Types.Name (Def.Ref));
               I_Type : constant String := WSDL.Set_Type (P_Type);
            begin
               return WSDL.V_Routine (P_Type, Constrained => True)
                 & " (" & I_Type & " ("
                 & Object & "))";
            end;

         when others =>
            return "";
      end case;
   end From_SOAP;

   ---------------------------
   -- Get_Constraint_Double --
   ---------------------------

   procedure Get_Constraint_Double
     (Constraints : Constraints_Def;
      Lower       : in out Long_Float;
      L_Set       : out Boolean;
      Upper       : in out Long_Float;
      U_Set       : out Boolean) is
   begin
      L_Set := False;
      U_Set := False;

      --  Min

      if Constraints.Min_Inclusive /= Null_Unbounded_String then
         Lower := Long_Float'Value (-Constraints.Min_Inclusive);
         L_Set := True;
      end if;

      if Constraints.Min_Exclusive /= Null_Unbounded_String then
         Lower := Long_Float'Succ
           (Long_Float'Value (-Constraints.Min_Exclusive));
         L_Set := True;
      end if;

      --  Max

      if Constraints.Max_Inclusive /= Null_Unbounded_String then
         Upper := Long_Float'Value (-Constraints.Max_Inclusive);
         U_Set := True;
      end if;

      if Constraints.Max_Exclusive /= Null_Unbounded_String then
         Upper := Long_Float'Pred
           (Long_Float'Value (-Constraints.Max_Exclusive));
         U_Set := True;
      end if;
   end Get_Constraint_Double;

   --------------------------
   -- Get_Constraint_Float --
   --------------------------

   procedure Get_Constraint_Float
     (Constraints : Constraints_Def;
      Lower       : in out Float;
      L_Set       : out Boolean;
      Upper       : in out Float;
      U_Set       : out Boolean) is
   begin
      L_Set := False;
      U_Set := False;

      --  Min

      if Constraints.Min_Inclusive /= Null_Unbounded_String then
         Lower := Float'Value (-Constraints.Min_Inclusive);
         L_Set := True;
      end if;

      if Constraints.Min_Exclusive /= Null_Unbounded_String then
         Lower := Float'Succ (Float'Value (-Constraints.Min_Exclusive));
         L_Set := True;
      end if;

      --  Max

      if Constraints.Max_Inclusive /= Null_Unbounded_String then
         Upper := Float'Value (-Constraints.Max_Inclusive);
         U_Set := True;
      end if;

      if Constraints.Max_Exclusive /= Null_Unbounded_String then
         Upper := Float'Pred (Float'Value (-Constraints.Max_Exclusive));
         U_Set := True;
      end if;
   end Get_Constraint_Float;

   ----------------------------
   -- Get_Constraint_Integer --
   ----------------------------

   procedure Get_Constraint_Integer
     (Constraints : Constraints_Def;
      Lower       : in out Long_Long_Integer;
      L_Set       : out Boolean;
      Upper       : in out Long_Long_Integer;
      U_Set       : out Boolean) is
   begin
      L_Set := False;
      U_Set := False;

      --  Min

      if Constraints.Min_Inclusive /= Null_Unbounded_String then
         Lower := Long_Long_Integer'Value (-Constraints.Min_Inclusive);
         L_Set := True;
      end if;

      if Constraints.Min_Exclusive /= Null_Unbounded_String then
         Lower := Long_Long_Integer'Succ
           (Long_Long_Integer'Value (-Constraints.Min_Exclusive));
         L_Set := True;
      end if;

      --  Max

      if Constraints.Max_Inclusive /= Null_Unbounded_String then
         Upper := Long_Long_Integer'Value (-Constraints.Max_Inclusive);
         U_Set := True;
      end if;

      if Constraints.Max_Exclusive /= Null_Unbounded_String then
         Upper := Long_Long_Integer'Pred
           (Long_Long_Integer'Value (-Constraints.Max_Exclusive));
         U_Set := True;
      end if;
   end Get_Constraint_Integer;

   ---------------------
   -- Get_Constraints --
   ---------------------

   procedure Get_Constraints
     (Def         : Definition;
      Constraints : out Constraints_Def) is
   begin
      if not WSDL.Is_Standard (To_String (Def.Ref.Name)) then
         --  Min

         if Constraints.Min_Inclusive = Null_Unbounded_String then
            Constraints.Min_Inclusive := Def.Constraints.Min_Inclusive;
         end if;

         if Constraints.Min_Exclusive = Null_Unbounded_String then
            Constraints.Min_Exclusive := Def.Constraints.Min_Exclusive;
         end if;

         --  Max

         if Constraints.Max_Inclusive = Null_Unbounded_String then
            Constraints.Max_Inclusive := Def.Constraints.Max_Inclusive;
         end if;

         if Constraints.Max_Exclusive = Null_Unbounded_String then
            Constraints.Max_Exclusive := Def.Constraints.Max_Exclusive;
         end if;

         --  Length

         if Constraints.Length = Unset then
            Constraints.Length := Def.Constraints.Length;
         end if;

         if Constraints.Min_Length = Unset then
            Constraints.Min_Length := Def.Constraints.Min_Length;
         end if;

         if Constraints.Max_Length = Unset then
            Constraints.Max_Length := Def.Constraints.Max_Length;
         end if;

         --  Pattern

         if Constraints.Pattern = Null_Unbounded_String then
            Constraints.Pattern := Def.Constraints.Pattern;
         end if;

         Get_Constraints (Find (Def.Parent), Constraints);
      end if;
   end Get_Constraints;

   -----------
   -- Image --
   -----------

   function Image (Def : Definition) return String is
   begin
      return (case Def.Mode is
                 when K_Record      =>
                    "record" & (if Def.Is_Choice then ":choice" else ""),
                 when K_Array       => "array",
                 when K_Derived     => "derived",
                 when K_Enumeration => "enumeration",
                 when K_Simple      => "simple");
   end Image;

   --------------------
   -- Is_Constrained --
   --------------------

   function Is_Constrained (Def : Definition) return Boolean is
   begin
      return (Def.Mode = K_Derived
              and then WSDL.Is_Standard (Name (Def.Parent))
              and then WSDL.To_Type (Name (Def.Parent)) = P_String
              and then Def.Constraints.Length /= Unset)
            or else Def.Mode /= K_Derived;
   end Is_Constrained;

   ----------
   -- Name --
   ----------

   function Name (O : Object; NS : Boolean := False) return String is
   begin
      if NS then
         return SOAP.Name_Space.Name (O.NS) & ":" & To_String (O.Name);
      else
         return To_String (O.Name);
      end if;
   end Name;

   --------
   -- NS --
   --------

   function NS (O : Object) return Name_Space.Object is
   begin
      return O.NS;
   end NS;

   ------------
   -- Output --
   ------------

   procedure Output (Def : Definition) is
   begin
      Text_IO.Put (To_String (Def.Ref.Name));

      if Def.Mode = K_Array then
         Text_IO.Put (" [" & To_String (Def.E_Type.Name) & "]");
      end if;
   end Output;

   --------------
   -- Register --
   --------------

   procedure Register (Def : Definition) is
   begin
      if not Contains (Def.Ref) then
         Store.Append (Def);
      end if;
   end Register;

   -------------
   -- Release --
   -------------

   procedure Release is
   begin
      Store.Clear;
   end Release;

   -------------------
   -- Root_Type_For --
   -------------------

   function Root_Type_For (Def : Definition) return String is
   begin
      if WSDL.Is_Standard (To_String (Def.Ref.Name)) then
         return To_String (Def.Ref.Name);
      else
         return Root_Type_For (Find (Def.Parent));
      end if;
   end Root_Type_For;

   -------------
   -- To_SOAP --
   -------------

   function To_SOAP
     (Def          : WSDL.Types.Definition;
      Object, Name : String;
      Name_Is_Var  : Boolean := False;
      Type_Name    : String := "";
      Is_Uniq      : Boolean := True) return String
   is

      function For_Derived
        (Def : WSDL.Types.Definition; Code : String) return String;
      --  ??

      function Set_Routine (Def : WSDL.Types.Definition) return String;
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
              & " (" & Code & ", "
              & (if Name_Is_Var then Name else """" & Name & """")
              & ", """ & Type_Name & """)";
         else
            declare
               P_Name : constant String :=
                          Utils.No_NS (Types.Name (Def.Parent));
            begin
               return For_Derived
                 (WSDL.Types.Find (Def.Parent),
                  "To_" & P_Name & "_Type" & " (" & Code & ')');
            end;
         end if;
      end For_Derived;

      -----------------
      -- Set_Routine --
      -----------------

      function Set_Routine (Def : WSDL.Types.Definition) return String is
         T_Name : constant String := Types.Name (Def.Ref);
      begin
         case Def.Mode is
            when WSDL.Types.K_Simple =>
               return WSDL.Set_Routine
                 (WSDL.To_Type (T_Name), Constrained => True);

            when WSDL.Types.K_Derived =>
               return WSDL.Set_Routine
                 (Types.Name (Def.Parent), Constrained => True);

            when WSDL.Types.K_Enumeration =>
               return WSDL.Set_Routine (WSDL.P_String, Constrained => True);

            when WSDL.Types.K_Array =>
               declare
                  E_Type : constant String := WSDL.Types.Name (Def.E_Type);
               begin
                  if WSDL.Is_Standard (E_Type) then
                     return WSDL.Set_Routine
                       (WSDL.To_Type (E_Type), Constrained => True);
                  else
                     return "To_SOAP_Object";
                  end if;
               end;

            when WSDL.Types.K_Record =>
               return "To_SOAP_Object";
         end case;
      end Set_Routine;

   begin
      case Def.Mode is
         when WSDL.Types.K_Simple =>
            return Set_Routine (WSDL.Types.Find (Def.Ref))
               & " (" & Object & ", """ & Name & """, """ & Type_Name & """)";

         when WSDL.Types.K_Record =>
            return Set_Routine (WSDL.Types.Find (Def.Ref))
              & " (" & Object & ", """ & Name & """)";

         when WSDL.Types.K_Derived =>
            return For_Derived
              (WSDL.Types.Find (Def.Ref), Object);

         when WSDL.Types.K_Enumeration =>
            return "SOAP.Types.E (Image (" & Object & "), """ & Type_Name
              & """, """ & Name & """)";

         when WSDL.Types.K_Array =>
            return (if Is_Uniq
                    then "SOAP_Array'(SOAP.Types.A"
                    else "SOAP_Set'(SOAP.Types.Set")
              & " (To_Object_Set (" & Object & "), """ & Name & """))";

      end case;
   end To_SOAP;

end SOAP.WSDL.Types;
