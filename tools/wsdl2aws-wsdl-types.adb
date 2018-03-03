------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2015-2018, AdaCore                     --
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
with SOAP.WSDL.Name_Spaces;

package body WSDL2AWS.WSDL.Types is

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

   function Create
     (Name : String; NS : SOAP.Name_Space.Object) return Object
   is
      use type SOAP.Name_Space.Object;
   begin
      if NS = SOAP.Name_Space.No_Name_Space then
         return Object'(To_Unbounded_String (Name), NS);
      else
         return Object'(To_Unbounded_String (SOAP.Utils.No_NS (Name)), NS);
      end if;
   end Create;

   ----------
   -- Find --
   ----------

   function Find
     (O          : Object;
      Registered : Boolean := False) return Definition is
   begin
      --  First check for a known definition, including a possible type with
      --  name Character which is not an Ada character.

      for D of Store loop
         if O = D.Ref then
            return D;
         end if;
      end loop;

      --  If no definition found, check for a standard type

      if not Registered and then SOAP.WSDL.Is_Standard (Name (O)) then
         return Definition'(K_Simple, Ref => (O.Name, SOAP.Name_Space.XSD));
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
      --  Recursively called to return the set of instructions to convert
      --  from a SOAP object to the actual Ada type.

      -----------------
      -- For_Derived --
      -----------------

      function For_Derived
        (Def  : WSDL.Types.Definition;
         Code : String) return String
      is
         use type SOAP.Name_Space.Object;
      begin
         if Types.NS (Def.Ref) = SOAP.Name_Space.XSD then
            if Is_SOAP_Type then
               return Code;
            else
               return SOAP.WSDL.V_Routine
                 (SOAP.WSDL.To_Type
                    (Types.Name (Def.Ref)),
                  not WSDL.Types.Is_Constrained (Def))
                 & " ("
                 & SOAP.WSDL.Set_Type
                     (SOAP.WSDL.To_Type (Types.Name (Def.Ref)))
                 & " (" & Code & "))";
            end if;

         else
            declare
               P_NS   : constant SOAP.Name_Space.Object := Def.Parent.NS;
               P_Name : constant String :=
                          SOAP.Utils.To_Name
                            (Types.Name (Def.Parent,
                             NS => not SOAP.WSDL.Name_Spaces.Is_XSD (P_NS)));
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
            if Is_Character (Def) then
               declare
                  P_Type : constant SOAP.WSDL.Parameter_Type :=
                             SOAP.WSDL.To_Type (Types.Name (Def.Ref));
                  I_Type : constant String := SOAP.WSDL.Set_Type (P_Type);
               begin
                  return SOAP.WSDL.V_Routine (P_Type, Constrained => True)
                    & " (" & I_Type & " ("
                    & Object & "))";
               end;

            else
               return For_Derived (Def, Object);
            end if;

         when WSDL.Types.K_Enumeration =>
            return Types.Name (Def.Ref) & "_Type'Value ("
              & "SOAP.Types.V (SOAP.Types.SOAP_Enumeration ("
              & Object & ")))";

         when WSDL.Types.K_Array =>
            if Is_Uniq then
               return "+To_" & SOAP.Utils.No_NS (Type_Name)
                 & "_Type (SOAP.Types.V (SOAP.Types.SOAP_Array ("
                 & Object & ")))";
            else
               return "+To_" & SOAP.Utils.No_NS (Type_Name)
                 & "_Type (" & Object & ")";
            end if;

         when WSDL.Types.K_Record =>
            return "To_" & SOAP.Utils.No_NS (Type_Name)
              & " (SOAP.Types.SOAP_Record (" & Object & "))";

         when WSDL.Types.K_Simple =>
            declare
               P_Type : constant SOAP.WSDL.Parameter_Type :=
                          SOAP.WSDL.To_Type (Types.Name (Def.Ref));
               I_Type : constant String := SOAP.WSDL.Set_Type (P_Type);
            begin
               return SOAP.WSDL.V_Routine (P_Type, Constrained => True)
                 & " (" & I_Type & " ("
                 & Object & "))";
            end;
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
      if not SOAP.WSDL.Is_Standard (To_String (Def.Ref.Name)) then
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

   ---------------------------
   -- Get_Schema_Definition --
   ---------------------------

   function Get_Schema_Definition return SOAP.WSDL.Schema.Definition is

      S_Def : SOAP.WSDL.Schema.Definition;

      procedure Set_Aliases (Def : Definition);

      procedure Set_Record (Def : Definition);

      procedure Set_Enumeration (Def : Definition);

      -----------------
      -- Set_Aliases --
      -----------------

      procedure Set_Aliases (Def : Definition) is
         use type SOAP.Name_Space.Object;

         N_N  : constant SOAP.Name_Space.Object :=
                  WSDL.Types.NS (Def.Ref);
         Name : constant String :=
                  (if N_N = SOAP.Name_Space.No_Name_Space
                   then ""
                   else SOAP.Name_Space.Name (N_N) & ':')
                  & WSDL.Types.Name (Def.Ref);

         Root_Type : constant String :=
                       (if Def.Mode = WSDL.Types.K_Derived
                        then WSDL.Types.Root_Type_For (Def, Registered => True)
                        else WSDL.Types.Name (Def.Ref));
      begin
         if not S_Def.Contains (Name) then
            S_Def.Insert (Name, Root_Type);
         end if;
      end Set_Aliases;

      ---------------------
      -- Set_Enumeration --
      ---------------------

      procedure Set_Enumeration (Def : Definition) is
         N_N  : constant SOAP.Name_Space.Object :=
                  WSDL.Types.NS (Def.Ref);
         Name : constant String :=
                  SOAP.Name_Space.Name (N_N)
                & ":" & WSDL.Types.Name (Def.Ref);
      begin
         if not S_Def.Contains (Name) then
            S_Def.Insert (Name, "@enum");
         end if;
      end Set_Enumeration;

      ----------------
      -- Set_Record --
      ----------------

      procedure Set_Record (Def : Definition) is
         N_N    : constant SOAP.Name_Space.Object :=
                    WSDL.Types.NS (Def.Ref);
         T_Name : constant String :=
                    SOAP.Name_Space.Name (N_N)
                    & ":" & WSDL.Types.Name (Def.Ref);

         Name   : constant String :=
                    WSDL.Types.Name (Def.Ref);
      begin
         if not S_Def.Contains (Name) then
            S_Def.Insert (Name, T_Name);
         end if;
      end Set_Record;

   begin
      for K in Store.Iterate loop
         declare
            D : constant Definition := Store (K);
         begin
            if D.Mode in WSDL.Types.K_Derived then
               Set_Aliases (D);

            elsif D.Mode in WSDL.Types.K_Enumeration then
               Set_Enumeration (D);

            elsif D.Mode =  WSDL.Types.K_Record then
               Set_Record (D);
            end if;
         end;
      end loop;

      return S_Def;
   end Get_Schema_Definition;

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

   ------------------
   -- Is_Character --
   ------------------

   function Is_Character (Def : Definition) return Boolean is
   begin
      return Def.Mode = K_Derived
        and then Def.Constraints.Length = 1
        and then Def.Parent.Name = "string";
   end Is_Character;

   --------------------
   -- Is_Constrained --
   --------------------

   function Is_Constrained (Def : Definition) return Boolean is
      use all type SOAP.WSDL.Parameter_Type;
   begin
      return (Def.Mode = K_Derived
              and then SOAP.WSDL.Is_Standard (Name (Def.Parent))
              and then SOAP.WSDL.To_Type (Name (Def.Parent)) = P_String
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

   function NS (O : Object) return SOAP.Name_Space.Object is
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

   function Root_Type_For
     (Def        : Definition;
      Registered : Boolean := False) return String
   is
      T_Name : constant String := WSDL.Types.Name (Def.Ref, NS => True);
   begin
      if Def.Mode /= K_Derived or else SOAP.WSDL.Is_Standard (T_Name) then
         return Name (Def.Ref, NS => True);

      else
         declare
            D : constant Definition := Find (Def.Parent, Registered);
         begin
            if D = No_Definition then
               return Name (Def.Parent, True);
            else
               return Root_Type_For (D, Registered);
            end if;
         end;
      end if;
   end Root_Type_For;

   -------------
   -- To_SOAP --
   -------------

   function To_SOAP
     (Def          : Definition;
      Object, Name : String;
      Type_Name    : String := "";
      Name_Kind    : Ref_Kind := Both_Value;
      Is_Uniq      : Boolean := True;
      NS           : String := "") return String
   is

      function For_Derived
        (Def  : Definition;
         Code : String;
         NS   : SOAP.Name_Space.Object) return String;
      --  Recursively output the code to convert a derived type definition to
      --  a SOAP object.

      function Set_Routine (Def : Definition) return String;
      --  The routine to convert from an Ada type to the corresponding SOAP
      --  object.

      function Get_Name return String
        is (if Name_Kind in Name_Var | Both_Var
            then Name
            else '"' & Name & '"');

      function Get_Type_Name return String
        is (if Name_Kind in Type_Var | Both_Var
            then Type_Name
            else '"' & Type_Name & '"');

      -----------------
      -- For_Derived --
      -----------------

      function For_Derived
        (Def  : Definition;
         Code : String;
         NS   : SOAP.Name_Space.Object) return String
      is
         use type SOAP.Name_Space.Object;
      begin
         if Types.NS (Def.Ref) = SOAP.Name_Space.XSD then
            return SOAP.WSDL.Set_Routine (WSDL.Types.Name (Def.Ref))
              & " (" & Code & ", "
              & Get_Name
              & ", "
              & Get_Type_Name
              & ", "
              & (if To_SOAP.NS = ""
                 then "SOAP.Name_Space.Create ("""
                      & SOAP.Name_Space.Name (NS) & """"
                      & ", """ & SOAP.Name_Space.Value (NS) & """)"
                 else To_SOAP.NS)
              & ")";

         else
            declare
               P_NS   : constant SOAP.Name_Space.Object := Def.Parent.NS;
               P_Name : constant String :=
                          SOAP.Utils.To_Name
                            (Types.Name
                               (Def.Parent,
                                NS =>
                                   not SOAP.WSDL.Name_Spaces.Is_XSD (P_NS)));
            begin
               return For_Derived
                 (WSDL.Types.Find (Def.Parent),
                  "To_" & P_Name & "_Type" & " (" & Code & ')', NS);
            end;
         end if;
      end For_Derived;

      -----------------
      -- Set_Routine --
      -----------------

      function Set_Routine (Def : WSDL.Types.Definition) return String is
         use all type SOAP.WSDL.Parameter_Type;
         T_Name : constant String := Types.Name (Def.Ref);
      begin
         case Def.Mode is
            when WSDL.Types.K_Simple =>
               return SOAP.WSDL.Set_Routine
                 (SOAP.WSDL.To_Type (T_Name), Constrained => True);

            when WSDL.Types.K_Derived =>
               if SOAP.WSDL.To_Type (T_Name) = P_Character then
                  return SOAP.WSDL.Set_Routine
                    (Types.Name (Def.Ref), Constrained => False);
               else
                  return SOAP.WSDL.Set_Routine
                    (Types.Name (Def.Parent), Constrained => True);
               end if;

            when WSDL.Types.K_Enumeration =>
               return SOAP.WSDL.Set_Routine
                 (SOAP.WSDL.P_String, Constrained => True);

            when WSDL.Types.K_Array =>
               declare
                  E_Type : constant String := WSDL.Types.Name (Def.E_Type);
               begin
                  if SOAP.WSDL.Is_Standard (E_Type) then
                     return SOAP.WSDL.Set_Routine
                       (SOAP.WSDL.To_Type (E_Type), Constrained => True);
                  else
                     return "To_SOAP_Object";
                  end if;
               end;

            when WSDL.Types.K_Record =>
               return "To_SOAP_Object";
         end case;
      end Set_Routine;

      NS_Param : constant String :=
                   (if NS = "" then "" else ", NS => " & NS);

   begin
      case Def.Mode is
         when WSDL.Types.K_Simple =>
            return Set_Routine (WSDL.Types.Find (Def.Ref))
              & " (" & Object & ", """ & Name & """, """ & Type_Name & """"
              & NS_Param & ")";

         when WSDL.Types.K_Record =>
            return Set_Routine (WSDL.Types.Find (Def.Ref))
              & " (" & Object & ", """ & Name & """" & NS_Param & ")";

         when WSDL.Types.K_Derived =>
            if Is_Character (Def) then
               return Set_Routine (WSDL.Types.Find (Def.Ref))
                 & " (" & Object & ", """ & Name & """, """ & Type_Name & """"
                 & NS_Param & ")";
            else
               return For_Derived
                 (WSDL.Types.Find (Def.Ref), Object, Def.Ref.NS);
            end if;

         when WSDL.Types.K_Enumeration =>
            return "SOAP.Types.E (Image (" & Object & "), " & Get_Type_Name
              & ", " & Get_Name & NS_Param & ")";

         when WSDL.Types.K_Array =>
            return (if Is_Uniq
                    then "SOAP_Array'(SOAP.Types.A"
                    else "SOAP_Set'(SOAP.Types.Set")
                   & " (To_Object_Set (" & Object & NS_Param & "), " & Get_Name
                   & ", " & Get_Type_Name & NS_Param & "))";
      end case;
   end To_SOAP;

end WSDL2AWS.WSDL.Types;
