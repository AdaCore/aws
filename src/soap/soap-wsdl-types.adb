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

   function Name (O : Object) return String is
   begin
      return To_String (O.Name);
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

end SOAP.WSDL.Types;
