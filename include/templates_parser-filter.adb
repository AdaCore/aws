------------------------------------------------------------------------------
--                             Templates Parser                             --
--                                                                          --
--                            Copyright (C) 2003                            --
--                               Pascal Obry                                --
--                                                                          --
--  This library is free software; you can redistribute it and/or modify    --
--  it under the terms of the GNU General Public License as published by    --
--  the Free Software Foundation; either version 2 of the License, or (at   --
--  your option) any later version.                                         --
--                                                                          --
--  This library is distributed in the hope that it will be useful, but     --
--  WITHOUT ANY WARRANTY; without even the implied warranty of              --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU       --
--  General Public License for more details.                                --
--                                                                          --
--  You should have received a copy of the GNU General Public License       --
--  along with this library; if not, write to the Free Software Foundation, --
--  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.          --
--                                                                          --
--  As a special exception, if other files instantiate generics from this   --
--  unit, or you link this unit with other files to produce an executable,  --
--  this  unit  does not  by itself cause  the resulting executable to be   --
--  covered by the GNU General Public License. This exception does not      --
--  however invalidate any other reasons why the executable file  might be  --
--  covered by the  GNU Public License.                                     --
------------------------------------------------------------------------------

--  $Id$

separate (Templates_Parser)
package body Filter is

   --  Filter tokens

   Multiply_Token      : aliased constant String := """*""";
   Plus_Token          : aliased constant String := """+""";
   Minus_Token         : aliased constant String := """-""";
   Divide_Token        : aliased constant String := """/""";
   Add_Token           : aliased constant String := "ADD";
   BR_2_LF_Token       : aliased constant String := "BR_2_LF";
   Capitalize_Token    : aliased constant String := "CAPITALIZE";
   Clean_Text_Token    : aliased constant String := "CLEAN_TEXT";
   Coma_2_Point_Token  : aliased constant String := "COMA_2_POINT";
   Contract_Token      : aliased constant String := "CONTRACT";
   Div_Token           : aliased constant String := "DIV";
   Exist_Token         : aliased constant String := "EXIST";
   Format_Number_Token : aliased constant String := "FORMAT_NUMBER";
   Is_Empty_Token      : aliased constant String := "IS_EMPTY";
   LF_2_BR_Token       : aliased constant String := "LF_2_BR";
   Lower_Token         : aliased constant String := "LOWER";
   Match_Token         : aliased constant String := "MATCH";
   Modulo_Token        : aliased constant String := "MOD";
   Mult_Token          : aliased constant String := "MULT";
   No_Digit_Token      : aliased constant String := "NO_DIGIT";
   No_Letter_Token     : aliased constant String := "NO_LETTER";
   No_Space_Token      : aliased constant String := "NO_SPACE";
   Oui_Non_Token       : aliased constant String := "OUI_NON";
   Point_2_Coma_Token  : aliased constant String := "POINT_2_COMA";
   Repeat_Token        : aliased constant String := "REPEAT";
   Reverse_Token       : aliased constant String := "REVERSE";
   Size_Token          : aliased constant String := "SIZE";
   Slice_Token         : aliased constant String := "SLICE";
   Sub_Token           : aliased constant String := "SUB";
   Trim_Token          : aliased constant String := "TRIM";
   Upper_Token         : aliased constant String := "UPPER";
   Web_Escape_Token    : aliased constant String := "WEB_ESCAPE";
   Web_NBSP_Token      : aliased constant String := "WEB_NBSP";
   Yes_No_Token        : aliased constant String := "YES_NO";

   --  Filters Table

   Table : constant array (Mode) of Filter_Record
     := (Multiply       =>
           (Multiply_Token'Access,       Multiply'Access),

         Plus           =>
           (Plus_Token'Access,           Plus'Access),

         Minus          =>
           (Minus_Token'Access,          Minus'Access),

         Divide         =>
           (Divide_Token'Access,         Divide'Access),

         Add            =>
           (Add_Token'Access,            Plus'Access),

         BR_2_LF        =>
           (BR_2_LF_Token'Access,        BR_2_LF'Access),

         Capitalize     =>
           (Capitalize_Token'Access,     Capitalize'Access),

         Clean_Text     =>
           (Clean_Text_Token'Access,     Clean_Text'Access),

         Coma_2_Point   =>
           (Coma_2_Point_Token'Access,   Coma_2_Point'Access),

         Contract       =>
           (Contract_Token'Access,       Contract'Access),

         Div            =>
           (Div_Token'Access,            Divide'Access),

         Exist          =>
           (Exist_Token'Access,          Exist'Access),

         Format_Number  =>
           (Format_Number_Token'Access,  Format_Number'Access),

         Is_Empty       =>
           (Is_Empty_Token'Access,       Is_Empty'Access),

         LF_2_BR        =>
           (LF_2_BR_Token'Access,        LF_2_BR'Access),

         Lower          =>
           (Lower_Token'Access,          Lower'Access),

         Match          =>
           (Match_Token'Access,          Match'Access),

         Modulo         =>
           (Modulo_Token'Access,         Modulo'Access),

         Mult           =>
           (Mult_Token'Access,           Multiply'Access),

         No_Digit       =>
           (No_Digit_Token'Access,       No_Digit'Access),

         No_Letter      =>
           (No_Letter_Token'Access,      No_Letter'Access),

         No_Space       =>
           (No_Space_Token'Access,       No_Space'Access),

         Oui_Non        =>
           (Oui_Non_Token'Access,        Oui_Non'Access),

         Point_2_Coma   =>
           (Point_2_Coma_Token'Access,   Point_2_Coma'Access),

         Repeat         =>
           (Repeat_Token'Access,         Repeat'Access),

         Invert        =>
           (Reverse_Token'Access,        Reverse_Data'Access),

         Size           =>
           (Size_Token'Access,           Size'Access),

         Slice          =>
           (Slice_Token'Access,          Slice'Access),

         Sub            =>
           (Sub_Token'Access,            Minus'Access),

         Trim           =>
           (Trim_Token'Access,           Trim'Access),

         Upper          =>
           (Upper_Token'Access,          Upper'Access),

         Web_Escape     =>
           (Web_Escape_Token'Access,     Web_Escape'Access),

         Web_NBSP =>
           (Web_NBSP_Token'Access,       Web_NBSP'Access),

         Yes_No         =>
           (Yes_No_Token'Access,         Yes_No'Access)
         );

   --------------------------
   -- Check_Null_Parameter --
   --------------------------

   procedure Check_Null_Parameter (P : in Parameter_Data) is
   begin
      if P.Mode /= Void then
         Exceptions.Raise_Exception
           (Template_Error'Identity, "no parameter allowed in this filter");
      end if;
   end Check_Null_Parameter;

   ---------------
   -- Parameter --
   ---------------

   function Parameter (Mode : in Filter.Mode) return Parameter_Mode is
   begin
      case Mode is
         when Match  => return Regexp;
         when Slice  => return Slice;
         when others => return Str;
      end case;
   end Parameter;

   ------------
   -- Handle --
   ------------

   function Handle (Name : in String) return Callback is
      Mode : constant Filter.Mode := Mode_Value (Name);
   begin
      return Table (Mode).Handle;
   end Handle;

   function Handle (Mode : in Filter.Mode) return Callback is
   begin
      return Table (Mode).Handle;
   end Handle;

   -----------
   -- Image --
   -----------

   function Image (P : in Parameter_Data) return String is
   begin
      case P.Mode is
         when Void   => return "";
         when Str    => return '(' & To_String (P.S) & ')';
         when Regexp => return '(' & To_String (P.R_Str) & ')';
         when Slice  => return '(' & Image (P.First)
                                 & " .. " & Image (P.Last) & ')';
      end case;
   end Image;

   ----------
   -- Mode --
   ----------

   function Mode_Value (Name : in String) return Mode is
      F, L, K : Mode;
   begin
      F := Mode'First;
      L := Mode'Last;

      loop
         K := Mode'Val ((Mode'Pos (F) + Mode'Pos (L)) / 2);

         if Table (K).Name.all = Name then
            return K;

         elsif Table (K).Name.all < Name then
            F := K;
            if F /= Mode'Last then
               F := Mode'Succ (F);
            end if;

         else
            L := K;
            if L /= Mode'First then
               L := Mode'Pred (L);
            end if;
         end if;

         exit when F = L and then F = K;

      end loop;

      Exceptions.Raise_Exception
        (Internal_Error'Identity, "Unknown filter " & Name);
   end Mode_Value;

   ----------
   -- Name --
   ----------

   function Name (Handle : in Callback) return String is
   begin
      for K in Table'Range loop
         if Table (K).Handle = Handle then
            return Table (K).Name.all;
         end if;
      end loop;

      Exceptions.Raise_Exception
        (Internal_Error'Identity, "Unknown filter handle");
   end Name;

   --
   -- Filters definition start here
   --

   -------------
   -- BR_2_LF --
   -------------

   function BR_2_LF
     (S : in String; P : in Parameter_Data := No_Parameter) return String
   is
      Result : String (S'Range);
      K      : Positive := Result'First;
      J      : Positive := S'First;
   begin
      Check_Null_Parameter (P);

      loop
         if S (J) = '<'
           and then J + 3 <= S'Last
           and then Characters.Handling.To_Lower (S (J .. J + 3)) = "<br>"
         then
            Result (K) := ASCII.LF;
            K := K + 1;
            J := J + 4;
         else
            Result (K) := S (J);
            K := K + 1;
            J := J + 1;
         end if;

         exit when J > S'Last;
      end loop;

      return Result (Result'First .. K - 1);
   end BR_2_LF;

   ----------------
   -- Capitalize --
   ----------------

   function Capitalize
     (S : in String;
      P : in Parameter_Data := No_Parameter)
      return String
   is
      Result : String (S'Range);
      Upper  : Boolean := True;
   begin
      Check_Null_Parameter (P);

      for K in Result'Range loop
         if Upper then
            Result (K) := Characters.Handling.To_Upper (S (K));
            Upper := False;
         else
            Result (K) := Characters.Handling.To_Lower (S (K));
            if Result (K) = ' ' or else Result (K) = '_' then
               Upper := True;
            end if;
         end if;
      end loop;
      return Result;
   end Capitalize;

   ----------------
   -- Clean_Text --
   ----------------

   function Clean_Text
     (S : in String;
      P : in Parameter_Data := No_Parameter)
      return String
   is

      use type Strings.Maps.Character_Set;

      Result : String (S'Range);

      Clean_Set : constant Strings.Maps.Character_Set
        := Strings.Maps.Constants.Letter_Set
        or Strings.Maps.Constants.Decimal_Digit_Set
        or Strings.Maps.To_Set (" йикопафз");

   begin
      Check_Null_Parameter (P);

      for K in S'Range loop
         if Strings.Maps.Is_In (S (K), Clean_Set) then
            Result (K) := S (K);
         else
            Result (K) := ' ';
         end if;
      end loop;
      return Result;
   end Clean_Text;

   ------------------
   -- Coma_2_Point --
   ------------------

   function Coma_2_Point
     (S : in String;
      P : in Parameter_Data := No_Parameter)
      return String
   is
      Result : String := S;
   begin
      Check_Null_Parameter (P);

      for K in Result'Range loop
         if Result (K) = ',' then
            Result (K) := '.';
         end if;
      end loop;

      return Result;
   end Coma_2_Point;

   --------------
   -- Contract --
   --------------

   function Contract
     (S : in String;
      P : in Parameter_Data := No_Parameter)
      return String
   is

      use type Strings.Maps.Character_Set;

      Result : String (S'Range);
      R      : Natural := 0;
      Space  : Boolean := False;

   begin
      Check_Null_Parameter (P);

      for K in S'Range loop

         if S (K) = ' ' then

            if Space = False then
               Space := True;

               R := R + 1;
               Result (R) := ' ';
            end if;

         else
            Space := False;

            R := R + 1;
            Result (R) := S (K);
         end if;

      end loop;

      if R = 0 then
         return "";
      else
         return Result (Result'First .. R);
      end if;
   end Contract;

   -----------
   -- Exist --
   -----------

   function Exist
     (S : in String;
      P : in Parameter_Data := No_Parameter)
      return String is
   begin
      Check_Null_Parameter (P);

      if S /= "" then
         return "TRUE";
      else
         return "FALSE";
      end if;
   end Exist;

   -------------------
   -- Format_Number --
   -------------------

   function Format_Number
     (S : in String;
      P : in Parameter_Data := No_Parameter)
      return String
   is
      TS : constant String := Strings.Fixed.Trim (S, Both);

      function Is_Number return Boolean;
      --  Returns true if S is a number.

      Point : Natural := 0;

      function Is_Number return Boolean is
      begin
         for K in TS'Range loop
            if TS (K) = '.' then
               Point := K;

            elsif not Characters.Handling.Is_Digit (TS (K)) then
               return False;
            end if;
         end loop;

         return True;
      end Is_Number;

      Result : String (1 .. TS'Length * 2);
      K      : Positive := Result'Last;

      I      : Natural;
      Count  : Natural := 0;

   begin
      Check_Null_Parameter (P);

      if Is_Number then

         if Point = 0 then
            I := TS'Last;
         else
            I := Point - 1;
         end if;

         for P in reverse TS'First .. I loop
            Result (K) := TS (P);
            K := K - 1;
            Count := Count + 1;

            if Count mod 3 = 0 and then P /= TS'First then
               Result (K) := ' ';
               K := K - 1;
            end if;
         end loop;

         if Point = 0 then
            return Result (K + 1 .. Result'Last);

         else
            return Result (K + 1 .. Result'Last) & TS (Point .. TS'Last);
         end if;

      else
         return S;
      end if;
   end Format_Number;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty
     (S : in String;
      P : in Parameter_Data := No_Parameter)
      return String is
   begin
      Check_Null_Parameter (P);

      if S = "" then
         return "TRUE";
      else
         return "FALSE";
      end if;
   end Is_Empty;

   -------------
   -- LF_2_BR --
   -------------

   function LF_2_BR
     (S : in String; P : in Parameter_Data := No_Parameter) return String
   is
      N      : constant Natural
        := Fixed.Count (S, Strings.Maps.To_Set (ASCII.LF));
   begin
      Check_Null_Parameter (P);

      if N = 0 then
         --  No LF, return the original string
         return S;
      end if;

      declare
         Result : String (1 .. S'Length + N * 3);
         K      : Positive := S'First;
      begin
         for J in S'Range loop
            if S (J) = ASCII.LF then
               Result (K .. K + 3) := "<br>";
               K := K + 4;
            else
               Result (K) := S (J);
               K := K + 1;
            end if;
         end loop;

         return Result (1 .. K - 1);
      end;
   end LF_2_BR;

   -----------
   -- Lower --
   -----------

   function Lower
     (S : in String;
      P : in Parameter_Data := No_Parameter)
      return String is
   begin
      Check_Null_Parameter (P);

      return Characters.Handling.To_Lower (S);
   end Lower;

   -----------
   -- Match --
   ------------

   function Match
     (S : in String;
      P : in Parameter_Data := No_Parameter)
      return String is
   begin
      if P = No_Parameter then
         Exceptions.Raise_Exception
           (Template_Error'Identity, "missing parameter for MATCH filter");
      end if;

      if GNAT.Regexp.Match (S, P.Regexp) then
         return "TRUE";
      else
         return "FALSE";
      end if;
   end Match;

   --------------
   -- No_Digit --
   --------------

   function No_Digit
     (S : in String;
      P : in Parameter_Data := No_Parameter)
      return String
   is
      Result : String := S;
   begin
      Check_Null_Parameter (P);

      for K in S'Range loop
         if Strings.Maps.Is_In (S (K),
                                Strings.Maps.Constants.Decimal_Digit_Set)
         then
            Result (K) := ' ';
         end if;
      end loop;

      return Result;
   end No_Digit;

   ---------------
   -- No_Letter --
   ---------------

   function No_Letter
     (S : in String;
      P : in Parameter_Data := No_Parameter)
      return String
   is
      Result : String := S;
   begin
      Check_Null_Parameter (P);

      for K in S'Range loop
         if Strings.Maps.Is_In (S (K), Strings.Maps.Constants.Letter_Set) then
            Result (K) := ' ';
         end if;
      end loop;

      return Result;
   end No_Letter;

   --------------
   -- No_Space --
   --------------

   function No_Space
     (S : in String;
      P : in Parameter_Data := No_Parameter)
      return String
   is
      Result : String (S'Range);
      L      : Natural := Result'First - 1;
   begin
      Check_Null_Parameter (P);

      for K in S'Range loop
         if not (S (K) = ' ') then
            L := L + 1;
            Result (L) := S (K);
         end if;
      end loop;

      return Result (Result'First .. L);
   end No_Space;

   -------------
   -- Oui_Non --
   -------------

   function Oui_Non
     (S : in String;
      P : in Parameter_Data := No_Parameter)
      return String is
   begin
      Check_Null_Parameter (P);

      if S = "TRUE" then
         return "OUI";

      elsif S = "true" then
         return "oui";

      elsif S = "True" then
         return "Oui";

      elsif S = "FALSE" then
         return "NON";

      elsif S = "false" then
         return "non";

      elsif S = "False" then
         return "Non";

      else
         return S;
      end if;
   end Oui_Non;

   ------------------
   -- Point_2_Coma --
   ------------------

   function Point_2_Coma
     (S : in String;
      P : in Parameter_Data := No_Parameter)
      return String
   is
      Result : String := S;
   begin
      Check_Null_Parameter (P);

      for K in Result'Range loop
         if Result (K) = '.' then
            Result (K) := ',';
         end if;
      end loop;

      return Result;
   end Point_2_Coma;

   ------------
   -- Repeat --
   ------------

   function Repeat
     (S : in String;
      P : in Parameter_Data := No_Parameter)
      return String
   is
      N : Natural;
   begin
      N := Natural'Value (To_String (P.S));

      declare
         R : String (1 .. N * S'Length);
      begin
         for K in 1 .. N loop
            R (1 + (K - 1) * S'Length .. S'Length * K) := S;
         end loop;

         return R;
      end;

   exception
      when Constraint_Error =>
         Exceptions.Raise_Exception
           (Template_Error'Identity, "repeat filter parameter error");
   end Repeat;

   ------------------
   -- Reverse_Data --
   ------------------

   function Reverse_Data
     (S : in String;
      P : in Parameter_Data := No_Parameter)
      return String
   is
      Result : String (S'Range);
   begin
      Check_Null_Parameter (P);

      for K in S'Range loop
         Result (Result'Last - K + Result'First) := S (K);
      end loop;
      return Result;
   end Reverse_Data;

   ----------
   -- Size --
   ----------

   function Size
     (S : in String;
      P : in Parameter_Data := No_Parameter)
      return String is
   begin
      Check_Null_Parameter (P);

      return Image (S'Length);
   end Size;

   -----------
   -- Slice --
   -----------

   function Slice
     (S : in String;
      P : in Parameter_Data := No_Parameter)
      return String
   is
      First, Last : Natural;
   begin
      First := Natural'Min (P.First, S'Length + 1);
      Last  := Natural'Min (P.Last, S'Length);

      return S (S'First + First - 1 .. S'First + Last - 1);
   end Slice;

   ----------
   -- Trim --
   ----------

   function Trim
     (S : in String;
      P : in Parameter_Data := No_Parameter)
      return String is
   begin
      Check_Null_Parameter (P);

      return Ada.Strings.Fixed.Trim (S, Ada.Strings.Both);
   end Trim;

   -----------
   -- Upper --
   -----------

   function Upper
     (S : in String;
      P : in Parameter_Data := No_Parameter)
      return String is
   begin
      Check_Null_Parameter (P);

      return Characters.Handling.To_Upper (S);
   end Upper;

   ------------
   -- Escape --
   ------------

   function Web_Escape
     (S : in String;
      P : in Parameter_Data := No_Parameter)
      return String
   is
      Max_Escape_Sequence : constant Positive := 5;
      Result              : String (1 .. S'Length * Max_Escape_Sequence);
      Last                : Natural := 0;
   begin
      Check_Null_Parameter (P);

      for I in S'Range loop
         Last := Last + 1;

         case S (I) is
            when '&' =>
               Result (Last .. Last + 4) := "&amp;";
               Last := Last + 4;

            when '>' =>
               Result (Last .. Last + 3) := "&gt;";
               Last := Last + 3;

            when '<' =>
               Result (Last .. Last + 3) := "&lt;";
               Last := Last + 3;

            when '"' =>
               Result (Last .. Last + 5) := "&quot;";
               Last := Last + 5;

            when others =>
               Result (Last) := S (I);
         end case;

      end loop;

      return Result (1 .. Last);
   end Web_Escape;

   --------------
   -- Web_NBSP --
   --------------

   function Web_NBSP
     (S : in String;
      P : in Parameter_Data := No_Parameter)
      return String
   is
      Nbsp_Token          : constant String := "&nbsp;";
      Max_Escape_Sequence : constant Positive := Nbsp_Token'Length;
      Result              : String (1 .. S'Length * Max_Escape_Sequence);
      Last                : Natural := 0;
   begin
      Check_Null_Parameter (P);

      for I in S'Range loop
         Last := Last + 1;

         if S (I) = ' ' then
            Result (Last .. Last + Nbsp_Token'Length - 1) := Nbsp_Token;
            Last := Last + Nbsp_Token'Length - 1;
         else
            Result (Last) := S (I);
         end if;

      end loop;

      return Result (1 .. Last);
   end Web_NBSP;

   ------------
   -- Yes_No --
   ------------

   function Yes_No
     (S : in String;
      P : in Parameter_Data := No_Parameter)
      return String is
   begin
      Check_Null_Parameter (P);

      if S = "TRUE" then
         return "YES";

      elsif S = "true" then
         return "yes";

      elsif S = "True" then
         return "Yes";

      elsif S = "FALSE" then
         return "NO";

      elsif S = "false" then
         return "no";

      elsif S = "False" then
         return "No";

      else
         return S;
      end if;
   end Yes_No;

   ----------
   -- Plus --
   ----------

   function Plus
     (S : in String; P : in Parameter_Data := No_Parameter) return String
   is
      N, V : Natural;
   begin
      begin
         N := Natural'Value (To_String (P.S));
      exception
         when Constraint_Error =>
            Exceptions.Raise_Exception
              (Template_Error'Identity, """+"" filter parameter error");
      end;

      begin
         V := Natural'Value (S);
         return Image (V + N);
      exception
         when others =>
            return "";
      end;
   end Plus;

   -----------
   -- Minus --
   -----------

   function Minus
     (S : in String; P : in Parameter_Data := No_Parameter) return String
   is
      N, V : Natural;
   begin
      begin
         N := Natural'Value (To_String (P.S));
      exception
         when Constraint_Error =>
            Exceptions.Raise_Exception
              (Template_Error'Identity, """-"" filter parameter error");
      end;

      begin
         V := Natural'Value (S);
         return Image (V - N);
      exception
         when others =>
            return "";
      end;
   end Minus;

   ------------
   -- Divide --
   ------------

   function Divide
     (S : in String; P : in Parameter_Data := No_Parameter) return String
   is
      N, V : Natural;
   begin
      begin
         N := Natural'Value (To_String (P.S));
      exception
         when Constraint_Error =>
            Exceptions.Raise_Exception
              (Template_Error'Identity, """/"" filter parameter error");
      end;

      begin
         V := Natural'Value (S);
         return Image (V / N);
      exception
         when others =>
            return "";
      end;
   end Divide;

   --------------
   -- Multiply --
   --------------

   function Multiply
     (S : in String; P : in Parameter_Data := No_Parameter) return String
   is
      N, V : Natural;
   begin
      begin
         N := Natural'Value (To_String (P.S));
      exception
         when Constraint_Error =>
            Exceptions.Raise_Exception
              (Template_Error'Identity, """*"" filter parameter error");
      end;

      begin
         V := Natural'Value (S);
         return Image (V * N);
      exception
         when others =>
            return "";
      end;
   end Multiply;

   ------------
   -- Modulo --
   ------------

   function Modulo
     (S : in String; P : in Parameter_Data := No_Parameter) return String
   is
      N, V : Natural;
   begin
      begin
         N := Natural'Value (To_String (P.S));
      exception
         when Constraint_Error =>
            Exceptions.Raise_Exception
              (Template_Error'Identity, "modulo filter parameter error");
      end;

      begin
         V := Natural'Value (S);
         return Image (V mod N);
      exception
         when others =>
            return "";
      end;
   end Modulo;

end Filter;
