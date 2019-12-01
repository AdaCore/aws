------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2003-2019, AdaCore                     --
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

--  This package provides services to handle WSDL

with Ada.Characters.Handling;
with Ada.Strings.Fixed;

with DOM.Readers;
with Input_Sources.File;
with Sax.Readers;

with SOAP.Types;
with SOAP.Utils;
with SOAP.WSDL.Name_Spaces;

package body SOAP.WSDL is

   use Ada;

   procedure To_Type
     (XSD_Type : String;
      Result   : out Parameter_Type;
      Standard : out Boolean);

   --------------
   -- From_Ada --
   --------------

   procedure From_Ada
     (Ada_Type : String;
      Result   : out WSDL.Parameter_Type;
      Standard : out Boolean)
   is
      L_Type : constant String := Characters.Handling.To_Lower (Ada_Type);

   begin
      Result := P_Any_Type;
      Standard := True;

      if L_Type = "string"
        or else L_Type = "unbounded_string"
      then
         Result := P_String;

      elsif L_Type = "character" then
         Result := P_Character;

      elsif L_Type = "long"
        or else L_Type = "long_long_integer"
        or else L_Type = "soap.types.long"
      then
         Result := P_Long;

      elsif L_Type = "integer"
        or else L_Type = "long_integer"
        or else L_Type = "natural"
        or else L_Type = "positive"
      then
         Result := P_Integer;

      elsif L_Type = "short"
        or else L_Type = "short_integer"
        or else L_Type = "soap.types.short"
      then
         Result := P_Short;

      elsif L_Type = "byte"
        or else L_Type = "soap.types.byte"
      then
         Result := P_Byte;

      elsif L_Type = "float" then
         Result := P_Float;

      elsif L_Type = "long_float" or else L_Type = "long_long_float" then
         Result := P_Double;

      elsif L_Type = "duration" then
         Result := P_Duration;

      elsif L_Type = "unsigned_long"
        or else L_Type = "soap.types.unsigned_long"
      then
         Result := P_Unsigned_Long;

      elsif L_Type = "unsigned_int"
        or else L_Type = "soap.types.unsigned_int"
      then
         Result := P_Unsigned_Int;

      elsif L_Type = "unsigned_short"
        or else L_Type = "soap.types.unsigned_short"
      then
         Result := P_Unsigned_Short;

      elsif L_Type = "unsigned_byte"
        or else L_Type = "soap.types.unsigned_byte"
      then
         Result := P_Unsigned_Byte;

      elsif L_Type = "boolean" then
         Result := P_Boolean;

      elsif L_Type = "time" then
         Result := P_Time;

      else
         Standard := False;
      end if;
   end From_Ada;

   ---------------
   -- From_Type --
   ---------------

   function From_Type (P : Parameter_Type) return String is
   begin
      case P is
         when P_String         => return "string";
         when P_Long           => return "long";
         when P_Integer        => return "int";
         when P_Short          => return "short";
         when P_Byte           => return "byte";
         when P_Float          => return "float";
         when P_Double         => return "double";
         when P_Duration       => return "duration";
         when P_Boolean        => return "boolean";
         when P_Time           => return "datetime";
         when P_B64            => return "base64binary";
         when P_Character      => return "character";
         when P_Unsigned_Long  => return "unsignedlong";
         when P_Unsigned_Int   => return "unsignedint";
         when P_Unsigned_Short => return "unsignedshort";
         when P_Unsigned_Byte  => return "unsignedbyte";
         when P_Any_Type       => return "anytype";
      end case;
   end From_Type;

   -----------------
   -- Get_Routine --
   -----------------

   function Get_Routine
     (P           : Parameter_Type;
      Constrained : Boolean := False) return String is
   begin
      case P is
         when P_String =>
            if Constrained then
               return "SOAP.Utils.Get";
            else
               return "SOAP.Types.Get";
            end if;

         when P_Character =>
            return "SOAP.Utils.Get";

         when P_Long | P_Integer | P_Short | P_Byte | P_Double | P_Float
            | P_Boolean | P_Time | P_B64 | P_Any_Type | P_Unsigned_Long
            | P_Unsigned_Int | P_Unsigned_Short | P_Unsigned_Byte | P_Duration
              =>
            return "SOAP.Types.Get";
      end case;
   end Get_Routine;

   -----------------
   -- Is_Standard --
   -----------------

   function Is_Standard (XSD_Type : String) return Boolean is
      NS       : constant String := Utils.NS (XSD_Type);
      P        : Parameter_Type;
      Standard : Boolean;
   begin
      if NS = "" then
         To_Type (XSD_Type, P, Standard);
      else
         Standard := WSDL.Name_Spaces.Is_XSD (NS);
      end if;

      return Standard;
   end Is_Standard;

   ----------
   -- Load --
   ----------

   function Load (Filename : String) return Object is
      use DOM.Readers;
      use Input_Sources.File;

      Source : File_Input;
      Reader : Tree_Reader;
   begin
      Open (Filename, Source);

      --  If True, xmlns:* attributes will be reported in Start_Element
      Set_Feature (Reader, Sax.Readers.Namespace_Prefixes_Feature, True);
      Set_Feature (Reader, Sax.Readers.Validation_Feature, False);

      Parse (Reader, Source);

      Close (Source);

      return Object (DOM.Readers.Get_Tree (Reader));
   end Load;

   -----------------
   -- Set_Routine --
   -----------------

   function Set_Routine
     (P           : Parameter_Type;
      Constrained : Boolean := False) return String is
   begin
      case P is
         when P_Long           => return "SOAP.Types.L";
         when P_Integer        => return "SOAP.Types.I";
         when P_Short          => return "SOAP.Types.S";
         when P_Byte           => return "SOAP.Types.B";
         when P_Float          => return "SOAP.Types.F";
         when P_Double         => return "SOAP.Types.D";
         when P_Duration       => return "SOAP.Types.D";
         when P_Boolean        => return "SOAP.Types.B";
         when P_Time           => return "SOAP.Types.T";
         when P_B64            => return "SOAP.Types.B64";
         when P_Character      => return "SOAP.Utils.C";
         when P_Unsigned_Long  => return "SOAP.Types.UL";
         when P_Unsigned_Int   => return "SOAP.Types.UI";
         when P_Unsigned_Short => return "SOAP.Types.US";
         when P_Unsigned_Byte  => return "SOAP.Types.UB";
         when P_Any_Type       =>
            if Constrained then
               return "SOAP.Utils.Any";
            else
               return "SOAP.Types.Any";
            end if;
         when P_String         =>
            if Constrained then
               return "SOAP.Utils.US";
            else
               return "SOAP.Types.S";
            end if;
      end case;
   end Set_Routine;

   function Set_Routine
     (P           : String;
      Constrained : Boolean := False) return String is
   begin
      if Is_Standard (P) then
         return Set_Routine (To_Type (P), Constrained);
      else
         return "To_" & Utils.No_NS (P) & "_Type";
      end if;
   end Set_Routine;

   --------------
   -- Set_Type --
   --------------

   function Set_Type (P : Parameter_Type) return String is
   begin
      case P is
         when P_Long           => return "SOAP.Types.XSD_Long";
         when P_Integer        => return "SOAP.Types.XSD_Integer";
         when P_Short          => return "SOAP.Types.XSD_Short";
         when P_Byte           => return "SOAP.Types.XSD_Byte";
         when P_Float          => return "SOAP.Types.XSD_Float";
         when P_Double         => return "SOAP.Types.XSD_Double";
         when P_Duration       => return "SOAP.Types.XSD_Duration";
         when P_Boolean        => return "SOAP.Types.XSD_Boolean";
         when P_Time           => return "SOAP.Types.XSD_Time_Instant";
         when P_B64            => return "SOAP.Types.SOAP_Base64";
         when P_String         => return "SOAP.Types.XSD_String";
         when P_Character      => return "SOAP.Types.XSD_String";
         when P_Unsigned_Long  => return "SOAP.Types.XSD_Unsigned_Long";
         when P_Unsigned_Int   => return "SOAP.Types.XSD_Unsigned_Int";
         when P_Unsigned_Short => return "SOAP.Types.XSD_Unsigned_Short";
         when P_Unsigned_Byte  => return "SOAP.Types.XSD_Unsigned_Byte";
         when P_Any_Type       => return "SOAP.Types.XSD_Any_Type";
      end case;
   end Set_Type;

   ------------
   -- To_Ada --
   ------------

   function To_Ada
     (P           : Parameter_Type;
      Constrained : Boolean := False) return String is
   begin
      case P is
         when P_Long           => return "SOAP.Types.Long";
         when P_Integer        => return "Integer";
         when P_Short          => return "SOAP.Types.Short";
         when P_Byte           => return "SOAP.Types.Byte";
         when P_Float          => return "Float";
         when P_Double         => return "Long_Float";
         when P_Duration       => return "Duration";
         when P_Boolean        => return "Boolean";
         when P_Time           => return "SOAP.Types.Local_Time";
         when P_B64            => return "String";
         when P_Character      => return "Character";
         when P_Unsigned_Long  => return "SOAP.Types.Unsigned_Long";
         when P_Unsigned_Int   => return "SOAP.Types.Unsigned_Int";
         when P_Unsigned_Short => return "SOAP.Types.Unsigned_Short";
         when P_Unsigned_Byte  => return "SOAP.Types.Unsigned_Byte";
         when P_Any_Type       =>
            if Constrained then
               return "SOAP.Types.XSD_Any_Type";
            else
               return "SOAP.Types.Object'Class";
            end if;
         when P_String         =>
            if Constrained then
               return "Unbounded_String";
            else
               return "String";
            end if;
      end case;
   end To_Ada;

   -------------
   -- To_Type --
   -------------

   procedure To_Type
     (XSD_Type : String;
      Result   : out Parameter_Type;
      Standard : out Boolean)
   is
      Name   : constant String := Characters.Handling.To_Lower (XSD_Type);
      C      : constant Natural := Strings.Fixed.Index (Name, ":");
      XSD    : constant String :=
                 (if C = 0 then "" else Name (Name'First .. C - 1));
      L_Type : constant String :=
                 (if C = 0 then Name else Name (C + 1 .. Name'Last));
   begin
      Result := P_Any_Type;
      Standard := True;

      if XSD /= "" and then not WSDL.Name_Spaces.Is_XSD (XSD) then
         Standard := False;
         return;
      end if;

      if L_Type = "string" then
         Result := P_String;

      elsif L_Type = "long" then
         Result := P_Long;

      elsif L_Type = "integer" or else L_Type = "int" then
         Result := P_Integer;

      elsif L_Type = "short" then
         Result := P_Short;

      elsif L_Type = "byte" then
         Result := P_Byte;

      elsif L_Type = "float" then
         Result := P_Float;

      elsif L_Type = "double" then
         Result := P_Double;

      elsif L_Type = "duration" then
         Result := P_Duration;

      elsif L_Type = "boolean" then
         Result := P_Boolean;

      elsif L_Type = "timeinstant" or else L_Type = "datetime" then
         Result := P_Time;

      elsif L_Type = "base64binary" then
         Result := P_B64;

      elsif L_Type = "character" then
         Result := P_Character;

      elsif L_Type = "unsignedlong" then
         Result := P_Unsigned_Long;

      elsif L_Type = "unsignedint" then
         Result := P_Unsigned_Int;

      elsif L_Type = "unsignedshort" then
         Result := P_Unsigned_Short;

      elsif L_Type = "unsignedbyte" then
         Result := P_Unsigned_Byte;

      elsif L_Type = "anytype" then
         Result := P_Any_Type;

      else
         Standard := False;
      end if;
   end To_Type;

   function To_Type (XSD_Type : String) return Parameter_Type is
      Result   : Parameter_Type;
      Standard : Boolean;
   begin
      To_Type (Utils.No_NS (XSD_Type), Result, Standard);

      if not Standard then
         raise WSDL_Error
           with "(To_Type) Type " & XSD_Type & " not supported.";
      end if;

      return Result;
   end To_Type;

   ------------
   -- To_XSD --
   ------------

   function To_XSD (P : WSDL.Parameter_Type) return String is
      use SOAP.Types;
   begin
      case P is
         when P_Long           => return XML_Long;
         when P_Integer        => return XML_Int;
         when P_Short          => return XML_Short;
         when P_Byte           => return XML_Byte;
         when P_Float          => return XML_Float;
         when P_Double         => return XML_Double;
         when P_Duration       => return XML_Duration;
         when P_Boolean        => return XML_Boolean;
         when P_Time           => return XML_Time_Instant;
         when P_B64            => return "xsd:base64";
         when P_String         => return XML_String;
         when P_Character      => return "Character";
         when P_Unsigned_Long  => return XML_Unsigned_Long;
         when P_Unsigned_Int   => return XML_Unsigned_Int;
         when P_Unsigned_Short => return XML_Unsigned_Short;
         when P_Unsigned_Byte  => return XML_Unsigned_Byte;
         when P_Any_Type       => return XML_Any_Type;
      end case;
   end To_XSD;

   ---------------
   -- V_Routine --
   ---------------

   function V_Routine
     (P           : Parameter_Type;
      Constrained : Boolean := False) return String is
   begin
      case P is
         when P_String =>
            if Constrained then
               return "SOAP.Utils.V";
            else
               return "SOAP.Types.V";
            end if;

         when P_Character =>
            return "SOAP.Utils.V";

         when P_Long | P_Integer | P_Short | P_Byte | P_Double | P_Float
            | P_Boolean | P_Time | P_B64 | P_Unsigned_Long | P_Unsigned_Int
            | P_Unsigned_Short | P_Unsigned_Byte | P_Any_Type | P_Duration
              =>
            return "SOAP.Types.V";
      end case;
   end V_Routine;

end SOAP.WSDL;
