------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2003-2004                          --
--                                ACT-Europe                                --
--                                                                          --
--  Authors: Dmitriy Anisimkov - Pascal Obry                                --
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

--  This package provides services to handle WSDL.

with Ada.Exceptions;
with Ada.Characters.Handling;

with Input_Sources.File;
with Sax.Readers;
with DOM.Readers;

with SOAP.Types;

package body SOAP.WSDL is

   use Ada;

   procedure To_Type
     (XSD_Type : in     String;
      Result   :    out Parameter_Type;
      Standard :    out Boolean);

   --------------
   -- From_Ada --
   --------------

   procedure From_Ada
     (Ada_Type : in     String;
      Result   :    out WSDL.Parameter_Type;
      Standard :    out Boolean)
   is
      use SOAP.WSDL;

      L_Type : constant String := Characters.Handling.To_Lower (Ada_Type);

   begin
      Standard := True;

      if L_Type = "string"
        or else L_Type = "unbounded_string"
      then
         Result := P_String;

      elsif L_Type = "character" then
         Result := P_Character;

      elsif L_Type = "integer"
        or else L_Type = "natural"
        or else L_Type = "positive"
      then
         Result := P_Integer;

      elsif L_Type = "long" then
         Result := P_Long;

      elsif L_Type = "float" or else L_Type = "long_float" then
         Result := P_Float;

      elsif L_Type = "long_long_float" then
         Result := P_Double;

      elsif L_Type = "boolean" then
         Result := P_Boolean;

      elsif L_Type = "time" then
         Result := P_Time;

      else
         Standard := False;
      end if;
   end From_Ada;

   -----------------
   -- Get_Routine --
   -----------------

   function Get_Routine
     (P       : in Parameter_Type;
      Context : in Context_Type := Parameter)
      return String is
   begin
      case P is
         when P_String =>
            if Context = Component then
               return "SOAP.Utils.Get";
            else
               return "SOAP.Types.Get";
            end if;

         when P_Character =>
            return "SOAP.Utils.Get";

         when P_Integer | P_Long | P_Double | P_Float | P_Boolean
           | P_Time | P_B64
           =>
            return "SOAP.Types.Get";
      end case;
   end Get_Routine;

   -----------------
   -- Is_Standard --
   -----------------

   function Is_Standard (XSD_Type : in String) return Boolean is
      P        : Parameter_Type;
      Standard : Boolean;
   begin
      To_Type (XSD_Type, P, Standard);

      return Standard;
   end Is_Standard;

   ----------
   -- Load --
   ----------

   function Load (Filename : in String) return Object is
      use Input_Sources.File;
      use DOM.Readers;

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
     (P       : in Parameter_Type;
      Context : in Context_Type := Parameter)
      return String is
   begin
      case P is
         when P_Integer   => return "SOAP.Types.I";
         when P_Long      => return "SOAP.Types.L";
         when P_Float     => return "SOAP.Types.F";
         when P_Double    => return "SOAP.Types.D";
         when P_Boolean   => return "SOAP.Types.B";
         when P_Time      => return "SOAP.Types.T";
         when P_B64       => return "SOAP.Types.B64";
         when P_Character => return "SOAP.Utils.C";
         when P_String  =>
            if Context = Parameter then
               return "SOAP.Types.S";
            else
               return "SOAP.Utils.US";
            end if;
      end case;
   end Set_Routine;

   --------------
   -- Set_Type --
   --------------

   function Set_Type (P : in Parameter_Type) return String is
   begin
      case P is
         when P_Integer   => return "SOAP.Types.XSD_Integer";
         when P_Long      => return "SOAP.Types.XSD_Long";
         when P_Float     => return "SOAP.Types.XSD_Float";
         when P_Double    => return "SOAP.Types.XSD_Double";
         when P_Boolean   => return "SOAP.Types.XSD_Boolean";
         when P_Time      => return "SOAP.Types.XSD_Time_Instant";
         when P_B64       => return "SOAP.Types.SOAP_Base64";
         when P_String    => return "SOAP.Types.XSD_String";
         when P_Character => return "SOAP.Types.SOAP_Enumeration";
      end case;
   end Set_Type;

   ------------
   -- To_Ada --
   ------------

   function To_Ada
     (P       : in Parameter_Type;
      Context : in Context_Type := Parameter)
      return String is
   begin
      case P is
         when P_Integer    => return "Integer";
         when P_Long       => return "SOAP.Types.Long";
         when P_Float      => return "Long_Float";
         when P_Double     => return "Long_Long_Float";
         when P_Boolean    => return "Boolean";
         when P_Time       => return "Ada.Calendar.Time";
         when P_B64        => return "String";
         when P_Character  => return "Character";
         when P_String  =>
            if Context = Parameter then
               return "String";
            else
               return "Unbounded_String";
            end if;
      end case;
   end To_Ada;

   -------------
   -- To_Type --
   -------------

   procedure To_Type
     (XSD_Type : in     String;
      Result   :    out Parameter_Type;
      Standard :    out Boolean)
   is
      L_Type : constant String := Characters.Handling.To_Lower (XSD_Type);
   begin
      Standard := True;

      if L_Type = "string" then
         Result := P_String;

      elsif L_Type = "integer" or else L_Type = "int" then
         Result := P_Integer;

      elsif L_Type = "long" then
         Result := P_Long;

      elsif L_Type = "float" then
         Result := P_Float;

      elsif L_Type = "double" then
         Result := P_Double;

      elsif L_Type = "boolean" then
         Result := P_Boolean;

      elsif L_Type = "timeinstant" or else L_Type = "datetime" then
         Result := P_Time;

      elsif L_Type = "base64binary" then
         Result := P_B64;

      elsif L_Type = "character" then
         Result := P_Character;

      else
         Standard := False;
      end if;
   end To_Type;

   function To_Type (XSD_Type : in String) return Parameter_Type is
      Result   : Parameter_Type;
      Standard : Boolean;
   begin
      To_Type (XSD_Type, Result, Standard);

      if not Standard then
         Exceptions.Raise_Exception
           (WSDL_Error'Identity,
            "(To_Type) Type " & XSD_Type & " not supported.");
      end if;

      return Result;
   end To_Type;

   ------------
   -- To_XSD --
   ------------

   function To_XSD (P : in WSDL.Parameter_Type) return String is
      use SOAP.WSDL;
      use SOAP.Types;
   begin
      case P is
         when P_Integer   => return XML_Int;
         when P_Long      => return XML_Long;
         when P_Float     => return XML_Float;
         when P_Double    => return XML_Double;
         when P_Boolean   => return XML_Boolean;
         when P_Time      => return XML_Time_Instant;
         when P_B64       => return "xsd:base64";
         when P_String    => return XML_String;
         when P_Character => return "Character";
      end case;
   end To_XSD;

   ---------------
   -- V_Routine --
   ---------------

   function V_Routine
     (P       : in Parameter_Type;
      Context : in Context_Type := Parameter)
      return String is
   begin
      case P is
         when P_String =>
            if Context = Component then
               return "SOAP.Utils.V";
            else
               return "SOAP.Types.V";
            end if;

         when P_Character =>
            return "SOAP.Utils.V";

         when P_Integer | P_Long | P_Double | P_Float | P_Boolean
           | P_Time | P_B64
           =>
            return "SOAP.Types.V";
      end case;
   end V_Routine;

end SOAP.WSDL;
