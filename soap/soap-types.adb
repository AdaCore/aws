------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2000-2001                          --
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

with Ada.Long_Float_Text_IO;
with Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Tags;
with Ada.Unchecked_Deallocation;

with AWS.Utils;
with GNAT.Calendar.Time_IO;

with SOAP.Utils;

package body SOAP.Types is

   use Ada;

   function xsi_type (Name : in String) return String;
   --  Returns the xsi:type field for the XML type representation whose name
   --  is passed as argument.

   ---------
   -- "+" --
   ---------

   function "+" (O : in Object'Class) return Object_Controlled is
   begin
      return (Finalization.Controlled with new Object'Class'(O));
   end "+";

   -------
   -- A --
   -------

   function A
     (V    : in Object_Set;
      Name : in String)
     return SOAP_Array is
   begin
      return (To_Unbounded_String (Name),
              (Finalization.Controlled with new Object_Set'(V)));
   end A;

   ------------
   -- Adjust --
   ------------

   procedure Adjust (O : in out Object_Controlled) is
   begin
      if O.O /= null then
         O.O := new Object'Class'(O.O.all);
      end if;
   end Adjust;

   procedure Adjust (O : in out Object_Set_Controlled) is
   begin
      if O.O /= null then
         O.O := new Object_Set'(O.O.all);
      end if;
   end Adjust;

   -------
   -- B --
   -------

   function B
     (V    : in Boolean;
      Name : in String  := "item")
     return XSD_Boolean is
   begin
      return (To_Unbounded_String (Name), V);
   end B;

   ---------
   -- B64 --
   ---------

   function B64
     (V      : in String;
      Name   : in String  := "item")
     return SOAP_Base64 is
   begin
      return (To_Unbounded_String (Name), To_Unbounded_String (V));
   end B64;

   -------
   -- F --
   -------

   function F
     (V    : in Long_Float;
      Name : in String := "item")
     return XSD_Float is
   begin
      return (To_Unbounded_String (Name), V);
   end F;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (O : in out Object_Controlled) is
      procedure Free is
         new Ada.Unchecked_Deallocation (Object'Class, Object_Access);
   begin
      if O.O /= null then
         Free (O.O);
      end if;
   end Finalize;

   procedure Finalize (O : in out Object_Set_Controlled) is
      procedure Free is
         new Ada.Unchecked_Deallocation (Object_Set, Object_Set_Access);
   begin
      if O.O /= null then
         Free (O.O);
      end if;
   end Finalize;

   ---------
   -- Get --
   ---------

   function Get (O : in Object'Class) return Integer is
      use type Ada.Tags.Tag;
   begin
      if O'Tag = Types.XSD_Integer'Tag then
         return V (XSD_Integer (O));

      else
         Exceptions.Raise_Exception
           (Data_Error'Identity,
            "Integer expected, found " & Tags.Expanded_Name (O'Tag));
      end if;
   end Get;

   function Get (O : in Object'Class) return Long_Float is
      use type Ada.Tags.Tag;
   begin
      if O'Tag = Types.XSD_Float'Tag then
         return V (XSD_Float (O));

      else
         Exceptions.Raise_Exception
           (Data_Error'Identity,
            "Float expected, found " & Tags.Expanded_Name (O'Tag));
      end if;
   end Get;

   function Get (O : in Object'Class) return String is
      use type Ada.Tags.Tag;
   begin
      if O'Tag = Types.XSD_String'Tag then
         return V (XSD_String (O));

      else
         Exceptions.Raise_Exception
           (Data_Error'Identity,
            "String expected, found " & Tags.Expanded_Name (O'Tag));
      end if;
   end Get;

   function Get (O : in Object'Class) return Boolean is
      use type Ada.Tags.Tag;
   begin
      if O'Tag = Types.XSD_Boolean'Tag then
         return V (XSD_Boolean (O));

      else
         Exceptions.Raise_Exception
           (Data_Error'Identity,
            "Boolean expected, found " & Tags.Expanded_Name (O'Tag));
      end if;
   end Get;

   function Get (O : in Object'Class) return SOAP_Record is
      use type Ada.Tags.Tag;
   begin
      if O'Tag = Types.SOAP_Record'Tag then
         return SOAP_Record (O);

      else
         Exceptions.Raise_Exception
           (Data_Error'Identity,
            "SOAP Struct expected, found " & Tags.Expanded_Name (O'Tag));
      end if;
   end Get;

   function Get (O : in Object'Class) return SOAP_Array is
      use type Ada.Tags.Tag;
   begin
      if O'Tag = Types.SOAP_Array'Tag then
         return SOAP_Array (O);

      else
         Exceptions.Raise_Exception
           (Data_Error'Identity,
            "SOAP Array expected, found " & Tags.Expanded_Name (O'Tag));
      end if;
   end Get;

   -------
   -- I --
   -------

   function I
     (V    : in Integer;
      Name : in String := "item")
     return XSD_Integer is
   begin
      return (To_Unbounded_String (Name), V);
   end I;

   -----------
   -- Image --
   -----------

   function Image (O : in Object) return String is
   begin
      return "";
   end Image;

   function Image (O : in XSD_Integer) return String is
      V : constant String := Integer'Image (O.V);
   begin
      if O.V >= 0 then
         return V (V'First + 1 .. V'Last);
      else
         return V;
      end if;
   end Image;

   function Image (O : in XSD_Float) return String is
      use Ada;

      Result : String (1 .. Long_Float'Width);
   begin
      Long_Float_Text_IO.Put (Result, O.V, Exp => 0);
      return Strings.Fixed.Trim (Result, Strings.Both);
   end Image;

   function Image (O : in XSD_String) return String is
   begin
      return To_String (O.V);
   end Image;

   function Image (O : in XSD_Boolean) return String is
   begin
      if O.V then
         return "1";
      else
         return "0";
      end if;
   end Image;

   function Image (O : in XSD_Time_Instant) return String is

      function Image (Timezone : in TZ) return String;
      --  Returns Image for the TZ

      function Image (Timezone : in TZ) return String is

         subtype Str2 is String (1 .. 2);

         function I2D (N : Natural) return Str2;

         function I2D (N : Natural) return Str2 is
            V : constant String := Natural'Image (N);
         begin
            if N > 9 then
               return V (V'First + 1 .. V'Last);
            else
               return '0' & V (V'First + 1 .. V'Last);
            end if;
         end I2D;

      begin
         if Timezone >= 0 then
            return '+' & I2D (Timezone) & ":00";
         else
            return '-' & I2D (abs Timezone) & ":00";
         end if;
      end Image;

   begin
      return GNAT.Calendar.Time_IO.Image (O.T, "%Y-%m-%dT%H:%M:%S")
        & Image (O.Timezone);
   end Image;

   function Image (O : in SOAP_Base64) return String is
   begin
      return To_String (O.V);
   end Image;

   function Image (O : in SOAP_Array) return String is
      Result : Unbounded_String;
   begin
      Append (Result, '(');

      for K in O.Items.O'Range loop
         Append (Result, Integer'Image (K));
         Append (Result, " => ");
         Append (Result, Image (O.Items.O (K).O.all));

         if K /= O.Items.O'Last then
            Append (Result, ", ");
         end if;
      end loop;

      Append (Result, ')');

      return To_String (Result);
   end Image;

   function Image (O : in SOAP_Record) return String is
      Result : Unbounded_String;
   begin
      Append (Result, '(');

      for K in O.Items.O'Range loop
         Append (Result, Name (O));
         Append (Result, " => ");
         Append (Result, Image (O.Items.O (K).O.all));

         if K /= O.Items.O'Last then
            Append (Result, ", ");
         end if;
      end loop;

      Append (Result, ')');

      return To_String (Result);
   end Image;

   -------
   -- N --
   -------

   function N (Name : in String  := "item") return XSD_Null is
   begin
      return (Name => To_Unbounded_String (Name));
   end N;

   ----------
   -- Name --
   ----------

   function Name (O : in Object'Class) return String is
   begin
      return To_String (O.Name);
   end Name;

   -------
   -- R --
   -------

   function R
     (V    : in Object_Set;
      Name : in String)
     return SOAP_Record is
   begin
      return (To_Unbounded_String (Name),
              (Finalization.Controlled with new Object_Set'(V)));
   end R;

   -------
   -- S --
   -------

   function S
     (V      : in String;
      Name   : in String  := "item";
      Encode : in Boolean := True)
     return XSD_String is
   begin
      if Encode then
         return (To_Unbounded_String (Name),
                 To_Unbounded_String (Utils.Encode (V)));
      else
         return (To_Unbounded_String (Name),
                 To_Unbounded_String (V));
      end if;
   end S;

   -------
   -- T --
   -------

   function T
     (V        : in Calendar.Time;
      Name     : in String        := "item";
      Timezone : in TZ            := GMT)
     return XSD_Time_Instant is
   begin
      return (To_Unbounded_String (Name), V, Timezone);
   end T;

   -------
   -- V --
   -------

   function V (O : in XSD_Integer) return Integer is
   begin
      return O.V;
   end V;

   function V (O : in XSD_Float) return Long_Float is
   begin
      return O.V;
   end V;

   function V (O : in XSD_String) return String is
   begin
      return To_String (O.V);
   end V;

   function V (O : in XSD_Boolean) return Boolean is
   begin
      return O.V;
   end V;

   function V (O : in XSD_Time_Instant) return Calendar.Time is
   begin
      return O.T;
   end V;

   function V (O : in SOAP_Base64) return String is
   begin
      return To_String (O.V);
   end V;

   function V (O : in SOAP_Array) return Object_Set is
   begin
      return O.Items.O.all;
   end V;

   function V (O : in SOAP_Record; Name : in String) return Object'Class is
   begin
      for K in O.Items.O'Range loop
         if Types.Name (O.Items.O (K).O.all) = Name then
            return O.Items.O (K).O.all;
         end if;
      end loop;

      Exceptions.Raise_Exception
        (Types.Data_Error'Identity,
         "(V) Struct object " & Name & " not found");
   end V;

   ---------------
   -- XML_Image --
   ---------------

   function XML_Image (O : in Object) return String is
      OC : constant Object'Class := Object'Class (O);
   begin
      return "<" & Name (OC) & xsi_type (XML_Type (OC)) & '>'
        & Image (OC)
        & "</" & Name (OC) & '>';
   end XML_Image;

   function XML_Image (O : in XSD_Integer) return String is
   begin
      return XML_Image (Object (O));
   end XML_Image;

   function XML_Image (O : in XSD_Float) return String is
   begin
      return XML_Image (Object (O));
   end XML_Image;

   function XML_Image (O : in XSD_String) return String is
   begin
      return XML_Image (Object (O));
   end XML_Image;

   function XML_Image (O : in XSD_Boolean) return String is
   begin
      return XML_Image (Object (O));
   end XML_Image;

   function XML_Image (O : in XSD_Time_Instant) return String is
   begin
      return XML_Image (Object (O));
   end XML_Image;

   function XML_Image (O : in XSD_Null) return String is
      OC : constant Object'Class := Object'Class (O);
   begin
      return "<" & Name (OC) & " xsi_null=""1""/>";
   end XML_Image;

   function XML_Image (O : in SOAP_Base64) return String is
   begin
      return XML_Image (Object (O));
   end XML_Image;

   New_Line : constant String := ASCII.CR & ASCII.LF;

   function XML_Image (O : in SOAP_Array) return String is

      function Array_Type return String;
      --  Returns the right SOAP array type.

      function Array_Type return String is
         use type Ada.Tags.Tag;

         T         : Ada.Tags.Tag;
         Same_Type : Boolean := True;
      begin
         T := O.Items.O (O.Items.O'First).O'Tag;

         for K in O.Items.O'First + 1 .. O.Items.O'Last loop
            if T /= O.Items.O (K).O'Tag then
               Same_Type := False;
               exit;
            end if;
         end loop;

         if Same_Type then
            return XML_Type (O.Items.O (O.Items.O'First).O.all);

         else
            return XML_Undefined;
         end if;
      end Array_Type;

      Result : Unbounded_String;
   begin
      --  Open array element

      Append (Result, '<');
      Append (Result, O.Name);
      Append (Result, " SOAP-ENC:arrayType=""");
      Append (Result, Array_Type);
      Append (Result, '[');
      Append (Result, AWS.Utils.Image (O.Items.O'Length));
      Append (Result, "]"" ");
      Append (Result, xsi_type (XML_Array));
      Append (Result, '>');
      Append (Result, New_Line);

      --  Add all elements

      for K in O.Items.O'Range loop
         Append (Result, XML_Image (O.Items.O (K).O.all));
         Append (Result, New_Line);
      end loop;

      --  End array element

      Append (Result, Utils.Tag (To_String (O.Name), Start => False));

      return To_String (Result);
   end XML_Image;

   function XML_Image (O : in SOAP_Record) return String is
      Result : Unbounded_String;
   begin
      Append (Result, Utils.Tag (Name (O), Start => True));
      Append (Result, New_Line);

      for K in O.Items.O'Range loop
         Append (Result, XML_Image (O.Items.O (K).O.all));
         Append (Result, New_Line);
      end loop;

      Append (Result, Utils.Tag (Name (O), Start => False));

      return To_String (Result);
   end XML_Image;

   --------------
   -- XML_Type --
   --------------

   function XML_Type (O : in Object) return String is
   begin
      return "";
   end XML_Type;

   function XML_Type (O : in XSD_Integer) return String is
   begin
      return XML_Int;
   end XML_Type;

   function XML_Type (O : in XSD_Float) return String is
   begin
      return XML_Float;
   end XML_Type;

   function XML_Type (O : in XSD_String) return String is
   begin
      return XML_String;
   end XML_Type;

   function XML_Type (O : in XSD_Boolean) return String is
   begin
      return XML_Boolean;
   end XML_Type;

   function XML_Type  (O : in XSD_Time_Instant) return String is
   begin
      return XML_Time_Instant;
   end XML_Type;

   function XML_Type (O : in XSD_Null) return String is
   begin
      return XML_Null;
   end XML_Type;

   function XML_Type (O : in SOAP_Base64) return String is
   begin
      return XML_Base64;
   end XML_Type;

   function XML_Type (O : in SOAP_Array) return String is
   begin
      return XML_Array;
   end XML_Type;

   function XML_Type  (O : in SOAP_Record) return String is
   begin
      return "";
   end XML_Type;

   --------------
   -- xsi_type --
   --------------

   function xsi_type (Name : in String) return String is
   begin
      return " xsi:type=""" & Name & '"';
   end xsi_type;

end SOAP.Types;
