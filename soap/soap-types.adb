------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2000-2004                          --
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
with Ada.Long_Long_Float_Text_IO;
with Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Tags;
with Ada.Task_Attributes;
with Ada.Unchecked_Deallocation;

with AWS.Utils;
with GNAT.Calendar.Time_IO;

with SOAP.Utils;

package body SOAP.Types is

   use Ada;

   procedure Free is
      new Ada.Unchecked_Deallocation (Object_Set, Object_Set_Access);

   procedure Free is
      new Ada.Unchecked_Deallocation (Natural, Counter_Access);

   function xsi_type (Name : in String) return String;
   pragma Inline (xsi_type);
   --  Returns the xsi:type field for the XML type representation whose name
   --  is passed as argument.

   function Spaces (N : in Natural) return String;
   pragma Inline (Spaces);
   --  Returns N * 3 spaces.

   package XML_Indent is new Ada.Task_Attributes (Natural, 0);
   --  Thread safe Indentation counter.

   ---------
   -- "+" --
   ---------

   function "+" (O : in Object'Class) return Object_Safe_Pointer is
   begin
      return (Finalization.Controlled with new Object'Class'(O));
   end "+";

   -------
   -- - --
   -------

   function "-" (O : in Object_Safe_Pointer) return Object'Class is
   begin
      return O.O.all;
   end "-";

   -------
   -- A --
   -------

   function A
     (V         : in Object_Set;
      Name      : in String;
      Type_Name : in String := "")
      return SOAP_Array is
   begin
      return
        (Finalization.Controlled
         with To_Unbounded_String (Name),
           new Natural'(1), new Object_Set'(V),
           To_Unbounded_String (Type_Name));
   end A;

   ------------
   -- Adjust --
   ------------

   procedure Adjust (O : in out Object_Safe_Pointer) is
   begin
      if O.O /= null then
         O.O := new Object'Class'(O.O.all);
      end if;
   end Adjust;

   procedure Adjust (O : in out Composite) is
   begin
      O.Ref_Counter.all := O.Ref_Counter.all + 1;
   end Adjust;

   -------
   -- B --
   -------

   function B
     (V    : in Boolean;
      Name : in String  := "item")
      return XSD_Boolean is
   begin
      return (Finalization.Controlled with To_Unbounded_String (Name), V);
   end B;

   ---------
   -- B64 --
   ---------

   function B64
     (V      : in String;
      Name   : in String  := "item")
      return SOAP_Base64 is
   begin
      return (Finalization.Controlled
                with To_Unbounded_String (Name), To_Unbounded_String (V));
   end B64;

   -------
   -- D --
   -------

   function D
     (V    : in Long_Long_Float;
      Name : in String          := "item")
      return XSD_Double is
   begin
      return (Finalization.Controlled with To_Unbounded_String (Name), V);
   end D;

   -------
   -- E --
   -------

   function E
     (V         : in String;
      Type_Name : in String;
      Name      : in String := "item")
      return SOAP_Enumeration is
   begin
      return (Finalization.Controlled
                with To_Unbounded_String (Name),
                     To_Unbounded_String (V),
                     To_Unbounded_String (Type_Name));
   end E;

   -------
   -- F --
   -------

   function F
     (V    : in Long_Float;
      Name : in String := "item")
      return XSD_Float is
   begin
      return (Finalization.Controlled with To_Unbounded_String (Name), V);
   end F;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (O : in out Object_Safe_Pointer) is
      procedure Free is
         new Ada.Unchecked_Deallocation (Object'Class, Object_Access);
   begin
      if O.O /= null then
         Free (O.O);
      end if;
   end Finalize;

   procedure Finalize (O : in out Composite) is
   begin
      O.Ref_Counter.all := O.Ref_Counter.all - 1;

      if O.Ref_Counter.all = 0 then
         Free (O.O);
         Free (O.Ref_Counter);
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

   function Get (O : in Object'Class) return Long is
      use type Ada.Tags.Tag;
   begin
      if O'Tag = Types.XSD_Long'Tag then
         return V (XSD_Long (O));

      else
         Exceptions.Raise_Exception
           (Data_Error'Identity,
            "Long expected, found " & Tags.Expanded_Name (O'Tag));
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

   function Get (O : in Object'Class) return Long_Long_Float is
      use type Ada.Tags.Tag;
   begin
      if O'Tag = Types.XSD_Double'Tag then
         return V (XSD_Double (O));

      else
         Exceptions.Raise_Exception
           (Data_Error'Identity,
            "Double expected, found " & Tags.Expanded_Name (O'Tag));
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

   function Get (O : in Object'Class) return Unbounded_String is
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

   function Get (O : in Object'Class) return Ada.Calendar.Time is
      use type Ada.Tags.Tag;
   begin
      if O'Tag = Types.XSD_Time_Instant'Tag then
         return V (XSD_Time_Instant (O));

      else
         Exceptions.Raise_Exception
           (Data_Error'Identity,
            "timeInstant expected, found " & Tags.Expanded_Name (O'Tag));
      end if;
   end Get;

   function Get (O : in Object'Class) return SOAP_Base64 is
      use type Ada.Tags.Tag;
   begin
      if O'Tag = Types.SOAP_Base64'Tag then
         return SOAP_Base64 (O);

      else
         Exceptions.Raise_Exception
           (Data_Error'Identity,
            "SOAP Base64 expected, found " & Tags.Expanded_Name (O'Tag));
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
      return (Finalization.Controlled with To_Unbounded_String (Name), V);
   end I;

   -----------
   -- Image --
   -----------

   function Image (O : in Object) return String is
      pragma Warnings (Off, O);
   begin
      return "";
   end Image;

   -----------
   -- Image --
   -----------

   function Image (O : in XSD_Integer) return String is
      V : constant String := Integer'Image (O.V);
   begin
      if O.V >= 0 then
         return V (V'First + 1 .. V'Last);
      else
         return V;
      end if;
   end Image;

   -----------
   -- Image --
   -----------

   function Image (O : in XSD_Long) return String is
      V : constant String := Long'Image (O.V);
   begin
      if O.V >= 0 then
         return V (V'First + 1 .. V'Last);
      else
         return V;
      end if;
   end Image;

   -----------
   -- Image --
   -----------

   function Image (O : in XSD_Float) return String is
      use Ada;

      Result : String (1 .. Long_Float'Width);
   begin
      Long_Float_Text_IO.Put (Result, O.V, Exp => 0);
      return Strings.Fixed.Trim (Result, Strings.Both);
   end Image;

   -----------
   -- Image --
   -----------

   function Image (O : in XSD_Double) return String is
      use Ada;

      Result : String (1 .. Long_Long_Float'Width);
   begin
      Long_Long_Float_Text_IO.Put (Result, O.V, Exp => 0);
      return Strings.Fixed.Trim (Result, Strings.Both);
   end Image;

   -----------
   -- Image --
   -----------

   function Image (O : in XSD_String) return String is
   begin
      return To_String (O.V);
   end Image;

   -----------
   -- Image --
   -----------

   function Image (O : in XSD_Boolean) return String is
   begin
      if O.V then
         return "1";
      else
         return "0";
      end if;
   end Image;

   -----------
   -- Image --
   -----------

   function Image (O : in XSD_Time_Instant) return String is

      function Image (Timezone : in TZ) return String;
      --  Returns Image for the TZ

      -----------
      -- Image --
      -----------

      function Image (Timezone : in TZ) return String is

         subtype Str2 is String (1 .. 2);

         function I2D (N : in Natural) return Str2;
         --  Returns N image with 2 characters padding with 0 is needed

         ---------
         -- I2D --
         ---------

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
         if Timezone = 0 then
            return "Z";
         elsif Timezone >= 0 then
            return '+' & I2D (Timezone) & ":00";
         else
            return '-' & I2D (abs Timezone) & ":00";
         end if;
      end Image;

   begin
      return GNAT.Calendar.Time_IO.Image (O.T, "%Y-%m-%dT%H:%M:%S")
        & Image (O.Timezone);
   end Image;

   -----------
   -- Image --
   -----------

   function Image (O : in SOAP_Base64) return String is
   begin
      return To_String (O.V);
   end Image;

   -----------
   -- Image --
   -----------

   function Image (O : in SOAP_Array) return String is
      Result : Unbounded_String;
   begin
      Append (Result, '(');

      for K in O.O'Range loop
         Append (Result, Integer'Image (K));
         Append (Result, " => ");
         Append (Result, Image (O.O (K).O.all));

         if K /= O.O'Last then
            Append (Result, ", ");
         end if;
      end loop;

      Append (Result, ')');

      return To_String (Result);
   end Image;

   -----------
   -- Image --
   -----------

   function Image (O : in SOAP_Record) return String is
      Result : Unbounded_String;
   begin
      Append (Result, '(');

      for K in O.O'Range loop
         Append (Result, Name (O));
         Append (Result, " => ");
         Append (Result, Image (O.O (K).O.all));

         if K /= O.O'Last then
            Append (Result, ", ");
         end if;
      end loop;

      Append (Result, ')');

      return To_String (Result);
   end Image;

   -----------
   -- Image --
   -----------

   function Image (O : in SOAP_Enumeration) return String is
   begin
      return To_String (O.V);
   end Image;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (O : in out Composite) is
   begin
      O.Ref_Counter := new Natural'(1);
   end Initialize;

   -------
   -- L --
   -------

   function L
     (V    : in Long;
      Name : in String := "item")
      return XSD_Long is
   begin
      return (Finalization.Controlled with To_Unbounded_String (Name), V);
   end L;

   -------
   -- N --
   -------

   function N (Name : in String  := "item") return XSD_Null is
   begin
      return (Finalization.Controlled with Name => To_Unbounded_String (Name));
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
     (V         : in Object_Set;
      Name      : in String;
      Type_Name : in String := "")
      return SOAP_Record
   is
      function T_Name return String;
      pragma Inline (T_Name);
      --  Returns Type_Name is not empty and Name otherwise

      ------------
      -- T_Name --
      ------------

      function T_Name return String is
      begin
         if Type_Name = "" then
            return Name;
         else
            return Type_Name;
         end if;
      end T_Name;

   begin
      return (Finalization.Controlled
                with To_Unbounded_String (Name),
                new Natural'(1), new Object_Set'(V),
                To_Unbounded_String (T_Name));
   end R;

   -------
   -- S --
   -------

   function S
     (V    : in String;
      Name : in String := "item")
      return XSD_String
   is
      L_V : constant String := Utils.To_Utf8 (V);
   begin
      return (Finalization.Controlled
                with To_Unbounded_String (Name), To_Unbounded_String (L_V));
   end S;

   function S
     (V      : in Unbounded_String;
      Name   : in String  := "item")
      return XSD_String is
   begin
      return (Finalization.Controlled
              with To_Unbounded_String (Name), Utils.To_Utf8 (V));
   end S;

   ----------
   -- Size --
   ----------

   function Size (O : in SOAP_Array) return Natural is
   begin
      return O.O'Length;
   end Size;

   ------------
   -- Spaces --
   ------------

   function Spaces (N : in Natural) return String is
      use Ada.Strings.Fixed;
   begin
      return (3 * N) * ' ';
   end Spaces;

   -------
   -- T --
   -------

   function T
     (V        : in Calendar.Time;
      Name     : in String        := "item";
      Timezone : in TZ            := GMT)
      return XSD_Time_Instant is
   begin
      return (Finalization.Controlled
                with To_Unbounded_String (Name), V, Timezone);
   end T;

   -------
   -- V --
   -------

   function V (O : in XSD_Integer) return Integer is
   begin
      return O.V;
   end V;

   function V (O : in XSD_Long) return Long is
   begin
      return O.V;
   end V;

   function V (O : in XSD_Float) return Long_Float is
   begin
      return O.V;
   end V;

   function V (O : in XSD_Double) return Long_Long_Float is
   begin
      return O.V;
   end V;

   function V (O : in XSD_String) return String is
   begin
      return Utils.From_Utf8 (To_String (O.V));
   end V;

   function V (O : in XSD_String) return Unbounded_String is
   begin
      return Utils.From_Utf8 (O.V);
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

   function V (O : in SOAP_Enumeration) return String is
   begin
      return To_String (O.V);
   end V;

   function V (O : in SOAP_Array) return Object_Set is
   begin
      return O.O.all;
   end V;

   function V (O : in SOAP_Array; N : in Positive) return Object'Class is
   begin
      return O.O (N).O.all;
   end V;

   function V (O : in SOAP_Record; Name : in String) return Object'Class is
   begin
      for K in O.O'Range loop
         if Types.Name (O.O (K).O.all) = Name then
            return O.O (K).O.all;
         end if;
      end loop;

      Exceptions.Raise_Exception
        (Types.Data_Error'Identity,
         "(V) Struct object " & Name & " not found");
   end V;

   function V (O : in SOAP_Record) return Object_Set is
   begin
      return O.O.all;
   end V;

   ---------------
   -- XML_Image --
   ---------------

   function XML_Image (O : in Object) return String is
      Indent : constant Natural      := XML_Indent.Value;
      OC     : constant Object'Class := Object'Class (O);
   begin
      if OC in XSD_String then
         return Spaces (Indent)
           & "<" & Name (OC) & xsi_type (XML_Type (OC)) & '>'
           & Utils.Encode (Image (OC))
           & "</" & Name (OC) & '>';
      else
         return Spaces (Indent)
           & "<" & Name (OC) & xsi_type (XML_Type (OC)) & '>'
           & Image (OC)
           & "</" & Name (OC) & '>';
      end if;
   end XML_Image;

   ---------------
   -- XML_Image --
   ---------------

   function XML_Image (O : in XSD_Integer) return String is
   begin
      return XML_Image (Object (O));
   end XML_Image;

   ---------------
   -- XML_Image --
   ---------------

   function XML_Image (O : in XSD_Long) return String is
   begin
      return XML_Image (Object (O));
   end XML_Image;

   ---------------
   -- XML_Image --
   ---------------

   function XML_Image (O : in XSD_Float) return String is
   begin
      return XML_Image (Object (O));
   end XML_Image;

   ---------------
   -- XML_Image --
   ---------------

   function XML_Image (O : in XSD_Double) return String is
   begin
      return XML_Image (Object (O));
   end XML_Image;

   ---------------
   -- XML_Image --
   ---------------

   function XML_Image (O : in XSD_String) return String is
   begin
      return XML_Image (Object (O));
   end XML_Image;

   ---------------
   -- XML_Image --
   ---------------

   function XML_Image (O : in XSD_Boolean) return String is
   begin
      return XML_Image (Object (O));
   end XML_Image;

   ---------------
   -- XML_Image --
   ---------------

   function XML_Image (O : in XSD_Time_Instant) return String is
   begin
      return XML_Image (Object (O));
   end XML_Image;

   ---------------
   -- XML_Image --
   ---------------

   function XML_Image (O : in XSD_Null) return String is
      Indent : constant Natural := XML_Indent.Value;
      OC     : constant Object'Class := Object'Class (O);
   begin
      return Spaces (Indent) & "<" & Name (OC) & " xsi_null=""1""/>";
   end XML_Image;

   ---------------
   -- XML_Image --
   ---------------

   function XML_Image (O : in SOAP_Base64) return String is
   begin
      return XML_Image (Object (O));
   end XML_Image;

   ---------------
   -- XML_Image --
   ---------------

   New_Line : constant String := ASCII.CR & ASCII.LF;

   function XML_Image (O : in SOAP_Array) return String is

      Indent : constant Natural := XML_Indent.Value;

      function Array_Type return String;
      --  Returns the right SOAP array type.

      ----------------
      -- Array_Type --
      ----------------

      function Array_Type return String is
         use type Ada.Tags.Tag;

         T : Ada.Tags.Tag;
      begin
         --  If we have a specified type name return it

         if O.Type_Name /= Null_Unbounded_String then
            return To_String (O.Type_Name);
         end if;

         --  Empty array

         if O.O'Length = 0 then
            --  This is a zero length array, type is undefined.
            return XML_Undefined;
         end if;

         T := O.O (O.O'First).O'Tag;

         --  Array with record components

         if T = SOAP_Record'Tag then
            --  This is a record, check if array is composed of only records
            --  having the same name.

            declare
               Name : constant String
                 := Types.XML_Type (O.O (O.O'First).O.all);
            begin
               --  For all remaining elements

               for K in O.O'First + 1 .. O.O'Last loop
                  if O.O (K).O'Tag /= SOAP_Record'Tag
                    or else Name /= Types.XML_Type (O.O (K).O.all)
                  then
                     return XML_Undefined;
                  end if;
               end loop;

               --  The array is composed of only records having the same
               --  name. Use this name for the array component type.
               return "awsns:" & Name;
            end;
         end if;

         --  All other cases

         for K in O.O'First + 1 .. O.O'Last loop

            --  Not same type if type different or is a composite type.

            if T /= O.O (K).O'Tag
              or else O.O (K).O.all in SOAP.Types.Composite'Class
            then
               return XML_Undefined;
            end if;

         end loop;

         --  We have the same type for all items

         return XML_Type (O.O (O.O'First).O.all);
      end Array_Type;

      Result : Unbounded_String;
   begin
      --  Open array element

      Append (Result, Spaces (Indent));
      Append (Result, '<');
      Append (Result, O.Name);
      Append (Result, " SOAP-ENC:arrayType=""");
      Append (Result, Array_Type);
      Append (Result, '[');
      Append (Result, AWS.Utils.Image (O.O'Length));
      Append (Result, "]""");
      Append (Result, xsi_type (XML_Array));
      Append (Result, '>');
      Append (Result, New_Line);

      --  Add all elements

      XML_Indent.Set_Value (Indent + 1);

      for K in O.O'Range loop
         Append (Result, XML_Image (O.O (K).O.all));
         Append (Result, New_Line);
      end loop;

      XML_Indent.Set_Value (Indent);

      --  End array element

      Append (Result, Spaces (Indent));
      Append (Result, Utils.Tag (To_String (O.Name), Start => False));

      return To_String (Result);
   end XML_Image;

   ---------------
   -- XML_Image --
   ---------------

   function XML_Image (O : in SOAP_Record) return String is
      Indent : constant Natural := XML_Indent.Value;
      Result : Unbounded_String;
   begin
      Append (Result, Spaces (Indent));

      if Name (O) = XML_Type (O) then
         --  The name and the type are identical, we do not have to specify
         --  the xsi:type in this case.
         Append (Result, Utils.Tag (Name (O), Start => True));

      else
         Append (Result, "<" & Name (O)
                   & " xsi:type=""awsns:" & XML_Type (O) & """>");
      end if;

      Append (Result, New_Line);

      XML_Indent.Set_Value (Indent + 1);

      for K in O.O'Range loop
         Append (Result, XML_Image (O.O (K).O.all));
         Append (Result, New_Line);
      end loop;

      XML_Indent.Set_Value (Indent);

      Append (Result, Spaces (Indent));
      Append (Result, Utils.Tag (Name (O), Start => False));

      return To_String (Result);
   end XML_Image;

   ---------------
   -- XML_Image --
   ---------------

   function XML_Image (O : in SOAP_Enumeration) return String is
   begin
      return Spaces (XML_Indent.Value) & "<" & Name (O)
        & " type=""" & To_String (O.Type_Name) & """>"
        & To_String (O.V)
        & Utils.Tag (Name (O), Start => False);
   end XML_Image;

   --------------
   -- XML_Type --
   --------------

   function XML_Type (O : in Object) return String is
      pragma Warnings (Off, O);
   begin
      return "";
   end XML_Type;

   function XML_Type (O : in XSD_Integer) return String is
      pragma Warnings (Off, O);
   begin
      return XML_Int;
   end XML_Type;

   function XML_Type (O : in XSD_Long) return String is
      pragma Warnings (Off, O);
   begin
      return XML_Long;
   end XML_Type;

   function XML_Type (O : in XSD_Float) return String is
      pragma Warnings (Off, O);
   begin
      return XML_Float;
   end XML_Type;

   function XML_Type (O : in XSD_Double) return String is
      pragma Warnings (Off, O);
   begin
      return XML_Double;
   end XML_Type;

   function XML_Type (O : in XSD_String) return String is
      pragma Warnings (Off, O);
   begin
      return XML_String;
   end XML_Type;

   function XML_Type (O : in XSD_Boolean) return String is
      pragma Warnings (Off, O);
   begin
      return XML_Boolean;
   end XML_Type;

   function XML_Type  (O : in XSD_Time_Instant) return String is
      pragma Warnings (Off, O);
   begin
      return XML_Time_Instant;
   end XML_Type;

   function XML_Type (O : in XSD_Null) return String is
      pragma Warnings (Off, O);
   begin
      return XML_Null;
   end XML_Type;

   function XML_Type (O : in SOAP_Base64) return String is
      pragma Warnings (Off, O);
   begin
      return XML_Base64;
   end XML_Type;

   function XML_Type (O : in SOAP_Array) return String is
      pragma Warnings (Off, O);
   begin
      return XML_Array;
   end XML_Type;

   function XML_Type  (O : in SOAP_Record) return String is
   begin
      return To_String (O.Type_Name);
   end XML_Type;

   function XML_Type (O : in SOAP_Enumeration) return String is
   begin
      return To_String (O.Type_Name);
   end XML_Type;

   --------------
   -- xsi_type --
   --------------

   function xsi_type (Name : in String) return String is
   begin
      return " xsi:type=""" & Name & '"';
   end xsi_type;

end SOAP.Types;
