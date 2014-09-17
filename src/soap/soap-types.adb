------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2000-2014, AdaCore                     --
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

pragma Ada_2012;

with Ada.Float_Text_IO;
with Ada.Long_Float_Text_IO;
with Ada.Strings.Fixed;
with Ada.Tags;
with Ada.Task_Attributes;
with Ada.Unchecked_Deallocation;

with AWS.Utils;
with GNAT.Calendar.Time_IO;

with SOAP.Types.Untyped;
with SOAP.Utils;

package body SOAP.Types is

   use Ada;

   procedure Unchecked_Free is
      new Ada.Unchecked_Deallocation (Object_Set, Object_Set_Access);

   procedure Unchecked_Free is
      new Ada.Unchecked_Deallocation (Natural, Counter_Access);

   function xsi_type (Name : String) return String with Inline;
   --  Returns the xsi:type field for the XML type representation whose name
   --  is passed as argument.

   function Spaces (N : Natural) return String with Inline;
   --  Returns N * 3 spaces

   package XML_Indent is new Ada.Task_Attributes (Natural, 0);
   --  Thread safe Indentation counter

   procedure Get_Error (Expected : String; O : Object'Class) with No_Return;
   --  Raise Data_Error, used by all Get routines

   No_Name_Space : SOAP.Name_Space.Object
     renames SOAP.Name_Space.No_Name_Space;

   ---------
   -- "+" --
   ---------

   function "+" (O : Object'Class) return Object_Safe_Pointer is
   begin
      return (Finalization.Controlled with new Object'Class'(O));
   end "+";

   -------
   -- - --
   -------

   function "-" (O : Object_Safe_Pointer) return Object'Class is
   begin
      return O.O.all;
   end "-";

   -------
   -- A --
   -------

   function A
     (V         : Object_Set;
      Name      : String;
      Type_Name : String := "") return SOAP_Array is
   begin
      return
        (Finalization.Controlled
         with To_Unbounded_String (Name), No_Name_Space,
           new Natural'(1), new Object_Set'(V),
           To_Unbounded_String (Type_Name));
   end A;

   ------------
   -- Adjust --
   ------------

   overriding procedure Adjust (O : in out Object_Safe_Pointer) is
   begin
      if O.O /= null then
         O.O := new Object'Class'(O.O.all);
      end if;
   end Adjust;

   overriding procedure Adjust (O : in out Composite) is
   begin
      O.Ref_Counter.all := O.Ref_Counter.all + 1;
   end Adjust;

   ---------
   -- Any --
   ---------

   function Any
     (V    : Object'Class;
      Name : String := "item") return XSD_Any_Type is
   begin
      return
        (Finalization.Controlled
         with To_Unbounded_String (Name), No_Name_Space, +V);
   end Any;

   -------
   -- B --
   -------

   function B
     (V    : Boolean;
      Name : String  := "item") return XSD_Boolean is
   begin
      return
        (Finalization.Controlled
         with To_Unbounded_String (Name), No_Name_Space, V);
   end B;

   function B
     (V    : Byte;
      Name : String  := "item") return XSD_Byte is
   begin
      return
        (Finalization.Controlled
         with To_Unbounded_String (Name), No_Name_Space, V);
   end B;

   ---------
   -- B64 --
   ---------

   function B64
     (V      : String;
      Name   : String  := "item") return SOAP_Base64 is
   begin
      return
        (Finalization.Controlled
         with To_Unbounded_String (Name),
              No_Name_Space,
              To_Unbounded_String (V));
   end B64;

   -------
   -- D --
   -------

   function D
     (V    : Long_Float;
      Name : String          := "item") return XSD_Double is
   begin
      return
        (Finalization.Controlled
         with To_Unbounded_String (Name), No_Name_Space, V);
   end D;

   -------
   -- E --
   -------

   function E
     (V         : String;
      Type_Name : String;
      Name      : String := "item") return SOAP_Enumeration is
   begin
      return (Finalization.Controlled
              with To_Unbounded_String (Name), No_Name_Space,
                   To_Unbounded_String (V),
                   To_Unbounded_String (Type_Name));
   end E;

   -------
   -- F --
   -------

   function F
     (V    : Float;
      Name : String := "item") return XSD_Float is
   begin
      return
        (Finalization.Controlled
         with To_Unbounded_String (Name), No_Name_Space, V);
   end F;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (O : in out Object_Safe_Pointer) is
      procedure Unchecked_Free is
        new Ada.Unchecked_Deallocation (Object'Class, Object_Access);
   begin
      if O.O /= null then
         Unchecked_Free (O.O);
      end if;
   end Finalize;

   overriding procedure Finalize (O : in out Composite) is
      Ref_Counter : Counter_Access := O.Ref_Counter;
   begin
      --  Ensure call is idempotent

      O.Ref_Counter := null;

      if Ref_Counter /= null then
         Ref_Counter.all := Ref_Counter.all - 1;

         if Ref_Counter.all = 0 then
            Unchecked_Free (O.O);
            Unchecked_Free (Ref_Counter);
         end if;
      end if;
   end Finalize;

   ---------
   -- Get --
   ---------

   function Get (O : Object'Class) return XSD_Any_Type is
      use type Ada.Tags.Tag;
   begin
      return Any (O, Name (O));
   end Get;

   function Get (O : Object'Class) return Long is
      use type Ada.Tags.Tag;
   begin
      if O'Tag = Types.XSD_Long'Tag then
         return V (XSD_Long (O));

      elsif O'Tag = Types.Untyped.Untyped'Tag then
         begin
            return Long'Value (V (XSD_String (O)));
         exception
            when others =>
               Get_Error ("Long", O);
         end;

      elsif O'Tag = Types.XSD_Any_Type'Tag
        and then XSD_Any_Type (O).O.O'Tag = Types.XSD_Long'Tag
      then
         return V (XSD_Long (XSD_Any_Type (O).O.O.all));

      else
         Get_Error ("Long", O);
      end if;
   end Get;

   function Get (O : Object'Class) return Integer is
      use type Ada.Tags.Tag;
   begin
      if O'Tag = Types.XSD_Integer'Tag then
         return V (XSD_Integer (O));

      elsif O'Tag = Types.Untyped.Untyped'Tag then
         begin
            return Integer'Value (V (XSD_String (O)));
         exception
            when others =>
               Get_Error ("Integer", O);
         end;

      elsif O'Tag = Types.XSD_Any_Type'Tag
        and then XSD_Any_Type (O).O.O'Tag = Types.XSD_Integer'Tag
      then
         return V (XSD_Integer (XSD_Any_Type (O).O.O.all));

      else
         Get_Error ("Integer", O);
      end if;
   end Get;

   function Get (O : Object'Class) return Short is
      use type Ada.Tags.Tag;
   begin
      if O'Tag = Types.XSD_Short'Tag then
         return V (XSD_Short (O));

      elsif O'Tag = Types.Untyped.Untyped'Tag then
         begin
            return Short'Value (V (XSD_String (O)));
         exception
            when others =>
               Get_Error ("Short", O);
         end;

      elsif O'Tag = Types.XSD_Any_Type'Tag
        and then XSD_Any_Type (O).O.O'Tag = Types.XSD_Short'Tag
      then
         return V (XSD_Short (XSD_Any_Type (O).O.O.all));

      else
         Get_Error ("Short", O);
      end if;
   end Get;

   function Get (O : Object'Class) return Byte is
      use type Ada.Tags.Tag;
   begin
      if O'Tag = Types.XSD_Byte'Tag then
         return V (XSD_Byte (O));

      elsif O'Tag = Types.Untyped.Untyped'Tag then
         begin
            return Byte'Value (V (XSD_String (O)));
         exception
            when others =>
               Get_Error ("Byte", O);
         end;

      elsif O'Tag = Types.XSD_Any_Type'Tag
        and then XSD_Any_Type (O).O.O'Tag = Types.XSD_Byte'Tag
      then
         return V (XSD_Byte (XSD_Any_Type (O).O.O.all));

      else
         Get_Error ("Byte", O);
      end if;
   end Get;

   function Get (O : Object'Class) return Float is
      use type Ada.Tags.Tag;
   begin
      if O'Tag = Types.XSD_Float'Tag then
         return V (XSD_Float (O));

      elsif O'Tag = Types.Untyped.Untyped'Tag then
         begin
            return Float'Value (V (XSD_String (O)));
         exception
            when others =>
               Get_Error ("Float", O);
         end;

      elsif O'Tag = Types.XSD_Any_Type'Tag
        and then XSD_Any_Type (O).O.O'Tag = Types.XSD_Float'Tag
      then
         return V (XSD_Float (XSD_Any_Type (O).O.O.all));

      else
         Get_Error ("Float", O);
      end if;
   end Get;

   function Get (O : Object'Class) return Long_Float is
      use type Ada.Tags.Tag;
   begin
      if O'Tag = Types.XSD_Double'Tag then
         return V (XSD_Double (O));

      elsif O'Tag = Types.Untyped.Untyped'Tag then
         begin
            return Long_Float'Value (V (XSD_String (O)));
         exception
            when others =>
               Get_Error ("Double", O);
         end;

      elsif O'Tag = Types.XSD_Any_Type'Tag
        and then XSD_Any_Type (O).O.O'Tag = Types.XSD_Double'Tag
      then
         return V (XSD_Double (XSD_Any_Type (O).O.O.all));

      else
         Get_Error ("Double", O);
      end if;
   end Get;

   function Get (O : Object'Class) return String is
      use type Ada.Tags.Tag;
   begin
      if O'Tag = Types.XSD_String'Tag
        or else O'Tag = Types.Untyped.Untyped'Tag
      then
         return V (XSD_String (O));

      elsif O'Tag = Types.XSD_Any_Type'Tag
        and then XSD_Any_Type (O).O.O'Tag = Types.XSD_String'Tag
      then
         return V (XSD_String (XSD_Any_Type (O).O.O.all));

      else
         Get_Error ("String", O);
      end if;
   end Get;

   function Get (O : Object'Class) return Unbounded_String is
      use type Ada.Tags.Tag;
   begin
      if O'Tag = Types.XSD_String'Tag
        or else O'Tag = Types.Untyped.Untyped'Tag
      then
         return V (XSD_String (O));

      elsif O'Tag = Types.XSD_Any_Type'Tag
        and then XSD_Any_Type (O).O.O'Tag = Types.XSD_String'Tag
      then
         return V (XSD_String (XSD_Any_Type (O).O.O.all));

      else
         Get_Error ("String", O);
      end if;
   end Get;

   function Get (O : Object'Class) return Boolean is
      use type Ada.Tags.Tag;
   begin
      if O'Tag = Types.XSD_Boolean'Tag then
         return V (XSD_Boolean (O));

      elsif O'Tag = Types.Untyped.Untyped'Tag then
         begin
            return Boolean'Value (V (XSD_String (O)));
         exception
            when others =>
               Get_Error ("Boolean", O);
         end;

      elsif O'Tag = Types.XSD_Any_Type'Tag
        and then XSD_Any_Type (O).O.O'Tag = Types.XSD_Boolean'Tag
      then
         return V (XSD_Boolean (XSD_Any_Type (O).O.O.all));

      else
         Get_Error ("Boolean", O);
      end if;
   end Get;

   function Get (O : Object'Class) return Ada.Calendar.Time is
      use type Ada.Tags.Tag;
   begin
      if O'Tag = Types.XSD_Time_Instant'Tag then
         return V (XSD_Time_Instant (O));

      elsif O'Tag = Types.Untyped.Untyped'Tag then
         begin
            return V (Utils.Time_Instant (V (XSD_String (O)), Name (O)));
         exception
            when others =>
               Get_Error ("timeInstant", O);
         end;

      elsif O'Tag = Types.XSD_Any_Type'Tag
        and then XSD_Any_Type (O).O.O'Tag = Types.XSD_Time_Instant'Tag
      then
         return V (XSD_Time_Instant (XSD_Any_Type (O).O.O.all));

      else
         Get_Error ("timeInstant", O);
      end if;
   end Get;

   function Get (O : Object'Class) return Unsigned_Long is
      use type Ada.Tags.Tag;
   begin
      if O'Tag = Types.XSD_Unsigned_Long'Tag then
         return V (XSD_Unsigned_Long (O));

      elsif O'Tag = Types.Untyped.Untyped'Tag then
         begin
            return Unsigned_Long'Value (V (XSD_String (O)));
         exception
            when others =>
               Get_Error ("Unsigned_Long", O);
         end;

      elsif O'Tag = Types.XSD_Any_Type'Tag
        and then XSD_Any_Type (O).O.O'Tag = Types.XSD_Unsigned_Long'Tag
      then
         return V (XSD_Unsigned_Long (XSD_Any_Type (O).O.O.all));

      else
         Get_Error ("Unsigned_Long", O);
      end if;
   end Get;

   function Get (O : Object'Class) return Unsigned_Int is
      use type Ada.Tags.Tag;
   begin
      if O'Tag = Types.XSD_Unsigned_Int'Tag then
         return V (XSD_Unsigned_Int (O));

      elsif O'Tag = Types.Untyped.Untyped'Tag then
         begin
            return Unsigned_Int'Value (V (XSD_String (O)));
         exception
            when others =>
               Get_Error ("Unsigned_Int", O);
         end;

      elsif O'Tag = Types.XSD_Any_Type'Tag
        and then XSD_Any_Type (O).O.O'Tag = Types.XSD_Unsigned_Int'Tag
      then
         return V (XSD_Unsigned_Int (XSD_Any_Type (O).O.O.all));

      else
         Get_Error ("Unsigned_Int", O);
      end if;
   end Get;

   function Get (O : Object'Class) return Unsigned_Short is
      use type Ada.Tags.Tag;
   begin
      if O'Tag = Types.XSD_Unsigned_Short'Tag then
         return V (XSD_Unsigned_Short (O));

      elsif O'Tag = Types.Untyped.Untyped'Tag then
         begin
            return Unsigned_Short'Value (V (XSD_String (O)));
         exception
            when others =>
               Get_Error ("Unsigned_Short", O);
         end;

      elsif O'Tag = Types.XSD_Any_Type'Tag
        and then XSD_Any_Type (O).O.O'Tag = Types.XSD_Unsigned_Short'Tag
      then
         return V (XSD_Unsigned_Short (XSD_Any_Type (O).O.O.all));

      else
         Get_Error ("Unsigned_Short", O);
      end if;
   end Get;

   function Get (O : Object'Class) return Unsigned_Byte is
      use type Ada.Tags.Tag;
   begin
      if O'Tag = Types.XSD_Unsigned_Byte'Tag then
         return V (XSD_Unsigned_Byte (O));

      elsif O'Tag = Types.Untyped.Untyped'Tag then
         begin
            return Unsigned_Byte'Value (V (XSD_String (O)));
         exception
            when others =>
               Get_Error ("Unsigned_Byte", O);
         end;

      elsif O'Tag = Types.XSD_Any_Type'Tag
        and then XSD_Any_Type (O).O.O'Tag = Types.XSD_Unsigned_Byte'Tag
      then
         return V (XSD_Unsigned_Byte (XSD_Any_Type (O).O.O.all));

      else
         Get_Error ("Unsigned_Byte", O);
      end if;
   end Get;

   function Get (O : Object'Class) return SOAP_Base64 is
      use type Ada.Tags.Tag;
   begin
      if O'Tag = Types.SOAP_Base64'Tag then
         return SOAP_Base64 (O);

      elsif O'Tag = Types.Untyped.Untyped'Tag then
         return B64 (V (XSD_String (O)), Name (O));

      elsif O'Tag = Types.XSD_Any_Type'Tag
        and then XSD_Any_Type (O).O.O'Tag = Types.SOAP_Base64'Tag
      then
         return SOAP_Base64 (XSD_Any_Type (O).O.O.all);

      else
         Get_Error ("SOAP Base64", O);
      end if;
   end Get;

   function Get (O : Object'Class) return SOAP_Record is
      use type Ada.Tags.Tag;
   begin
      if O'Tag = Types.SOAP_Record'Tag then
         return SOAP_Record (O);

      elsif O'Tag = Types.XSD_Any_Type'Tag
        and then XSD_Any_Type (O).O.O'Tag = Types.SOAP_Record'Tag
      then
         return SOAP_Record (XSD_Any_Type (O).O.O.all);

      else
         Get_Error ("SOAP Struct", O);
      end if;
   end Get;

   function Get (O : Object'Class) return SOAP_Array is
      use type Ada.Tags.Tag;
   begin
      if O'Tag = Types.SOAP_Array'Tag then
         return SOAP_Array (O);

      elsif O'Tag = Types.XSD_Any_Type'Tag
        and then XSD_Any_Type (O).O.O'Tag = Types.SOAP_Array'Tag
      then
         return SOAP_Array (XSD_Any_Type (O).O.O.all);

      else
         Get_Error ("SOAP Array", O);
      end if;
   end Get;

   ---------------
   -- Get_Error --
   ---------------

   procedure Get_Error (Expected : String; O : Object'Class) is
      use type Ada.Tags.Tag;
   begin
      if O'Tag = Types.XSD_Any_Type'Tag then
         raise Data_Error
           with Expected & " expected, found "
             & Tags.Expanded_Name (XSD_Any_Type (O).O.O'Tag)
             & " in an XSD_Any_Type object.";
      else
         raise Data_Error
           with Expected & " expected, found " & Tags.Expanded_Name (O'Tag);
      end if;
   end Get_Error;

   -------
   -- I --
   -------

   function I
     (V    : Integer;
      Name : String := "item") return XSD_Integer is
   begin
      return
        (Finalization.Controlled
         with To_Unbounded_String (Name), No_Name_Space, V);
   end I;

   -----------
   -- Image --
   -----------

   function Image (O : Object) return String is
      pragma Unreferenced (O);
   begin
      return "";
   end Image;

   overriding function Image (O : XSD_Any_Type) return String is
   begin
      return Image (O.O.O.all);
   end Image;

   overriding function Image (O : XSD_Long) return String is
      V : constant String := Long'Image (O.V);
   begin
      if O.V >= 0 then
         return V (V'First + 1 .. V'Last);
      else
         return V;
      end if;
   end Image;

   overriding function Image (O : XSD_Integer) return String is
      V : constant String := Integer'Image (O.V);
   begin
      if O.V >= 0 then
         return V (V'First + 1 .. V'Last);
      else
         return V;
      end if;
   end Image;

   overriding function Image (O : XSD_Short) return String is
      V : constant String := Short'Image (O.V);
   begin
      if O.V >= 0 then
         return V (V'First + 1 .. V'Last);
      else
         return V;
      end if;
   end Image;

   overriding function Image (O : XSD_Byte) return String is
      V : constant String := Byte'Image (O.V);
   begin
      if O.V >= 0 then
         return V (V'First + 1 .. V'Last);
      else
         return V;
      end if;
   end Image;

   overriding function Image (O : XSD_Float) return String is
      Result : String (1 .. Long_Float'Width);
   begin
      Float_Text_IO.Put (Result, O.V, Exp => 0);
      return Strings.Fixed.Trim (Result, Strings.Both);
   end Image;

   overriding function Image (O : XSD_Double) return String is
      Result : String (1 .. Long_Long_Float'Width);
   begin
      Long_Float_Text_IO.Put (Result, O.V, Exp => 0);
      return Strings.Fixed.Trim (Result, Strings.Both);
   end Image;

   overriding function Image (O : XSD_String) return String is
   begin
      return To_String (O.V);
   end Image;

   overriding function Image (O : XSD_Boolean) return String is
   begin
      if O.V then
         return "1";
      else
         return "0";
      end if;
   end Image;

   overriding function Image (O : XSD_Time_Instant) return String is

      function Image (Timezone : TZ) return String;
      --  Returns Image for the TZ

      -----------
      -- Image --
      -----------

      function Image (Timezone : TZ) return String is

         subtype Str2 is String (1 .. 2);

         function I2D (N : Natural) return Str2;
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

   overriding function Image (O : XSD_Unsigned_Long) return String is
      V : constant String := Unsigned_Long'Image (O.V);
   begin
      return V (V'First + 1 .. V'Last);
   end Image;

   overriding function Image (O : XSD_Unsigned_Int) return String is
      V : constant String := Unsigned_Int'Image (O.V);
   begin
      return V (V'First + 1 .. V'Last);
   end Image;

   overriding function Image (O : XSD_Unsigned_Short) return String is
      V : constant String := Unsigned_Short'Image (O.V);
   begin
      return V (V'First + 1 .. V'Last);
   end Image;

   overriding function Image (O : XSD_Unsigned_Byte) return String is
      V : constant String := Unsigned_Byte'Image (O.V);
   begin
      return V (V'First + 1 .. V'Last);
   end Image;

   overriding function Image (O : SOAP_Base64) return String is
   begin
      return To_String (O.V);
   end Image;

   overriding function Image (O : SOAP_Array) return String is
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

   overriding function Image (O : SOAP_Record) return String is
      Result : Unbounded_String;
   begin
      Append (Result, '(');

      for K in O.O'Range loop
         Append (Result, Name (O.O (K).O.all));
         Append (Result, " => ");
         Append (Result, Image (O.O (K).O.all));

         if K /= O.O'Last then
            Append (Result, ", ");
         end if;
      end loop;

      Append (Result, ')');

      return To_String (Result);
   end Image;

   overriding function Image (O : SOAP_Enumeration) return String is
   begin
      return To_String (O.V);
   end Image;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (O : in out Composite) is
   begin
      O.Ref_Counter := new Natural'(1);
   end Initialize;

   function Is_Empty (O : Object) return Boolean is
      pragma Unreferenced (O);
   begin
      return False;
   end Is_Empty;

   overriding function Is_Empty (O : XSD_Null) return Boolean is
      pragma Unreferenced (O);
   begin
      return True;
   end Is_Empty;

   overriding function Is_Empty (O : Composite) return Boolean is
   begin
      return O.O'Length = 0;
   end Is_Empty;

   -------
   -- L --
   -------

   function L
     (V    : Long;
      Name : String := "item") return XSD_Long is
   begin
      return
        (Finalization.Controlled
         with To_Unbounded_String (Name), No_Name_Space, V);
   end L;

   -------
   -- N --
   -------

   function N (Name : String  := "item") return XSD_Null is
   begin
      return
        (Finalization.Controlled
         with To_Unbounded_String (Name), No_Name_Space);
   end N;

   ----------
   -- Name --
   ----------

   function Name (O : Object'Class) return String is
   begin
      return To_String (O.Name);
   end Name;

   ----------------
   -- Name_Space --
   ----------------

   function Name_Space (O : Object'Class) return SOAP.Name_Space.Object is
   begin
      return O.NS;
   end Name_Space;

   -------
   -- R --
   -------

   function R
     (V         : Object_Set;
      Name      : String;
      Type_Name : String := "") return SOAP_Record is
   begin
      return
        (Finalization.Controlled
         with To_Unbounded_String (Name), SOAP.Name_Space.AWS,
         new Natural'(1), new Object_Set'(V),
         To_Unbounded_String (if Type_Name = "" then Name else Type_Name));
   end R;

   ------------
   -- Rename --
   ------------

   procedure Rename (O : in out Object'Class; Name : String) is
   begin
      O.Name := To_Unbounded_String (Name);
   end Rename;

   function Rename (O : Object'Class; Name : String) return Object'Class is
      Copy : Object'Class := O;
   begin
      Rename (Copy, Name);

      return Copy;
   end Rename;

   -------
   -- S --
   -------

   function S (V : Short; Name : String := "item") return XSD_Short is
   begin
      return
        (Finalization.Controlled
         with To_Unbounded_String (Name), No_Name_Space, V);
   end S;

   function S
     (V    : String;
      Name : String := "item") return XSD_String
   is
      L_V : constant String := Utils.To_Utf8 (V);
   begin
      return
        (Finalization.Controlled
         with To_Unbounded_String (Name), No_Name_Space,
         To_Unbounded_String (L_V));
   end S;

   function S
     (V      : Unbounded_String;
      Name   : String  := "item") return XSD_String is
   begin
      return
        (Finalization.Controlled
         with To_Unbounded_String (Name), No_Name_Space, Utils.To_Utf8 (V));
   end S;

   --------------------
   -- Set_Name_Space --
   --------------------

   procedure Set_Name_Space
     (O : in out Object'Class; NS : SOAP.Name_Space.Object) is
   begin
      O.NS := NS;
   end Set_Name_Space;

   ----------
   -- Size --
   ----------

   function Size (O : SOAP_Array) return Natural is
   begin
      return O.O'Length;
   end Size;

   ------------
   -- Spaces --
   ------------

   function Spaces (N : Natural) return String is
      use Ada.Strings.Fixed;
   begin
      return (3 * N) * ' ';
   end Spaces;

   -------
   -- T --
   -------

   function T
     (V        : Calendar.Time;
      Name     : String        := "item";
      Timezone : TZ            := GMT) return XSD_Time_Instant is
   begin
      return
        (Finalization.Controlled
         with To_Unbounded_String (Name), No_Name_Space, V, Timezone);
   end T;

   --------
   -- UB --
   --------

   function UB
     (V    : Unsigned_Byte;
      Name : String := "item") return XSD_Unsigned_Byte is
   begin
      return
        (Finalization.Controlled
         with To_Unbounded_String (Name), No_Name_Space, V);
   end UB;

   --------
   -- UI --
   --------

   function UI
     (V    : Unsigned_Int;
      Name : String := "item") return XSD_Unsigned_Int is
   begin
      return
        (Finalization.Controlled
         with To_Unbounded_String (Name), No_Name_Space, V);
   end UI;

   --------
   -- UL --
   --------

   function UL
     (V    : Unsigned_Long;
      Name : String := "item") return XSD_Unsigned_Long is
   begin
      return
        (Finalization.Controlled
         with To_Unbounded_String (Name), No_Name_Space, V);
   end UL;

   --------
   -- US --
   --------

   function US
     (V    : Unsigned_Short;
      Name : String := "item") return XSD_Unsigned_Short is
   begin
      return
        (Finalization.Controlled
         with To_Unbounded_String (Name), No_Name_Space, V);
   end US;

   -------
   -- V --
   -------

   function V (O : XSD_Any_Type) return Object_Access is
   begin
      return O.O.O;
   end V;

   function V (O : XSD_Long) return Long is
   begin
      return O.V;
   end V;

   function V (O : XSD_Integer) return Integer is
   begin
      return O.V;
   end V;

   function V (O : XSD_Short) return Short is
   begin
      return O.V;
   end V;

   function V (O : XSD_Byte) return Byte is
   begin
      return O.V;
   end V;

   function V (O : XSD_Float) return Float is
   begin
      return O.V;
   end V;

   function V (O : XSD_Double) return Long_Float is
   begin
      return O.V;
   end V;

   function V (O : XSD_String) return String is
   begin
      return Utils.From_Utf8 (To_String (O.V));
   end V;

   function V (O : XSD_String) return Unbounded_String is
   begin
      return Utils.From_Utf8 (O.V);
   end V;

   function V (O : XSD_Boolean) return Boolean is
   begin
      return O.V;
   end V;

   function V (O : XSD_Time_Instant) return Calendar.Time is
   begin
      return O.T;
   end V;

   function V (O : XSD_Unsigned_Long) return Unsigned_Long is
   begin
      return O.V;
   end V;

   function V (O : XSD_Unsigned_Int) return Unsigned_Int is
   begin
      return O.V;
   end V;

   function V (O : XSD_Unsigned_Short) return Unsigned_Short is
   begin
      return O.V;
   end V;

   function V (O : XSD_Unsigned_Byte) return Unsigned_Byte is
   begin
      return O.V;
   end V;

   function V (O : SOAP_Base64) return String is
   begin
      return To_String (O.V);
   end V;

   function V (O : SOAP_Enumeration) return String is
   begin
      return To_String (O.V);
   end V;

   function V (O : Composite) return Object_Set is
   begin
      return O.O.all;
   end V;

   function V (O : SOAP_Array; N : Positive) return Object'Class is
   begin
      return O.O (N).O.all;
   end V;

   function V (O : SOAP_Record; Name : String) return Object'Class is
   begin
      for K in O.O'Range loop
         if Types.Name (O.O (K).O.all) = Name then
            return O.O (K).O.all;
         end if;
      end loop;

      raise Types.Data_Error
        with "(V) Struct object " & Name & " not found";
   end V;

   ---------------
   -- XML_Image --
   ---------------

   procedure XML_Image
     (O      : Object;
      Result : in out Unbounded_String)
   is
      Indent : constant Natural      := XML_Indent.Value;
      OC     : constant Object'Class := Object'Class (O);
   begin
      if OC in XSD_String then
         Append (Result, Spaces (Indent));
         Append (Result, "<");
         Append (Result, Name (OC));
         Append (Result, xsi_type (XML_Type (OC)));
         Append (Result, '>');
         Utils.Encode (XSD_String (OC).V, Result);
         Append (Result, "</");
         Append (Result, Name (OC));
         Append (Result, '>');
      else
         Append (Result, Spaces (Indent));
         Append (Result, "<");
         Append (Result, Name (OC));
         Append (Result, xsi_type (XML_Type (OC)));
         Append (Result, '>');
         Append (Result, Image (OC));
         Append (Result, "</");
         Append (Result, Name (OC));
         Append (Result, '>');
      end if;
   end XML_Image;

   overriding procedure XML_Image
     (O : XSD_Any_Type; Result : in out Unbounded_String) is
   begin
      XML_Image (Object (O.O.O.all), Result);
   end XML_Image;

   overriding procedure XML_Image
     (O : XSD_Long; Result : in out Unbounded_String) is
   begin
      XML_Image (Object (O), Result);
   end XML_Image;

   overriding procedure XML_Image
     (O : XSD_Integer; Result : in out Unbounded_String) is
   begin
      XML_Image (Object (O), Result);
   end XML_Image;

   overriding procedure XML_Image
     (O : XSD_Short; Result : in out Unbounded_String) is
   begin
      XML_Image (Object (O), Result);
   end XML_Image;

   overriding procedure XML_Image
     (O : XSD_Byte; Result : in out Unbounded_String) is
   begin
      XML_Image (Object (O), Result);
   end XML_Image;

   overriding procedure XML_Image
     (O : XSD_Float; Result : in out Unbounded_String) is
   begin
      XML_Image (Object (O), Result);
   end XML_Image;

   overriding procedure XML_Image
     (O : XSD_Double; Result : in out Unbounded_String) is
   begin
      XML_Image (Object (O), Result);
   end XML_Image;

   overriding procedure XML_Image
     (O : XSD_String; Result : in out Unbounded_String) is
   begin
      XML_Image (Object (O), Result);
   end XML_Image;

   overriding procedure XML_Image
     (O : XSD_Boolean; Result : in out Unbounded_String) is
   begin
      XML_Image (Object (O), Result);
   end XML_Image;

   overriding procedure XML_Image
     (O : XSD_Time_Instant; Result : in out Unbounded_String) is
   begin
      XML_Image (Object (O), Result);
   end XML_Image;

   overriding procedure XML_Image
     (O : XSD_Unsigned_Long; Result : in out Unbounded_String) is
   begin
      XML_Image (Object (O), Result);
   end XML_Image;

   overriding procedure XML_Image
     (O : XSD_Unsigned_Int; Result : in out Unbounded_String) is
   begin
      XML_Image (Object (O), Result);
   end XML_Image;

   overriding procedure XML_Image
     (O : XSD_Unsigned_Short; Result : in out Unbounded_String) is
   begin
      XML_Image (Object (O), Result);
   end XML_Image;

   overriding procedure XML_Image
     (O : XSD_Unsigned_Byte; Result : in out Unbounded_String) is
   begin
      XML_Image (Object (O), Result);
   end XML_Image;

   overriding procedure XML_Image
     (O : XSD_Null; Result : in out Unbounded_String)
   is
      Indent : constant Natural := XML_Indent.Value;
      OC     : constant Object'Class := Object'Class (O);
   begin
      Append (Result, Spaces (Indent));
      Append (Result, "<");
      Append (Result, Name (OC));
      Append (Result, " xsi_null=""1""/>");
   end XML_Image;

   overriding procedure XML_Image
    (O : SOAP_Base64; Result : in out Unbounded_String) is
   begin
      XML_Image (Object (O), Result);
   end XML_Image;

   New_Line : constant String := ASCII.CR & ASCII.LF;

   overriding procedure XML_Image
     (O      : SOAP_Array;
      Result : in out Unbounded_String)
   is

      Indent : constant Natural := XML_Indent.Value;

      function Array_Type return String;
      --  Returns the right SOAP array type

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
            --  This is a zero length array, type is undefined
            return XML_Undefined;
         end if;

         T := O.O (O.O'First).O'Tag;

         --  Array with record components

         if T = SOAP_Record'Tag then
            --  This is a record, check if array is composed of only records
            --  having the same name.

            declare
               Name : constant String :=
                        Types.XML_Type (O.O (O.O'First).O.all);
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

               return SOAP.Name_Space.Name
                 (Name_Space (O.O (O.O'First).O.all)) & ":" & Name;
            end;
         end if;

         --  An heterogeneous array

         if T = XSD_Any_Type'Tag then
            return XML_Any_Type;
         end if;

         --  All other cases

         for K in O.O'First + 1 .. O.O'Last loop

            --  Not same type if type different or is a composite type

            if T /= O.O (K).O'Tag
              or else O.O (K).O.all in SOAP.Types.Composite'Class
            then
               return XML_Any_Type;
            end if;

         end loop;

         --  We have the same type for all items

         return XML_Type (O.O (O.O'First).O.all);
      end Array_Type;

   begin
      --  Open array element

      Append (Result, Spaces (Indent));
      Append (Result, '<');
      Append (Result, O.Name);
      Append (Result, " soapenc:arrayType=""");
      Append (Result, Array_Type);
      Append (Result, '[');
      Append (Result, AWS.Utils.Image (Natural (O.O'Length)));
      Append (Result, "]""");
      Append (Result, xsi_type (XML_Array));
      Append (Result, '>');
      Append (Result, New_Line);

      --  Add all elements

      XML_Indent.Set_Value (Indent + 1);

      for K in O.O'Range loop
         XML_Image (O.O (K).O.all, Result);
         Append (Result, New_Line);
      end loop;

      XML_Indent.Set_Value (Indent);

      --  End array element

      Append (Result, Spaces (Indent));
      Append (Result, Utils.Tag (To_String (O.Name), Start => False));
   end XML_Image;

   overriding procedure XML_Image
     (O      : SOAP_Record;
      Result : in out Unbounded_String)
   is
      Indent : constant Natural := XML_Indent.Value;
   begin
      Append (Result, Spaces (Indent));

      if Name (O) = XML_Type (O) then
         --  The name and the type are identical, we do not have to specify
         --  the xsi:type in this case.
         Append (Result, Utils.Tag (Name (O), Start => True));

      else
         Append (Result, "<" & Name (O)
                 & " xsi:type="""
                 & SOAP.Name_Space.Name (O.NS) & ":" & XML_Type (O) & """>");
      end if;

      Append (Result, New_Line);

      XML_Indent.Set_Value (Indent + 1);

      for K in O.O'Range loop
         XML_Image (O.O (K).O.all, Result);
         Append (Result, New_Line);
      end loop;

      XML_Indent.Set_Value (Indent);

      Append (Result, Spaces (Indent));
      Append (Result, Utils.Tag (Name (O), Start => False));
   end XML_Image;

   overriding procedure XML_Image
     (O : SOAP_Enumeration; Result : in out Unbounded_String) is
   begin
      Append (Result, Spaces (XML_Indent.Value));
      Append (Result, "<");
      Append (Result, Name (O));
      Append (Result, " type=""");
      Append (Result, O.Type_Name);
      Append (Result, """>");
      Append (Result, O.V);
      Append (Result, Utils.Tag (Name (O), Start => False));
   end XML_Image;

   function XML_Image (O : Object'Class) return String is
      Result : Unbounded_String;
   begin
      XML_Image (O, Result);
      return To_String (Result);
   end XML_Image;

   --------------
   -- XML_Type --
   --------------

   function XML_Type (O : Object) return String is
      pragma Unreferenced (O);
   begin
      return "";
   end XML_Type;

   overriding function XML_Type (O : XSD_Any_Type) return String is
   begin
      return XML_Type (O.O.O.all);
   end XML_Type;

   overriding function XML_Type (O : XSD_Long) return String is
      pragma Unreferenced (O);
   begin
      return XML_Long;
   end XML_Type;

   overriding function XML_Type (O : XSD_Integer) return String is
      pragma Unreferenced (O);
   begin
      return XML_Int;
   end XML_Type;

   overriding function XML_Type (O : XSD_Short) return String is
      pragma Unreferenced (O);
   begin
      return XML_Short;
   end XML_Type;

   overriding function XML_Type (O : XSD_Byte) return String is
      pragma Unreferenced (O);
   begin
      return XML_Byte;
   end XML_Type;

   overriding function XML_Type (O : XSD_Float) return String is
      pragma Unreferenced (O);
   begin
      return XML_Float;
   end XML_Type;

   overriding function XML_Type (O : XSD_Double) return String is
      pragma Unreferenced (O);
   begin
      return XML_Double;
   end XML_Type;

   overriding function XML_Type (O : XSD_String) return String is
      pragma Unreferenced (O);
   begin
      return XML_String;
   end XML_Type;

   overriding function XML_Type (O : XSD_Boolean) return String is
      pragma Unreferenced (O);
   begin
      return XML_Boolean;
   end XML_Type;

   overriding function XML_Type (O : XSD_Time_Instant) return String is
      pragma Unreferenced (O);
   begin
      return XML_Time_Instant;
   end XML_Type;

   overriding function XML_Type (O : XSD_Unsigned_Long) return String is
      pragma Unreferenced (O);
   begin
      return XML_Unsigned_Long;
   end XML_Type;

   overriding function XML_Type (O : XSD_Unsigned_Int) return String is
      pragma Unreferenced (O);
   begin
      return XML_Unsigned_Int;
   end XML_Type;

   overriding function XML_Type (O : XSD_Unsigned_Short) return String is
      pragma Unreferenced (O);
   begin
      return XML_Unsigned_Short;
   end XML_Type;

   overriding function XML_Type (O : XSD_Unsigned_Byte) return String is
      pragma Unreferenced (O);
   begin
      return XML_Unsigned_Byte;
   end XML_Type;

   overriding function XML_Type (O : XSD_Null) return String is
      pragma Unreferenced (O);
   begin
      return XML_Null;
   end XML_Type;

   overriding function XML_Type (O : SOAP_Base64) return String is
      pragma Unreferenced (O);
   begin
      return XML_Base64;
   end XML_Type;

   overriding function XML_Type (O : SOAP_Array) return String is
      pragma Unreferenced (O);
   begin
      return XML_Array;
   end XML_Type;

   overriding function XML_Type (O : SOAP_Record) return String is
   begin
      return To_String (O.Type_Name);
   end XML_Type;

   overriding function XML_Type (O : SOAP_Enumeration) return String is
   begin
      return To_String (O.Type_Name);
   end XML_Type;

   --------------
   -- xsi_type --
   --------------

   function xsi_type (Name : String) return String is
   begin
      return " xsi:type=""" & Name & '"';
   end xsi_type;

end SOAP.Types;
