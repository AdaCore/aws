------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2001-2004                          --
--                                ACT-Europe                                --
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

--  This package contains all SOAP types supported by this implementation.
--  Here are some notes about adding support for a new SOAP type (not a
--  container) and the corresponding WSDL support:
--
--  1. Add new type derived from scalar in this package. Implements all
--     inherited routines (Image, XML_Image and XML_Type). Implements also
--     a constructor for this new type and a routine named V to get the
--     value as an Ada type.
--
--  2. In SOAP.Parameters add corrsponding Get routine.
--
--  3. In SOAP.WSDL, add the new type name in Parameter_Type.
--
--  4. Add support for this new type in all SOAP.WSDL routines. All routines
--     are using a case statement to be sure that it won't compile without
--     fixing it first. For obvious reasons, only SOAP.WSDL.To_Type and
--     SOAP.WSDL.From_Ada are not using a case statement, be sure to do the
--     right Change There.
--
--  5. Finaly add support for this type in SOAP.Message.XML. Add this type
--     into Type_State, write the corresponding parse procedure and fill entry
--     into Handlers. Again after adding the proper type into Type_State the
--     compiler will issue errors where changes are needed.

with Ada.Calendar;
with Ada.Finalization;
with Ada.Strings.Unbounded;

with SOAP.Name_Space;

package SOAP.Types is

   use Ada.Strings.Unbounded;

   Data_Error : exception;
   --  Raised when a variable has not the expected type

   type Object is abstract tagged private;
   --  Root type for all SOAP types defined in this package

   type Object_Access is access all Object'Class;

   type Object_Safe_Pointer is tagged private;
   --  A safe pointer to a SOAP object, such objects are controlled so the
   --  memory is freed automatically.

   type Object_Set is array (Positive range <>) of Object_Safe_Pointer;
   --  A set of SOAP types. This is used to build arrays or records. We use
   --  Positive for the index to have the item index map the SOAP array
   --  element order.

   function Image (O : in Object) return String;
   --  Returns O value image.

   function XML_Image (O : in Object) return String;
   --  Returns O value encoded for use by the Payload object or Response
   --  object.

   function XML_Type (O : in Object) return String;
   --  Returns the XML type for the object.

   function Name (O : in Object'Class) return String;
   --  Returns name for object O.

   function "+" (O : in Object'Class) return Object_Safe_Pointer;
   --  Allocate an object into the heap and return a safe pointer to it.

   function "-" (O : in Object_Safe_Pointer) return Object'Class;
   --  Returns the object associated with the safe pointer.

   type Scalar is abstract new Object with private;
   --  Scalar types are using a by-copy semantic.

   type Composite is abstract new Object with private;
   --  Composite types are using a by-reference semantic for efficiency
   --  reason. Not that these types are not thread safe.

   function V (C : in Composite) return Object_Set is abstract;

   --------------
   -- Any Type --
   --------------

   XML_Any_Type : aliased constant String := "xsd:anyType";

   type XSD_Any_Type is new Object with private;

   function Image     (O : in XSD_Any_Type) return String;
   function XML_Image (O : in XSD_Any_Type) return String;
   function XML_Type  (O : in XSD_Any_Type) return String;

   function Any
     (V    : in Object'Class;
      Name : in String  := "item") return XSD_Any_Type;

   function V (O : in XSD_Any_Type) return Object_Access;

   -----------
   -- Array --
   -----------

   XML_Array     : constant String := "soapenc:Array";
   XML_Undefined : aliased constant String := "xsd:ur-type";

   type SOAP_Array is new Composite with private;

   function Image     (O : in SOAP_Array) return String;
   function XML_Image (O : in SOAP_Array) return String;
   function XML_Type  (O : in SOAP_Array) return String;

   function A
     (V         : in Object_Set;
      Name      : in String;
      Type_Name : in String := "")
      return SOAP_Array;
   --  Type_Name of the array's elements, if not specified it will be computed
   --  based on element's name.

   function Size (O : in SOAP_Array) return Natural;
   --  Returns the number of item into the array

   function V (O : in SOAP_Array; N : in Positive) return Object'Class;
   --  Returns SOAP_Array item at position N

   function V (O : in SOAP_Array) return Object_Set;

   ------------
   -- Base64 --
   ------------

   XML_Base64        : aliased constant String := "soapenc:base64";
   XML_Base64_Binary : constant String := "xsd:base64Binary";

   type SOAP_Base64 is new Scalar with private;

   function Image     (O : in SOAP_Base64) return String;
   function XML_Image (O : in SOAP_Base64) return String;
   function XML_Type  (O : in SOAP_Base64) return String;

   function B64
     (V    : in String;
      Name : in String := "item")
      return SOAP_Base64;

   function V (O : in SOAP_Base64) return String;

   -------------
   -- Boolean --
   -------------

   XML_Boolean : aliased constant String := "xsd:boolean";

   type XSD_Boolean is new Scalar with private;

   function Image     (O : in XSD_Boolean) return String;
   function XML_Image (O : in XSD_Boolean) return String;
   function XML_Type  (O : in XSD_Boolean) return String;

   function B (V : in Boolean; Name : in String  := "item") return XSD_Boolean;
   function V (O : in XSD_Boolean) return Boolean;

   ------------
   -- Double --
   ------------

   XML_Double : aliased constant String := "xsd:double";

   type XSD_Double is new Scalar with private;

   function Image     (O : in XSD_Double) return String;
   function XML_Image (O : in XSD_Double) return String;
   function XML_Type  (O : in XSD_Double) return String;

   function D
     (V    : in Long_Long_Float;
      Name : in String          := "item")
      return XSD_Double;

   function V (O : in XSD_Double) return Long_Long_Float;

   -----------
   -- Float --
   -----------

   XML_Float : aliased constant String := "xsd:float";

   type XSD_Float is new Scalar with private;

   function Image     (O : in XSD_Float) return String;
   function XML_Image (O : in XSD_Float) return String;
   function XML_Type  (O : in XSD_Float) return String;

   function F (V : in Long_Float; Name : in String := "item") return XSD_Float;
   function V (O : in XSD_Float) return Long_Float;

   -------------
   -- Integer --
   -------------

   XML_Int : aliased constant String := "xsd:int";

   type XSD_Integer is new Scalar with private;

   function Image     (O : in XSD_Integer) return String;
   function XML_Image (O : in XSD_Integer) return String;
   function XML_Type  (O : in XSD_Integer) return String;

   function I (V : in Integer; Name : in String := "item") return XSD_Integer;
   function V (O : in XSD_Integer) return Integer;

   ----------
   -- Long --
   ----------

   type Long is range -2**63 .. 2**63 - 1;

   XML_Long : aliased constant String := "xsd:long";

   type XSD_Long is new Scalar with private;

   function Image     (O : in XSD_Long) return String;
   function XML_Image (O : in XSD_Long) return String;
   function XML_Type  (O : in XSD_Long) return String;

   function L (V : in Long; Name : in String := "item") return XSD_Long;
   function V (O : in XSD_Long) return Long;

   ----------
   -- Null --
   ----------

   XML_Null : constant String := "1";

   type XSD_Null is new Scalar with private;

   function XML_Image (O : in XSD_Null) return String;
   function XML_Type  (O : in XSD_Null) return String;

   function N (Name : in String  := "item") return XSD_Null;

   ------------
   -- Record --
   ------------

   type SOAP_Record is new Composite with private;

   function Image     (O : in SOAP_Record) return String;
   function XML_Image (O : in SOAP_Record) return String;
   function XML_Type  (O : in SOAP_Record) return String;

   function R
     (V         : in Object_Set;
      Name      : in String;
      Type_Name : in String := "")
      return SOAP_Record;
   --  If Type_Name is omitted then the type name is the name of the record.
   --  Type_Name must be specified for item into an array for example.

   function V (O : in SOAP_Record; Name : in String) return Object'Class;
   --  Returns SOAP_Record field named Name

   function V (O : in SOAP_Record) return Object_Set;

   -----------
   -- Short --
   -----------

   type Short is range -2**15 .. 2**15 - 1;

   XML_Short : aliased constant String := "xsd:short";

   type XSD_Short is new Scalar with private;

   function Image     (O : in XSD_Short) return String;
   function XML_Image (O : in XSD_Short) return String;
   function XML_Type  (O : in XSD_Short) return String;

   function S (V : in Short; Name : in String := "item") return XSD_Short;
   function V (O : in XSD_Short) return Short;

   ------------
   -- String --
   ------------

   XML_String : aliased constant String := "xsd:string";

   type XSD_String is new Scalar with private;

   function Image     (O : in XSD_String) return String;
   function XML_Image (O : in XSD_String) return String;
   function XML_Type  (O : in XSD_String) return String;

   function S
     (V    : in String;
      Name : in String := "item")
      return XSD_String;

   function S
     (V    : in Unbounded_String;
      Name : in String  := "item")
      return XSD_String;

   function V (O : in XSD_String) return String;

   function V (O : in XSD_String) return Unbounded_String;

   -----------------
   -- TimeInstant --
   -----------------

   XML_Time_Instant : aliased constant String := "xsd:timeInstant";
   XML_Date_Time    : constant String := "xsd:dateTime";

   type XSD_Time_Instant is new Scalar with private;

   function Image     (O : in XSD_Time_Instant) return String;
   function XML_Image (O : in XSD_Time_Instant) return String;
   function XML_Type  (O : in XSD_Time_Instant) return String;

   subtype TZ is Integer range -11 .. +11;
   GMT : constant TZ := 0;

   function T
     (V        : in Ada.Calendar.Time;
      Name     : in String        := "item";
      Timezone : in TZ            := GMT)
      return XSD_Time_Instant;

   function V (O : in XSD_Time_Instant) return Ada.Calendar.Time;
   --  Returns a GMT date and time

   -----------------
   -- Enumeration --
   -----------------

   type SOAP_Enumeration is new Scalar with private;

   function Image     (O : in SOAP_Enumeration) return String;
   function XML_Image (O : in SOAP_Enumeration) return String;
   function XML_Type  (O : in SOAP_Enumeration) return String;

   function E
     (V         : in String;
      Type_Name : in String;
      Name      : in String := "item")
      return SOAP_Enumeration;

   function V (O : in SOAP_Enumeration) return String;

   ---------
   -- Get --
   ---------

   --  It is possible to pass an XSD_Any_Type to all get routines below. The
   --  proper value will be returned if the XSD_Any_Type is actually of this
   --  type.

   function Get (O : in Object'Class) return XSD_Any_Type;
   --  Returns O value as an XSD_Any_Type. Raises Data_Error if O is not a
   --  SOAP anyType.

   function Get (O : in Object'Class) return Integer;
   --  Returns O value as an Integer. Raises Data_Error if O is not a SOAP
   --  Integer.

   function Get (O : in Object'Class) return Short;
   --  Returns O value as a Short. Raises Data_Error if O is not a SOAP
   --  Short.

   function Get (O : in Object'Class) return Long;
   --  Returns O value as a Long. Raises Data_Error if O is not a SOAP
   --  Long.

   function Get (O : in Object'Class) return Long_Float;
   --  Returns O value as a Long_Float. Raises Data_Error if O is not a SOAP
   --  Float.

   function Get (O : in Object'Class) return Long_Long_Float;
   --  Returns O value as a Long_Long_Float. Raises Data_Error if O is not a
   --  SOAP Double.

   function Get (O : in Object'Class) return String;
   --  Returns O value as a String. Raises Data_Error if O is not a SOAP
   --  String.

   function Get (O : in Object'Class) return Unbounded_String;
   --  As above but returns an Unbounded_String

   function Get (O : in Object'Class) return Boolean;
   --  Returns O value as a Boolean. Raises Data_Error if O is not a SOAP
   --  Boolean.

   function Get (O : in Object'Class) return Ada.Calendar.Time;
   --  Returns O value as a Time. Raises Data_Error if O is not a SOAP
   --  Time.

   function Get (O : in Object'Class) return SOAP_Base64;
   --  Returns O value as a SOAP Base64. Raises Data_Error if O is not a SOAP
   --  Base64 object.

   function Get (O : in Object'Class) return SOAP_Record;
   --  Returns O value as a SOAP Struct. Raises Data_Error if O is not a SOAP
   --  Struct.

   function Get (O : in Object'Class) return SOAP_Array;
   --  Returns O value as a SOAP Array. Raises Data_Error if O is not a SOAP
   --  Array.

   ----------------
   -- Name space --
   ----------------

   procedure Set_Name_Space
     (O  : in out Object'Class;
      NS : in Name_Space.Object);
   --  Set the name space for object O

   function Name_Space (O : in Object'Class) return Name_Space.Object;
   --  Returns name space associated with object O

private

   use Ada.Strings.Unbounded;

   --  Object

   type Object is abstract new Ada.Finalization.Controlled with record
      Name : Unbounded_String;
      NS   : SOAP.Name_Space.Object;
   end record;

   --  Object_Safe_Pointer

   type Object_Safe_Pointer is new Ada.Finalization.Controlled with record
      O : Object_Access;
   end record;

   procedure Adjust     (O : in out Object_Safe_Pointer);
   pragma Inline (Adjust);

   procedure Finalize   (O : in out Object_Safe_Pointer);
   pragma Inline (Finalize);

   --  Scalar

   type Scalar is abstract new Object with null record;

   type Counter_Access is access Natural;

   --  Composite

   type Object_Set_Access is access Object_Set;

   type Composite is abstract new Object with record
      Ref_Counter : Counter_Access;
      O           : Object_Set_Access;
   end record;

   procedure Initialize (O : in out Composite);
   pragma Inline (Initialize);

   procedure Adjust     (O : in out Composite);
   pragma Inline (Adjust);

   procedure Finalize   (O : in out Composite);
   pragma Inline (Finalize);

   --  AnyType

   type XSD_Any_Type is new Object with record
      O : Object_Safe_Pointer;
   end record;

   --  Simple SOAP types

   type XSD_Integer is new Scalar with record
      V : Integer;
   end record;

   type XSD_Short is new Scalar with record
      V : Short;
   end record;

   type XSD_Long is new Scalar with record
      V : Long;
   end record;

   type XSD_Float is new Scalar with record
      V : Long_Float;
   end record;

   type XSD_Double is new Scalar with record
      V : Long_Long_Float;
   end record;

   type XSD_String is new Scalar with record
      V : Unbounded_String;
   end record;

   type XSD_Boolean is new Scalar with record
      V : Boolean;
   end record;

   type XSD_Time_Instant is new Scalar with record
      T        : Ada.Calendar.Time;
      Timezone : TZ;
   end record;

   type XSD_Null is new Scalar with null record;

   type SOAP_Base64 is new Scalar with record
      V : Unbounded_String;
   end record;

   type SOAP_Enumeration is new Scalar with record
      V         : Unbounded_String;
      Type_Name : Unbounded_String;
   end record;

   --  Composite SOAP types

   type SOAP_Array is new Composite with record
      Type_Name : Unbounded_String;
   end record;

   type SOAP_Record is new Composite with record
      Type_Name : Unbounded_String;
   end record;

end SOAP.Types;
