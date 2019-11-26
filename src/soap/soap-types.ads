------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2001-2019, AdaCore                     --
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

--  This package contains all SOAP types supported by this implementation.
--  Here are some notes about adding support for a new SOAP type (not a
--  container) and the corresponding WSDL support:
--
--  1. Add new type derived from scalar in this package. Implements all
--     inherited routines (Image, XML_Image and XML_Type). Implements also
--     a constructor for this new type and a routine named V to get the
--     value as an Ada type.
--
--  2. In SOAP.Parameters add corresponding Get routine.
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
with SOAP.WSDL.Schema;

package SOAP.Types is

   use Ada;
   use Ada.Strings.Unbounded;

   subtype Encoding_Style is WSDL.Schema.Encoding_Style;
   --  SOAP encoding style for the entities

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

   Empty_Object_Set : constant Object_Set;

   function Image (O : Object) return String;
   --  Returns O value image

   function Is_Empty (O : Object) return Boolean;
   --  Returns True if the object is empty Array, Empty Record or null value

   procedure XML_Image
     (O        : Object;
      Result   : in out Unbounded_String;
      Encoding : Encoding_Style := WSDL.Schema.Encoded;
      Schema   : WSDL.Schema.Definition := WSDL.Schema.Empty);
   --  Returns O value encoded for use by the Payload object or Response
   --  object. The generated characters are appened to Result.

   function XML_Image (O : Object'Class) return String;
   --  Returns O value encoded for use by the Payload object or Response
   --  object.

   function XML_Type (O : Object) return String;
   --  Returns the XML type for the object

   function Name (O : Object'Class) return String;
   --  Returns name for object O

   function Type_Name (O : Object'Class) return String;
   --  Returns the type name for object O

   function "+" (O : Object'Class) return Object_Safe_Pointer;
   --  Allocate an object into the heap and return a safe pointer to it

   function "-" (O : Object_Safe_Pointer) return Object'Class;
   --  Returns the object associated with the safe pointer

   type Scalar is abstract new Object with private;
   --  Scalar types are using a by-copy semantic

   type Composite is abstract new Object with private;
   --  Composite types are using a by-reference semantic for efficiency
   --  reason. Not that these types are not thread safe.

   function V (O : Composite) return Object_Set;

   overriding function Is_Empty (O : Composite) return Boolean;

   --------------
   -- Any Type --
   --------------

   XML_Any_Type : aliased constant String := "xsd:anyType";

   type XSD_Any_Type is new Object with private;

   overriding function  XML_Type  (O : XSD_Any_Type) return String;
   overriding function  Image     (O : XSD_Any_Type) return String;
   overriding procedure XML_Image
     (O        : XSD_Any_Type;
      Result   : in out Unbounded_String;
      Encoding : Encoding_Style := WSDL.Schema.Encoded;
      Schema   : WSDL.Schema.Definition := WSDL.Schema.Empty);

   function Any
     (V         : Object'Class;
      Name      : String := "item";
      Type_Name : String := "";
      NS        : Name_Space.Object := Name_Space.No_Name_Space)
      return XSD_Any_Type;

   function V (O : XSD_Any_Type) return Object_Access;

   -----------
   -- Array --
   -----------

   XML_Array     : constant String := "soapenc:Array";
   XML_Undefined : aliased constant String := "xsd:ur-type";

   type SOAP_Array is new Composite with private;

   overriding function Image (O : SOAP_Array) return String;
   overriding procedure XML_Image
     (O        : SOAP_Array;
      Result   : in out Unbounded_String;
      Encoding : Encoding_Style := WSDL.Schema.Encoded;
      Schema   : WSDL.Schema.Definition := WSDL.Schema.Empty);

   function A
     (V         : Object_Set;
      Name      : String;
      Type_Name : String := "";
      NS        : Name_Space.Object := Name_Space.No_Name_Space)
      return SOAP_Array;
   --  Type_Name of the array's elements, if not specified it will be computed
   --  based on element's name.

   function Size (O : SOAP_Array) return Natural;
   --  Returns the number of item into the array

   function V (O : SOAP_Array; N : Positive) return Object'Class;
   --  Returns SOAP_Array item at position N

   ----------
   --  Set --
   ----------

   type SOAP_Set is new Composite with private;
   --  A set is like an array but to record multi-occurence of parameters. The
   --  SOAP message does not contain the enclosing SOAP array XML tag.

   overriding function Image (O : SOAP_Set) return String;
   overriding procedure XML_Image
     (O        : SOAP_Set;
      Result   : in out Unbounded_String;
      Encoding : Encoding_Style := WSDL.Schema.Encoded;
      Schema   : WSDL.Schema.Definition := WSDL.Schema.Empty);

   function Set
     (V         : Object_Set;
      Name      : String;
      Type_Name : String := "";
      NS        : SOAP.Name_Space.Object := SOAP.Name_Space.No_Name_Space)
      return SOAP_Set;
   --  Type_Name of the array's elements, if not specified it will be computed
   --  based on element's name.

   ------------
   -- Base64 --
   ------------

   XML_Base64        : aliased constant String := "soapenc:base64";
   XML_Base64_Binary : aliased constant String := "xsd:base64Binary";

   type SOAP_Base64 is new Scalar with private;

   overriding function Image (O : SOAP_Base64) return String;

   function B64
     (V         : String;
      Name      : String := "item";
      Type_Name : String := XML_Base64;
      NS        : Name_Space.Object := Name_Space.No_Name_Space)
      return SOAP_Base64;

   function V (O : SOAP_Base64) return String;

   -------------
   -- Boolean --
   -------------

   XML_Boolean : aliased constant String := "xsd:boolean";

   type XSD_Boolean is new Scalar with private;

   overriding function Image (O : XSD_Boolean) return String;

   function B
     (V         : Boolean;
      Name      : String := "item";
      Type_Name : String := XML_Boolean;
      NS        : Name_Space.Object := Name_Space.No_Name_Space)
      return XSD_Boolean;
   function V (O : XSD_Boolean) return Boolean;

   ----------
   -- Byte --
   ----------

   type Byte is range -2**7 .. 2**7 - 1;

   XML_Byte : aliased constant String := "xsd:byte";

   type XSD_Byte is new Scalar with private;

   overriding function Image (O : XSD_Byte) return String;

   function B
     (V         : Byte;
      Name      : String := "item";
      Type_Name : String := XML_Byte;
      NS        : Name_Space.Object := Name_Space.No_Name_Space)
      return XSD_Byte;
   function V (O : XSD_Byte) return Byte;

   ------------
   -- Double --
   ------------

   XML_Double : aliased constant String := "xsd:double";

   type XSD_Double is new Scalar with private;

   overriding function Image (O : XSD_Double) return String;

   function D
     (V         : Long_Float;
      Name      : String := "item";
      Type_Name : String := XML_Double;
      NS        : Name_Space.Object := Name_Space.No_Name_Space)
      return XSD_Double;

   function V (O : XSD_Double) return Long_Float;

   -----------
   -- Float --
   -----------

   XML_Float : aliased constant String := "xsd:float";

   type XSD_Float is new Scalar with private;

   overriding function Image (O : XSD_Float) return String;

   function F
     (V         : Float;
      Name      : String := "item";
      Type_Name : String := XML_Float;
      NS        : Name_Space.Object := Name_Space.No_Name_Space)
      return XSD_Float;
   function V (O : XSD_Float) return Float;

   -------------
   -- Integer --
   -------------

   XML_Int : aliased constant String := "xsd:int";

   type XSD_Integer is new Scalar with private;

   overriding function Image (O : XSD_Integer) return String;

   function I
     (V         : Integer;
      Name      : String := "item";
      Type_Name : String := XML_Int;
      NS        : Name_Space.Object := Name_Space.No_Name_Space)
      return XSD_Integer;
   function V (O : XSD_Integer) return Integer;

   ----------
   -- Long --
   ----------

   type Long is range -2**63 .. 2**63 - 1;

   XML_Long : aliased constant String := "xsd:long";

   type XSD_Long is new Scalar with private;

   overriding function Image (O : XSD_Long) return String;

   function L
     (V         : Long;
      Name      : String := "item";
      Type_Name : String := XML_Long;
      NS        : Name_Space.Object := Name_Space.No_Name_Space)
      return XSD_Long;
   function V (O : XSD_Long) return Long;

   ----------
   -- Null --
   ----------

   type XSD_Null is new Scalar with private;

   overriding procedure XML_Image
     (O        : XSD_Null;
      Result   : in out Unbounded_String;
      Encoding : Encoding_Style := WSDL.Schema.Encoded;
      Schema   : WSDL.Schema.Definition := WSDL.Schema.Empty);

   function N
     (Name      : String;
      Type_Name : String;
      NS        : SOAP.Name_Space.Object := SOAP.Name_Space.No_Name_Space)
      return XSD_Null;

   overriding function Is_Empty (O : XSD_Null) return Boolean;

   ------------
   -- Record --
   ------------

   type SOAP_Record is new Composite with private;

   overriding function Image (O : SOAP_Record) return String;
   overriding procedure XML_Image
     (O        : SOAP_Record;
      Result   : in out Unbounded_String;
      Encoding : Encoding_Style := WSDL.Schema.Encoded;
      Schema   : WSDL.Schema.Definition := WSDL.Schema.Empty);

   function R
     (V         : Object_Set;
      Name      : String;
      Type_Name : String := "";
      NS        : Name_Space.Object := Name_Space.No_Name_Space)
      return SOAP_Record;
   --  If Type_Name is omitted then the type name is the name of the record.
   --  Type_Name must be specified for item into an array for example.

   function V (O : SOAP_Record; Name : String) return Object'Class;
   --  Returns SOAP_Record field named Name

   function V (O : SOAP_Record; Name : String) return Object_Set;
   --  Returns SOAP_Record fields named Name

   function Exists (O : SOAP_Record; Field_Name : String) return Boolean;
   --  Returns True if the record O constains Field_Name

   -----------
   -- Short --
   -----------

   type Short is range -2**15 .. 2**15 - 1;

   XML_Short : aliased constant String := "xsd:short";

   type XSD_Short is new Scalar with private;

   overriding function Image (O : XSD_Short) return String;

   function S
     (V         : Short;
      Name      : String := "item";
      Type_Name : String := XML_Short;
      NS        : Name_Space.Object := Name_Space.No_Name_Space)
      return XSD_Short;
   function V (O : XSD_Short) return Short;

   ------------
   -- String --
   ------------

   XML_String : aliased constant String := "xsd:string";

   type XSD_String is new Scalar with private;

   overriding function Image (O : XSD_String) return String;

   function S
     (V    : String;
      Name : String      := "item";
      Type_Name : String := XML_String;
      NS        : Name_Space.Object := Name_Space.No_Name_Space)
      return XSD_String;

   function S
     (V         : Unbounded_String;
      Name      : String  := "item";
      Type_Name : String := XML_String;
      NS        : Name_Space.Object := Name_Space.No_Name_Space)
      return XSD_String;

   function V (O : XSD_String) return String;

   function V (O : XSD_String) return Unbounded_String;

   -----------------
   -- TimeInstant --
   -----------------

   subtype Local_Time is Calendar.Time;
   --  All times are local time. This means that a timeInstant is always
   --  converted to a local time for the running host.

   XML_Time_Instant : aliased constant String := "xsd:timeInstant";
   XML_Date_Time    : aliased constant String := "xsd:dateTime";

   type XSD_Time_Instant is new Scalar with private;

   overriding function Image (O : XSD_Time_Instant) return String;

   function T
     (V         : Local_Time;
      Name      : String := "item";
      Type_Name : String := XML_Time_Instant;
      NS        : Name_Space.Object := Name_Space.No_Name_Space)
      return XSD_Time_Instant;

   function V (O : XSD_Time_Instant) return Local_Time;
   --  Returns a GMT date and time

   --------------
   -- Duration --
   --------------

   XML_Duration : aliased constant String := "xsd:duration";

   type XSD_Duration is new Scalar with private;

   overriding function Image (O : XSD_Duration) return String;

   function D
     (V         : Duration;
      Name      : String := "item";
      Type_Name : String := XML_Duration;
      NS        : Name_Space.Object := Name_Space.No_Name_Space)
      return XSD_Duration;

   function V (O : XSD_Duration) return Duration;
   --  Returns the Ada duration

   -------------------
   -- Unsigned_Long --
   -------------------

   type Unsigned_Long is mod 2**64;

   XML_Unsigned_Long : aliased constant String := "xsd:unsignedLong";

   type XSD_Unsigned_Long is new Scalar with private;

   overriding function Image (O : XSD_Unsigned_Long) return String;

   function UL
     (V         : Unsigned_Long;
      Name      : String := "item";
      Type_Name : String := XML_Unsigned_Long;
      NS        : Name_Space.Object := Name_Space.No_Name_Space)
      return XSD_Unsigned_Long;
   function V (O : XSD_Unsigned_Long) return Unsigned_Long;

   ------------------
   -- Unsigned_Int --
   ------------------

   type Unsigned_Int is mod 2**32;

   XML_Unsigned_Int : aliased constant String := "xsd:unsignedInt";

   type XSD_Unsigned_Int is new Scalar with private;

   overriding function Image (O : XSD_Unsigned_Int) return String;

   function UI
     (V         : Unsigned_Int;
      Name      : String := "item";
      Type_Name : String := XML_Unsigned_Int;
      NS        : Name_Space.Object := Name_Space.No_Name_Space)
      return XSD_Unsigned_Int;
   function V (O : XSD_Unsigned_Int) return Unsigned_Int;

   --------------------
   -- Unsigned_Short --
   --------------------

   type Unsigned_Short is mod 2**16;

   XML_Unsigned_Short : aliased constant String := "xsd:unsignedShort";

   type XSD_Unsigned_Short is new Scalar with private;

   overriding function Image (O : XSD_Unsigned_Short) return String;

   function US
     (V         : Unsigned_Short;
      Name      : String := "item";
      Type_Name : String := XML_Unsigned_Short;
      NS        : Name_Space.Object := Name_Space.No_Name_Space)
      return XSD_Unsigned_Short;
   function V (O : XSD_Unsigned_Short) return Unsigned_Short;

   -------------------
   -- Unsigned_Byte --
   -------------------

   type Unsigned_Byte is mod 2**8;

   XML_Unsigned_Byte : aliased constant String := "xsd:unsignedByte";

   type XSD_Unsigned_Byte is new Scalar with private;

   overriding function Image (O : XSD_Unsigned_Byte) return String;

   function UB
     (V         : Unsigned_Byte;
      Name      : String := "item";
      Type_Name : String := XML_Unsigned_Byte;
      NS        : Name_Space.Object := Name_Space.No_Name_Space)
      return XSD_Unsigned_Byte;
   function V (O : XSD_Unsigned_Byte) return Unsigned_Byte;

   -----------------
   -- Enumeration --
   -----------------

   type SOAP_Enumeration is new Scalar with private;

   overriding function Image      (O : SOAP_Enumeration) return String;
   overriding procedure XML_Image
     (O        : SOAP_Enumeration;
      Result   : in out Unbounded_String;
      Encoding : Encoding_Style := WSDL.Schema.Encoded;
      Schema   : WSDL.Schema.Definition := WSDL.Schema.Empty);

   function E
     (V         : String;
      Type_Name : String;
      Name      : String := "item";
      NS        : Name_Space.Object := Name_Space.No_Name_Space)
      return SOAP_Enumeration;

   function V (O : SOAP_Enumeration) return String;

   ---------
   -- Get --
   ---------

   --  It is possible to pass an XSD_Any_Type to all get routines below. The
   --  proper value will be returned if the XSD_Any_Type is actually of this
   --  type.

   function Get (O : Object'Class) return XSD_Any_Type;
   --  Returns O value as an XSD_Any_Type. Raises Data_Error if O is not a
   --  SOAP anyType.

   function Get (O : Object'Class) return Long;
   --  Returns O value as a Long. Raises Data_Error if O is not a SOAP
   --  Long.

   function Get (O : Object'Class) return Integer;
   --  Returns O value as an Integer. Raises Data_Error if O is not a SOAP
   --  Integer.

   function Get (O : Object'Class) return Short;
   --  Returns O value as a Short. Raises Data_Error if O is not a SOAP
   --  Short.

   function Get (O : Object'Class) return Byte;
   --  Returns O value as a Byte. Raises Data_Error if O is not a SOAP
   --  Byte.

   function Get (O : Object'Class) return Float;
   --  Returns O value as a Long_Float. Raises Data_Error if O is not a SOAP
   --  Float.

   function Get (O : Object'Class) return Long_Float;
   --  Returns O value as a Long_Long_Float. Raises Data_Error if O is not a
   --  SOAP Double.

   function Get (O : Object'Class) return String;
   --  Returns O value as a String. Raises Data_Error if O is not a SOAP
   --  String.

   function Get (O : Object'Class) return Unbounded_String;
   --  As above but returns an Unbounded_String

   function Get (O : Object'Class) return Boolean;
   --  Returns O value as a Boolean. Raises Data_Error if O is not a SOAP
   --  Boolean.

   function Get (O : Object'Class) return Local_Time;
   --  Returns O value as a Time. Raises Data_Error if O is not a SOAP
   --  Time.

   function Get (O : Object'Class) return Duration;
   --  Returns O value as a Duration. Raises Data_Error if O is not a SOAP
   --  Duration.

   function Get (O : Object'Class) return Unsigned_Long;
   --  Returns O value as a Unsigned_Long. Raises Data_Error if O is not a SOAP
   --  Unsigned_Long.

   function Get (O : Object'Class) return Unsigned_Int;
   --  Returns O value as a Unsigned_Byte. Raises Data_Error if O is not a SOAP
   --  Unsigned_Int.

   function Get (O : Object'Class) return Unsigned_Short;
   --  Returns O value as a Unsigned_Short. Raises Data_Error if O is not a
   --  SOAP Unsigned_Short.

   function Get (O : Object'Class) return Unsigned_Byte;
   --  Returns O value as a Unsigned_Byte. Raises Data_Error if O is not a SOAP
   --  Unsigned_Byte.

   function Get (O : Object'Class) return SOAP_Base64;
   --  Returns O value as a SOAP Base64. Raises Data_Error if O is not a SOAP
   --  Base64 object.

   function Get (O : Object'Class) return SOAP_Record;
   --  Returns O value as a SOAP Struct. Raises Data_Error if O is not a SOAP
   --  Struct.

   function Get (O : Object'Class) return SOAP_Array;
   --  Returns O value as a SOAP Array. Raises Data_Error if O is not a SOAP
   --  Array.

   ----------------
   -- Name space --
   ----------------

   procedure Set_Name_Space
     (O  : in out Object'Class;
      NS : Name_Space.Object);
   --  Set the name space for object O

   function Name_Space (O : Object'Class) return Name_Space.Object;
   --  Returns name space associated with object O

   procedure Rename (O : in out Object'Class; Name : String);
   --  Set the name to the object

   function Rename (O : Object'Class; Name : String) return Object'Class;
   --  Return the same object with changed name

private

   --  Object

   type Object is abstract new Ada.Finalization.Controlled with record
      Name      : Unbounded_String;
      Type_Name : Unbounded_String;
      NS        : SOAP.Name_Space.Object;
   end record;

   --  Object_Safe_Pointer

   type Object_Safe_Pointer is new Ada.Finalization.Controlled with record
      O : Object_Access;
   end record;

   overriding procedure Adjust   (O : in out Object_Safe_Pointer) with Inline;

   overriding procedure Finalize (O : in out Object_Safe_Pointer) with Inline;

   --  Scalar

   type Scalar is abstract new Object with null record;

   type Counter_Access is access Natural;

   --  Composite

   Empty_Object_Set : constant Object_Set := Object_Set'(1 .. 0 => <>);

   type Object_Set_Access is access Object_Set;

   type Composite is abstract new Object with record
      Ref_Counter : Counter_Access;
      O           : Object_Set_Access;
   end record;

   overriding procedure Initialize (O : in out Composite) with Inline;

   overriding procedure Adjust     (O : in out Composite) with Inline;

   overriding procedure Finalize   (O : in out Composite) with Inline;

   --  AnyType

   type XSD_Any_Type is new Object with record
      O : Object_Safe_Pointer;
   end record;

   --  Simple SOAP types

   type XSD_Long is new Scalar with record
      V : Long;
   end record;

   type XSD_Integer is new Scalar with record
      V : Integer;
   end record;

   type XSD_Short is new Scalar with record
      V : Short;
   end record;

   type XSD_Byte is new Scalar with record
      V : Byte;
   end record;

   type XSD_Float is new Scalar with record
      V : Float;
   end record;

   type XSD_Double is new Scalar with record
      V : Long_Float;
   end record;

   type XSD_String is new Scalar with record
      V : Unbounded_String;
   end record;

   type XSD_Boolean is new Scalar with record
      V : Boolean;
   end record;

   type XSD_Time_Instant is new Scalar with record
      T : Local_Time;
   end record;

   type XSD_Duration is new Scalar with record
      V : Duration;
   end record;

   type XSD_Unsigned_Long is new Scalar with record
      V : Unsigned_Long;
   end record;

   type XSD_Unsigned_Int is new Scalar with record
      V : Unsigned_Int;
   end record;

   type XSD_Unsigned_Short is new Scalar with record
      V : Unsigned_Short;
   end record;

   type XSD_Unsigned_Byte is new Scalar with record
      V : Unsigned_Byte;
   end record;

   type XSD_Null is new Scalar with null record;

   type SOAP_Base64 is new Scalar with record
      V : Unbounded_String;
   end record;

   type SOAP_Enumeration is new Scalar with record
      V : Unbounded_String;
   end record;

   --  Composite SOAP types

   type SOAP_Array is new Composite with null record;

   type SOAP_Set is new SOAP_Array with null record;

   type SOAP_Record is new Composite with null record;

end SOAP.Types;
