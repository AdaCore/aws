
with Ada.Strings.Unbounded;

package SOAP.Types is

   Data_Error : exception;

   type Object is tagged private;

   function Image (O : in Object) return String;
   --  Returns O value image.

   function Payload_Image (O : in Object) return String;
   --  Returns O value encoded for use by the Payload object.

   function Name  (O : in Object) return String;
   --  Returns name for object O.

   function Get (O : in Object'Class) return Standard.Integer;
   --  Returns O value as an Integer. Raises Data_Error if O is not a SOAP
   --  Integer.

   function Get (O : in Object'Class) return Standard.Float;
   --  Returns O value as an Integer. Raises Data_Error if O is not a SOAP
   --  Integer.

   type Scalars is abstract new Types.Object with private;

   -------------
   -- Integer --
   -------------

   XML_Int : constant String := "xsd:int";

   type Integer is new Scalars with private;

   function Image         (O : in Integer) return String;
   function Payload_Image (O : in Integer) return String;

   function I (Name : in String; V : in Standard.Integer) return Integer;
   function V (O : in Integer) return Standard.Integer;

   -----------
   -- Float --
   -----------

   XML_Float : constant String := "xsd:float";

   type Float is new Scalars with private;

   function Image         (O : in Float) return String;
   function Payload_Image (O : in Float) return String;

   function F (Name : in String; V : in Standard.Float) return Float;
   function V (O : in Float) return Standard.Float;

private

   use Ada.Strings.Unbounded;

   type Object is tagged record
      Name : Unbounded_String;
   end record;

   type Scalars is abstract new Object with null record;

   type Integer is new Scalars with record
      V : Standard.Integer;
   end record;

   type Float is new Scalars with record
      V : Standard.Float;
   end record;

end SOAP.Types;
