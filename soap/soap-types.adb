
with Ada.Float_Text_IO;
with Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Tags;

--  with SOAP.Types.Scalars;

package body SOAP.Types is

   use Ada;

   -------
   -- F --
   -------

   function F (Name : in String; V : in Standard.Float) return Float is
   begin
      return (To_Unbounded_String (Name), V);
   end F;

   -----------
   -- Image --
   -----------

   function Image (O : in Object) return String is
   begin
      return "";
   end Image;

   ----------
   -- Name --
   ----------

   function Name (O : in Object) return String is
   begin
      return To_String (O.Name);
   end Name;

   -------------------
   -- Payload_Image --
   -------------------

   function Payload_Image (O : in Object) return String is
   begin
      return "";
   end Payload_Image;

   -------
   -- V --
   -------

   function Get (O : in Object'Class) return Standard.Integer is
      use type Ada.Tags.Tag;
   begin
      if O'Tag = Types.Integer'Tag then
         return V (Integer (O));

      else
         Exceptions.Raise_Exception
           (Data_Error'Identity,
            "Integer expected, found " & Tags.Expanded_Name (O'Tag));
      end if;
   end Get;

   function Get (O : in Object'Class) return Standard.Float is
      use type Ada.Tags.Tag;
   begin
      if O'Tag = Types.Float'Tag then
         return V (Float (O));

      else
         Exceptions.Raise_Exception
           (Data_Error'Identity,
            "Float expected, found " & Tags.Expanded_Name (O'Tag));
      end if;
   end Get;

   -------
   -- I --
   -------

   function I (Name : in String; V : in Standard.Integer) return Integer is
   begin
      return (To_Unbounded_String (Name), V);
   end I;

   -----------
   -- Image --
   -----------

   function Image (O : in Integer) return String is
      V : constant String := Standard.Integer'Image (O.V);
   begin
      return V (V'First + 1 .. V'Last);
   end Image;

   function Image (O : in Float) return String is
      use Ada;

      Result : String (1 .. Standard.Float'Width);
   begin
      Float_Text_IO.Put (Result, O.V, Exp => 0);
      return Strings.Fixed.Trim (Result, Strings.Both);
   end Image;

   -------------------
   -- Payload_Image --
   -------------------

   function Payload_Image (O : in Integer) return String is
   begin
      return "<" & Name (O) & " xsi:type=""" & XML_Int & """>"
        & Image (O)
        & "</" & Name (O) & '>';
   end Payload_Image;

   function Payload_Image (O : in Float) return String is
   begin
      return "<" & Name (O) & " xsi:type=""" & XML_Float & """>"
        & Image (O)
        & "</" & Name (O) & '>';
   end Payload_Image;

   -------
   -- V --
   -------

   function V (O : in Integer) return Standard.Integer is
   begin
      return O.V;
   end V;

   function V (O : in Float) return Standard.Float is
   begin
      return O.V;
   end V;

end SOAP.Types;
