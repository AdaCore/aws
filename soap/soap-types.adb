------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2000-2001                          --
--                                ACT-Europe                                --
--                                                                          --
--  Authors: Dmitriy Anisimov - Pascal Obry                                 --
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

with Ada.Float_Text_IO;
with Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Tags;

package body SOAP.Types is

   use Ada;

   function xsi_type (Name : in Standard.String) return Standard.String;
   --  Returns the xsi:type field for the XML type representation whose name
   --  is passed as argument.

   -------
   -- F --
   -------

   function F
     (Name : in Standard.String;
      V    : in Standard.Float)
     return Float is
   begin
      return (To_Unbounded_String (Name), V);
   end F;

   ---------
   -- Get --
   ---------

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

   function Get (O : in Object'Class) return Standard.String is
      use type Ada.Tags.Tag;
   begin
      if O'Tag = Types.String'Tag then
         return V (String (O));

      else
         Exceptions.Raise_Exception
           (Data_Error'Identity,
            "String expected, found " & Tags.Expanded_Name (O'Tag));
      end if;
   end Get;

   -------
   -- I --
   -------

   function I
     (Name : in Standard.String;
      V    : in Standard.Integer)
     return Integer is
   begin
      return (To_Unbounded_String (Name), V);
   end I;

   -----------
   -- Image --
   -----------

   function Image (O : in Object) return Standard.String is
   begin
      return "";
   end Image;

   function Image (O : in Integer) return Standard.String is
      V : constant Standard.String := Standard.Integer'Image (O.V);
   begin
      return V (V'First + 1 .. V'Last);
   end Image;

   function Image (O : in Float) return Standard.String is
      use Ada;

      Result : Standard.String (1 .. Standard.Float'Width);
   begin
      Float_Text_IO.Put (Result, O.V, Exp => 0);
      return Strings.Fixed.Trim (Result, Strings.Both);
   end Image;

   function Image (O : in String) return Standard.String is
   begin
      return To_String (O.V);
   end Image;

   ----------
   -- Name --
   ----------

   function Name (O : in Object) return Standard.String is
   begin
      return To_String (O.Name);
   end Name;

   -------
   -- S --
   -------

   function S
     (Name : in Standard.String;
      V    : in Standard.String)
     return String is
   begin
      return (To_Unbounded_String (Name), To_Unbounded_String (V));
   end S;

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

   function V (O : in String) return Standard.String is
   begin
      return To_String (O.V);
   end V;

   ---------------
   -- XML_Image --
   ---------------

   function XML_Image (O : in Object) return Standard.String is
   begin
      return "";
   end XML_Image;

   function XML_Image (O : in Integer) return Standard.String is
   begin
      return "<" & Name (O) & xsi_type (XML_Int) & '>'
        & Image (O)
        & "</" & Name (O) & '>';
   end XML_Image;

   function XML_Image (O : in Float) return Standard.String is
   begin
      return "<" & Name (O) & xsi_type (XML_Float) & '>'
        & Image (O)
        & "</" & Name (O) & '>';
   end XML_Image;

   function XML_Image (O : in String) return Standard.String is
   begin
      return "<" & Name (O) & xsi_type (XML_String) & '>'
        & Image (O)
        & "</" & Name (O) & '>';
   end XML_Image;

   --------------
   -- xsi_type --
   --------------

   function xsi_type (Name : in Standard.String) return Standard.String is
   begin
      return " xsi:type=""" & Name & '"';
   end xsi_type;

end SOAP.Types;
