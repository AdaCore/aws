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

with Ada.Strings.Unbounded;

package SOAP.Types is

   Data_Error : exception;
   --  Raised when a variable has not the expected type.

   type Object is tagged private;

   function Image (O : in Object) return String;
   --  Returns O value image.

   function XML_Image (O : in Object) return String;
   --  Returns O value encoded for use by the Payload object or Response
   --  object.

   function Name (O : in Object) return String;
   --  Returns name for object O.

   function Get (O : in Object'Class) return Standard.Integer;
   --  Returns O value as an Integer. Raises Data_Error if O is not a SOAP
   --  Integer.

   function Get (O : in Object'Class) return Standard.Float;
   --  Returns O value as an Integer. Raises Data_Error if O is not a SOAP
   --  Float.

   function Get (O : in Object'Class) return Standard.String;
   --  Returns O value as a String. Raises Data_Error if O is not a SOAP
   --  String.

   type Scalars is abstract new Types.Object with private;

   -------------
   -- Integer --
   -------------

   XML_Int : constant Standard.String := "xsd:int";

   type Integer is new Scalars with private;

   function Image     (O : in Integer) return String;
   function XML_Image (O : in Integer) return String;

   function I (Name : in String; V : in Standard.Integer) return Integer;
   function V (O : in Integer) return Standard.Integer;

   -----------
   -- Float --
   -----------

   XML_Float : constant Standard.String := "xsd:float";

   type Float is new Scalars with private;

   function Image     (O : in Float) return String;
   function XML_Image (O : in Float) return String;

   function F (Name : in String; V : in Standard.Float) return Float;
   function V (O : in Float) return Standard.Float;

   ------------
   -- String --
   ------------

   XML_String : constant Standard.String := "xsd:string";

   type String is new Scalars with private;

   function Image     (O : in String) return Standard.String;
   function XML_Image (O : in String) return Standard.String;

   function S
     (Name : in Standard.String;
      V    : in Standard.String)
     return String;

   function V (O : in String) return Standard.String;

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

   type String is new Scalars with record
      V : Unbounded_String;
   end record;

end SOAP.Types;
