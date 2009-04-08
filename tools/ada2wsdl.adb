------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2003-2009, AdaCore                     --
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

with Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Characters.Handling;
with Ada.Characters.Conversions;

with Asis.Elements;
with Asis.Declarations;
with Asis.Text;

with AWS.Utils;

package body Ada2WSDL is

   use Ada;
   use Asis;
   use AWS;

   --------------
   -- Location --
   --------------

   function Location (E : Asis.Element) return String is

      function Image (Str : Wide_String) return String;
      --  Return Str as a lower-case and trimmed string

      -----------
      -- Image --
      -----------

      function Image (Str : Wide_String) return String is
      begin
         return Characters.Handling.To_Lower
           (Strings.Fixed.Trim
              (Characters.Conversions.To_String (Str), Strings.Both));
      end Image;

      E_Span    : constant Text.Span := Text.Element_Span (E);
      Unit      : constant Asis.Declaration
        := Elements.Unit_Declaration (Elements.Enclosing_Compilation_Unit (E));
      --  Unit containing element E

      Unit_Name : constant Asis.Element := Declarations.Names (Unit) (1);
      --  Unit name
   begin
      return Image (Text.Element_Image (Unit_Name)) & ".ads:"
        & Utils.Image (E_Span.First_Line)
        & ':' & Utils.Image (E_Span.First_Column);
   end Location;

   ----------------------
   -- Raise_Spec_Error --
   ----------------------

   procedure Raise_Spec_Error
     (E       : Asis.Element;
      Message : String) is
   begin
      Exceptions.Raise_Exception
        (Spec_Error'Identity, Location (E) & ": " & Message);
   end Raise_Spec_Error;

end Ada2WSDL;
