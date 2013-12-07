------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2003-2013, AdaCore                     --
--                                                                          --
--  This is free software;  you can redistribute it  and/or modify it       --
--  under terms of the  GNU General Public License as published  by the     --
--  Free Software  Foundation;  either version 3,  or (at your option) any  --
--  later version.  This software is distributed in the hope  that it will  --
--  be useful, but WITHOUT ANY WARRANTY;  without even the implied warranty --
--  of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU     --
--  General Public License for  more details.                               --
--                                                                          --
--  You should have  received  a copy of the GNU General  Public  License   --
--  distributed  with  this  software;   see  file COPYING3.  If not, go    --
--  to http://www.gnu.org/licenses for a complete copy of the license.      --
------------------------------------------------------------------------------

with Ada.Characters.Conversions;
with Ada.Characters.Handling;
with Ada.Exceptions;
with Ada.Strings.Fixed;

with Asis.Declarations;
with Asis.Elements;
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
      Unit      : constant Asis.Declaration :=
                    Elements.Unit_Declaration
                      (Elements.Enclosing_Compilation_Unit (E));
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
