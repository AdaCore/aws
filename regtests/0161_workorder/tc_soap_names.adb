------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2008-2012, AdaCore                     --
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

with Ada.Strings.Fixed;     use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Tags;              use Ada.Tags;

package body Tc_Soap_Names is

   CrLf : constant String := (1 => Ascii.Cr, 2 => Ascii.Lf);

   function Soap_Image
     (Obj    : SOAP.Types.Object'Class;
      Indent : Integer := 0) return String
   is
      Result : Unbounded_String;
      Ind : String := Indent * ' ';
   begin
      if Obj in SOAP_Array then
         declare
            Arr : Object_Set := V (SOAP_Array (Obj));
         begin
            Result := To_Unbounded_String (Ind & "ARRAY " & Name (Obj) & CrLf);
            for I in Arr'Range loop
               Result := Result & Soap_Image (-Arr (I), Indent + 3);
            end loop;
         end;

      elsif Obj in SOAP_Record then
         declare
            Rec : Object_Set := V (SOAP_Record (Obj));
         begin
            Result := To_Unbounded_String
              (Ind & "RECORD " & Name (Obj) & CrLf);
            for I in Rec'Range loop
               Result := Result & Soap_Image (-Rec (I), Indent + 3);
            end loop;
         end;

      else
         return Ind & Name (Obj) & " " & Expanded_Name (Obj'Tag) & "=" &
           Image (Obj) & CrLf;
      end if;

      return To_String (Result);
   end Soap_Image;

end Tc_Soap_Names;
