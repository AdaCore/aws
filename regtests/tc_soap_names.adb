
--  $Id$

with Ada.Strings.Fixed;     use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Tags;              use Ada.Tags;

package body Tc_Soap_Names is

   CrLf : constant String := (1 => Ascii.Cr, 2 => Ascii.Lf);

   function Soap_Image
     (Obj    : in SOAP.Types.Object'Class;
      Indent : in Integer                 := 0)
     return String
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
               Result := Result & Soap_Image (Arr (I).O.all, Indent + 3);
            end loop;
         end;

      elsif Obj in SOAP_Record then
         declare
            Rec : Object_Set := V (SOAP_Record (Obj));
         begin
            Result := To_Unbounded_String
              (Ind & "RECORD " & Name (Obj) & CrLf);
            for I in Rec'Range loop
               Result := Result & Soap_Image (Rec (I).O.all, Indent + 3);
            end loop;
         end;

      else
         return Ind & Name (Obj) & " " & Expanded_Name (Obj'Tag) & "=" &
           Image (Obj) & CrLf;
      end if;

      return To_String (Result);
   end Soap_Image;

end Tc_Soap_Names;
