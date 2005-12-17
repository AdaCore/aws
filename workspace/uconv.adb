
--  Test availability of the Unchecked_Conversion between
--  Stream_Element_Array and String

with Ada.Unchecked_Conversion;
with Ada.Streams; use Ada.Streams;
with Ada.Text_IO;
with Ada.Command_Line;

procedure Uconv is

   Sample : constant String :=
      "$Author$" & ASCII.LF &
      "$Date$" & ASCII.LF &
      "$Name$" & ASCII.LF &
      "$Locker$" & ASCII.LF &
      "$RCSfile$" & ASCII.LF &
      "$Revision$" & ASCII.LF &
      "$Source$" & ASCII.LF &
      "$State$" & ASCII.LF;

   subtype Fixed_String is String (Sample'First .. Sample'Last);

   subtype Fixed_Array is Stream_Element_Array
     (Stream_Element_Offset (Sample'First)
      .. Stream_Element_Offset (Sample'Last));

   function To_Stream_Elements is
     new Ada.Unchecked_Conversion (Fixed_String, Fixed_Array);

   function To_String is
     new Ada.Unchecked_Conversion (Fixed_Array, Fixed_String);

begin
   if Fixed_Array'Size = Fixed_String'Size
   and then Fixed_Array'Alignment = Fixed_String'Alignment
   and then Sample = To_String (To_Stream_Elements (Sample)) then
      Ada.Text_IO.Put_Line ("Use fast.");
   else
      Ada.Text_IO.Put_Line ("Use portable.");
      declare
         use Ada.Command_Line;
      begin
         Set_Exit_Status (Failure);
      end;
   end if;
end Uconv;
