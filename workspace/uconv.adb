--  $Id$
--  Draft test availability of the Unchecked_Conversion between
--  Stream_Element_Array and String

with Ada.Unchecked_Conversion;
with Ada.Streams;
use Ada.Streams;
with Ada.Text_IO;

procedure Uconv is
   subtype Fixed_Array is Stream_Element_Array (1 .. 24);
   subtype Fixed_String is String
      (Integer (Fixed_Array'First) .. Integer (Fixed_Array'Last));
   function To_Stream_Elements is
     new Ada.Unchecked_Conversion (Fixed_String, Fixed_Array);
   function To_String is
     new Ada.Unchecked_Conversion (Fixed_Array, Fixed_String);
   Text    : String               := "выпендриваюсь.";
   Elements : Stream_Element_Array := To_Stream_Elements
      (Text & (Text'Last + 1 .. Fixed_String'Last => '2'));
begin
   Ada.Text_IO.Put      (Fixed_Array'Size'Img);
   Ada.Text_IO.Put_Line (Fixed_String'Size'Img);
   Ada.Text_IO.Put      (Fixed_Array'Alignment'Img);
   Ada.Text_IO.Put_Line (Fixed_String'Alignment'Img);

   Ada.Text_IO.Put_Line (To_String (Elements));
   for I in Elements'Range loop
      Ada.Text_IO.Put (Elements (I)'Img);
   end loop;
end Uconv;

   