with Ada.Text_IO;
with Templates_Parser;
with Ada.Exceptions;
with Ada.Command_Line;

procedure TP is
   use Templates_Parser;

   T1 : Tag;
   T2 : Tag;

   procedure Show_Tag (T : Tag);
   task type Secondary;

   procedure Show_Tag (T : Tag) is
      use Ada.Text_IO;
   begin
      for J in 1 .. Size (T) loop
         Put (Item (T, J) & ' ');
      end loop;

      New_Line;
   end Show_Tag;

   ---------------
   -- Secondary --
   ---------------

   task body Secondary is
      use Ada.Command_Line;
      use Ada.Text_IO;
      Translations : Translate_Table (1 .. Argument_Count / 2);
      T : Tag;

      Len2 : Natural;
      Len1 : Natural := 0;
   begin
      for J in Translations'Range loop
         T := +Argument (J * 2);
         Translations (J) := Assoc (Argument (J * 2 - 1), T);
      end loop;

      loop
         declare
            Result : constant String := Parse ("file.tmplt", Translations, False);
         begin
            Len2 := Result'Length;

            --  if Len2 /= Len1 then
               Put_Line (Result);
            --  end if;

            Len1 := Len2;
         end;
      end loop;

   exception
      when E : others =>
         Put_Line (Ada.Exceptions.Exception_Information (E));
   end Secondary;

   TS : array (1 .. 2) of Secondary;

begin
   T1 := +"One";
   T2 := T1 & "Two";
   Show_Tag (T1);
   Show_Tag (T2);
end TP;
