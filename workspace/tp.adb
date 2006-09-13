with Ada.Exceptions;
with Ada.Text_IO;
with Templates_Parser;

procedure TP is
   Cached : constant Boolean := False;

   task type Secondary is
      entry Start;
   end Secondary;

   procedure Parse (P1 : Positive);

   -----------
   -- Parse --
   -----------

   procedure Parse (P1 : Positive) is
      use Templates_Parser;

      Translations : Translate_Table (1 .. 1);
      T : Tag;
   begin
      T := +P1;
      Translations (1) := Assoc ("P1", T);

      declare
         Result : constant String
           := Parse ("file.tmplt", Translations, Cached);
      begin
         if P1 rem 1000 = 0 then
            Ada.Text_IO.Put_Line (Result);
         end if;
      end;
   end Parse;

   ---------------
   -- Secondary --
   ---------------

   task body Secondary is
   begin
      accept Start;

      for K in Positive'Range loop
         Parse (K);
      end loop;

   exception
      when E : others =>
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
   end Secondary;

   TS : array (1 .. 2) of Secondary;

begin
   Parse (1);

   for J in TS'Range loop
      TS (J).Start;
   end loop;
end TP;
