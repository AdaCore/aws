
--  Program that tests the <<if ... elsif ... else ... end if>>
--  templates
--
--  Thomas Vergnaud <vergnaud@act-europe.fr>

with Ada.Text_IO; use Ada.Text_IO;

with AWS.Templates; use AWS.Templates;

procedure Test_Templates_If is

   package TP renames AWS.Templates;

   procedure Run is
      Names_To_Display, References_Names, Depths : TP.Vector_Tag;
      Translations : TP.Translate_Table (1 .. 3);
      First_Node : Natural := 0;
   begin
--        TP.Clear (Names_To_Display);
--        TP.Clear (References_Names);
--        TP.Clear (Depths);

      Names_To_Display := Names_To_Display & "titi";
      Names_To_Display := Names_To_Display & ".";
      Names_To_Display := Names_To_Display & "tata";
      Names_To_Display := Names_To_Display & "..";
      Names_To_Display := Names_To_Display & "toto";
      Names_To_Display := Names_To_Display & "toutou";

      References_Names := References_Names & "titi";
      References_Names := References_Names & ".";
      References_Names := References_Names & "tata";
      References_Names := References_Names & "..";
      References_Names := References_Names & "toto";
      References_Names := References_Names & "toutou";

      Depths := Depths & "=";
      Depths := Depths & "+1";
      Depths := Depths & "=";
      Depths := Depths & "-1";
      Depths := Depths & "=";
      Depths := Depths & "=";

      Translations :=
        (TP.Assoc ("ITEM", Names_To_Display),
         TP.Assoc ("FILE_NAME", References_Names),
         TP.Assoc ("DEPTH", Depths)
        );

      Put_Line (TP.Parse ("ftp.thtml", Translations));
   end Run;

begin
   Run;
end Test_Templates_If;
