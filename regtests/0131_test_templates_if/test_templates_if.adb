------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2004-2008, AdaCore                     --
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

--  Program that tests the <<if ... elsif ... else ... end if>>
--  templates

with Ada.Text_IO; use Ada.Text_IO;

with AWS.Templates; use AWS.Templates;

procedure Test_Templates_If is

   package TP renames AWS.Templates;

   procedure Run is
      Names_To_Display, References_Names, Depths : TP.Vector_Tag;
      Translations : TP.Translate_Table (1 .. 3);
      First_Node : Natural := 0;
   begin
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
