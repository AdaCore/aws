------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                       Copyright (C) 2014, AdaCore                        --
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

--  Private key signature test.

pragma Ada_2012;

with Ada.Text_IO;
with AWS.Net.SSL;
with AWS.Translator;

procedure SDig is
   use Ada.Text_IO;
   use AWS.Net.SSL;
   use AWS.Translator;
   Key : Private_Key := Load ("sdig.key");
begin
   Set_Debug (11);
   for J in Hash_Method loop
      Put_Line (J'Img & ' ' & Base64_Encode (Signature (J'Img, Key, J)));
   end loop;
end SDig;