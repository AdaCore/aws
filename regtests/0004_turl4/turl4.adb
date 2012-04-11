------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                    Copyright (C) 2002-2012, AdaCore                      --
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

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO;    use Ada.Text_IO;
with AWS.URL;        use AWS.URL;

procedure Turl4 is

   procedure Test (URL : String; Ok : Boolean) is
      O : Object;
   begin
      O := Parse (URL);

      if Ok then
         Put_Line ("OK,  no exception raised : " & URL);
      else
         Put_Line ("NOK, no exception raised : " & URL);
      end if;

   exception
      when E : URL_Error =>
         if Ok then
            Put_Line ("NOk, URL_Exception for : " & URL);
         else
            Put_Line ("Ok,  URL_Exception for : " & URL);
         end if;
      when E : others =>
         Put_Line ("Error, wrong exception raised : " & URL);
         Put_Line (Ada.Exceptions.Exception_Information (E));
   end Test;

begin
   Test ("javascript:retour()", True);
   Test ("javascript:retour('kkk')", True);
   Test ("kkk:retour('uuu')", True);
   Test ("()", True);
   Test ("'", True);
end Turl4;
