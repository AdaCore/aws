------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                            Copyright (C) 2002                            --
--                               ACT-Europe                                 --
--                                                                          --
--  Authors: Dmitriy Anisimkov - Pascal Obry                                --
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

--  $Id$

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO;    use Ada.Text_IO;
with AWS.URL;        use AWS.URL;

procedure Turl4 is

   procedure Test (URL : in String; Ok : Boolean) is
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
   Test ("javascript:retour()", False);
   Test ("javascript:retour('kkk')", False);
   Test ("kkk:retour('uuu')", False);
   Test ("()", True);
   Test ("'", True);
end Turl4;
