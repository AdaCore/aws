------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2000-2001                          --
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

procedure Turl is

   procedure Test (URL : in String) is
      O : Object;
   begin
      O := Parse (URL);

      Put_Line (URL);
      Put_Line (AWS.URL.URL (O));
      Put_Line ("   Server    : " & Host (O));
      Put_Line ("   Port      : " & Port (O));
      Put_Line ("   URI       : " & Pathname (O));

      Normalize (O);

      Put_Line ("   *");
      Put_Line ("   Server    : " & Host (O));
      Put_Line ("   Port      : " & Port (O));
      Put_Line ("   URI       : " & Pathname (O));

   exception
      when E : URL_Error =>
         Put_Line ("Error on " & URL & " - " & Exception_Message (E));
   end Test;

begin
   Test ("http://toto.titi.tata/my_url/xyz");
   Test ("http://a.b.c:1234/my_url/../xyz");
   Test ("http://a/tata/my_url/xyz/../../b/c/d/../../z");
   Test ("/azerty/me/too.html");
   Test ("demo.html");
   Test ("../demo.html");
   Test ("http://myserver:8088/../demo.html");
   Test ("http://myserver.com");
   Test ("http://myserver.com/./un/././deux.html");
   Test ("https://myserver.com");
   Test ("kye/demo.html");
   Test ("http://www.myserver.com:12/request?p1=9");
   Test ("http://sinetics.der.edf.fr/./afficher_liste.php");
   Test ("");
end Turl;
