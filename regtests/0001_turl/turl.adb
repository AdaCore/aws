------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2000-2016, AdaCore                     --
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

procedure Turl is

   procedure Test (URL : String) is
      O : Object;

      procedure Print_Not_Empty (Title, Value : String) is
      begin
         if Value /= "" then
            Put ("   " & Title);
            Set_Col (14);
            Put_Line (": " & Value);
         end if;
      end Print_Not_Empty;

   begin
      O := Parse (URL);

      Put_Line (URL);
      Put_Line (AWS.URL.URL (O));
      Put_Line ("   Server    : " & Host (O));
      Put_Line ("   Port      : " & Port (O));
      Put_Line ("   URI       : " & Pathname_And_Parameters (O));

      Normalize (O);

      Put_Line ("   *");
      Put_Line ("   Server    : " & Host (O));
      Put_Line ("   Port      : " & Port (O));
      Put_Line ("   URI       : " & Pathname_And_Parameters (O));

      Print_Not_Empty ("Fragment", Fragment (O));

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
   Test ("http://www.myserver.com:89/app/request?p1=9&p2=toto");
   Test ("http://www.myserver.com:89/app/request/?p1=9&p2=toto");
   Test ("http://www.myserver.com:89/app/request/?p1=9&url=http://titi");
   Test ("http://www.myserver.com:89/app/..\..\toto&url=http://titi");
   Test ("http://www.myserver.com:89/app/.\..\toto&url=http://titi");
   Test ("http://www.myserver.com:89/app//../\..\\..\toto?url=http://titi");
   Test ("http://192.168.58.44:8083/whatever/me/dev[6]");
   Test ("http://www.myserver.com:12/request?p1=9#fragment");
   Test ("http://www.itserver.com:34/query#fragment?question");
   Test ("https://onlineadspy.com/suite/social/?&is_active=1059");
end Turl;
