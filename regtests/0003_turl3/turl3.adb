------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2000-2013, AdaCore                     --
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

procedure Turl3 is

   procedure Test (URL : String) is
      O : Object;
   begin
      O := Parse (URL);

      Put_Line (URL);
      Put_Line (AWS.URL.URL (O));
      Put_Line ("   Protocol   : " & Protocol_Name (O));
      Put_Line ("   User       : " & User (O));
      Put_Line ("   Password   : " & Password (O));
      Put_Line ("   Server     : " & Host (O));
      Put_Line ("   Port       : " & Port (O));
      Put_Line ("   Path       : " & Path (O));
      Put_Line ("   File       : " & File (O));
      Put_Line ("   Pathname   : " & Pathname (O));
      Put_Line ("   Parameters : " & Parameters (O));
      Put_Line ("   Query      : " & Query (O));
      Put_Line ("   URI        : " & Pathname_And_Parameters (O));

      Normalize (O);

      Put_Line ("   *");
      Put_Line ("   Protocol   : " & Protocol_Name (O));
      Put_Line ("   User       : " & User (O));
      Put_Line ("   Password   : " & Password (O));
      Put_Line ("   Server     : " & Host (O));
      Put_Line ("   Port       : " & Port (O));
      Put_Line ("   Path       : " & Path (O));
      Put_Line ("   File       : " & File (O));
      Put_Line ("   Pathname   : " & Pathname (O));
      Put_Line ("   Parameters : " & Parameters (O));
      Put_Line ("   Query      : " & Query (O));
      Put_Line ("   URI        : " & Pathname_And_Parameters (O));

   exception
      when E : URL_Error =>
         Put_Line ("Error on " & URL & " - " & Exception_Message (E));
   end Test;

begin
   Test ("http://me:pwd@www.myserver.com:89/app/request?p1=9&p2=toto");
   Test ("http://me:pwd@www.myserver.com/app/request?p1=9&p2=toto");
   Test ("http://me:pwd@[1234:5678::9ABCD]/app/request?p1=9&p2=toto");
   Test ("http://me:pwd@www.myserver.com");
   Test ("http://me:pwd@www.myserver.com:89");
   Test ("http://me:@www.myserver.com:89");
   Test ("http://me:@www.myserver.com");
   Test ("http://:@www.myserver.com:89/app/request?p1=9&p2=toto");
   Test ("http://:@www.myserver.com/app/request?p1=9&p2=toto");
   Test ("http://:@www.myserver.com/app/request");
   Test ("http://:@www.myserver.com");
   Test ("https://me:pwd@www.myserver.com:89/app/request?p1=9&p2=toto");
   Test ("http://me:pwd@www.myserver.com:89/app/request?wsdl");
end Turl3;
