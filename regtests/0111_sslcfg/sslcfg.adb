------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2004-2012, AdaCore                     --
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

--  Test for certificate or keyfile absence and wrong format

with Ada.Exceptions;
with Ada.Text_IO;
with AWS.Net.SSL;

procedure SSLCfg is

   procedure Test (Cert : String; Key : String := "");

   ----------
   -- Test --
   ----------

   procedure Test (Cert : String; Key : String := "") is
      use AWS.Net.SSL;
      Conf : Config;
   begin
      Initialize
        (Config               => Conf,
         Certificate_Filename => Cert,
         Key_Filename         => Key);
      Ada.Text_IO.Put_Line ("Success.");
   exception
      when E : others =>
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Message (E));
   end Test;

begin
   Test ("absent-file");
   Test ("cert.pem", "absent-file");
   Test ("sslcfg.adb");
   Test ("cert.pem", "sslcfg.adb");
   Test ("cert.pem");
   Test ("cert.pem", "cert.pem");
end SSLCfg;
