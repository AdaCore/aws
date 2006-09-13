------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                            Copyright (C) 2004                            --
--                                ACT-Europe                                --
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

--  ~ MAIN [SSL]

--  Test for certificate or keyfile absence and wrong format.

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
