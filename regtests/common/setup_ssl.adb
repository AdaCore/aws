------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                       Copyright (C) 2024, AdaCore                        --
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

with AWS.Net.SSL.Certificate;

with CERT_Pwd;

package body Setup_SSL is

   use AWS;

   --------------
   --  Default --
   --------------

   procedure Default is
   begin
      Net.SSL.Initialize_Default_Config
        (Security_Mode        => Net.SSL.TLS,
         Client_Certificate   => "cert.pem",
         Trusted_CA_Filename  => "",
         Server_Key           => "",
         Server_Certificate   => "cert.pem",
         Exchange_Certificate => False,
         Check_Certificate    => False,
         Check_Host           => False);

      CERT_Pwd.Message := False;

      Net.SSL.Certificate.Set_Password_Callback
        (CERT_Pwd.Set_Password'Access);
   end Default;

   ----------
   -- Full --
   ----------

   procedure Full
     (WS   : in out AWS.Server.HTTP;
      Mess : Boolean := True)
   is
      SSL : Net.SSL.Config;
   begin
      Default;

      CERT_Pwd.Message := Mess;

      Net.SSL.Initialize
        (SSL,
         Security_Mode        => Net.SSL.TLS_Server,
         Server_Certificate   => "aws-server.crt",
         Server_Key           => "aws-server.key",
         Exchange_Certificate => True,
         Check_Certificate    => False);

      Server.Set_SSL_Config (WS, SSL);
   end Full;

end Setup_SSL;
