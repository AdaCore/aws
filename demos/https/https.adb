------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                       Copyright (C) 2015, AdaCore                        --
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

--  Demos using TLS 1.2 only

with Ada.Text_IO;

with AWS.Config;
with AWS.Net.SSL.Certificate;
with AWS.Server;

with HTTPS_CB;

procedure HTTPS is

   use Ada;
   use AWS;

   WS  : Server.HTTP;
   SSL : Net.SSL.Config;

begin
   Text_IO.Put_Line ("Call me on port 4433, press Q to exit");
   Text_IO.New_Line;

   --  Allows only TLS 1.2

   Net.SSL.Certificate.Set_Password_Callback
     (HTTPS_CB.Set_Password'Access);

   Net.SSL.Initialize
     (SSL,
      Certificate_Filename => "aws-server.crt",
      Key_Filename         => "aws-server.key",
      Security_Mode        => Net.SSL.TLSv1_2);

   Server.Set_SSL_Config (WS, SSL);

   Server.Start
     (WS, "HTTPS",
      Max_Connection => 5,
      Security       => True,
      Port           => 4433,
      Callback       => HTTPS_CB.HW_CB'Access);

   Server.Wait (Server.Q_Key_Pressed);

   Server.Shutdown (WS);
end HTTPS;
