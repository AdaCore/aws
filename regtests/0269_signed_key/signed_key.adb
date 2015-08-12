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

with Ada.Text_IO;

with AWS.Client;
with AWS.Config;
with AWS.Net.SSL.Certificate;
with AWS.Response;
with AWS.Server.Status;

with Signed_Key_CB;

procedure Signed_Key is

   use Ada;
   use AWS;

   WS  : Server.HTTP;
   SSL : Net.SSL.Config;
   R   : Response.Data;

begin
   Net.SSL.Certificate.Set_Password_Callback
     (Signed_Key_CB.Set_Password'Access);

   Net.SSL.Initialize
     (SSL,
      Certificate_Filename => "aws-server.crt",
      Key_Filename         => "aws-server.key");

   Server.Set_SSL_Config (WS, SSL);

   Server.Start
     (WS, "HTTPS",
      Port           => 0,
      Max_Connection => 5,
      Security       => True,
      Callback       => Signed_Key_CB.HW_CB'Access);

   R := Client.Get (Server.Status.Local_URL (WS));
   Text_IO.Put_Line ("R : " & Response.Message_Body (R));

   Server.Shutdown (WS);
end Signed_Key;
