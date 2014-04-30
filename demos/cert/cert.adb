------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2012-2014, AdaCore                     --
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

--  Demos using certificates on both server and client and a
--  Certificate Authority.

with Ada.Text_IO;
with Ada.Command_Line;

with AWS.Config;
with AWS.Default;
with AWS.Net.Log;
with AWS.Net.SSL;
with AWS.Net.SSL.Certificate;
with AWS.Server;

with Cert_CB;

with GNAT.Traceback.Symbolic;

procedure Cert is

   use Ada;
   use AWS;

   WS  : Server.HTTP;
   Cnf : Config.Object := Config.Get_Current;
   SSL : Net.SSL.Config;

   -----------
   -- Error --
   -----------

   procedure Error (Socket : Net.Socket_Type'Class; Message : String) is
      use GNAT.Traceback;
      Trace : Tracebacks_Array (1 .. 64);
      Last  : Natural;
   begin
      Call_Chain (Trace, Last);

      Text_IO.Put_Line
        ("# Network error: "
         & Message & Symbolic.Symbolic_Traceback (Trace (1 .. Last)));
   end Error;

begin
   if Ada.Command_Line.Argument_Count > 0 then
      Net.Log.Start (Error => Error'Unrestricted_Access, Write => null);
   end if;

   Text_IO.Put_Line ("Call me on port 4433, press Q to exit");
   Text_IO.New_Line;

   Text_IO.Put_Line
     ("To see the client certificate it is needed to install one in");
   Text_IO.Put_Line
     ("the Web Browser. The provided aws-client[12].p12 (in this directory)");
   Text_IO.Put_Line
     ("can be installed into Firefox with:");
   Text_IO.Put_Line
     ("  preferences -> Advanced -> View Certificates -> Import");
   Text_IO.Put_Line
     ("The certificate password is: demo");
   Text_IO.New_Line;

   --  First configure SSL layer, this is needed to be able to have a
   --  callback to verify client's certificate.

   Net.SSL.Initialize
     (SSL,
      Certificate_Filename => "aws-server.crt",
      Key_Filename         => "aws-server.key",
      --  The 3 following configs are from aws.ini
      Exchange_Certificate => Config.Exchange_Certificate (Cnf),
      Certificate_Required => Config.Certificate_Required (Cnf),
      Trusted_CA_Filename  => Config.Trusted_CA (Cnf),
      CRL_Filename         => Config.CRL_File (Cnf));

   Net.SSL.Certificate.Set_Verify_Callback (SSL, Cert_CB.Verify_Cert'Access);

   Server.Set_SSL_Config (WS, SSL);

   Server.Start
     (WS, "Hello World",
      Max_Connection => 5,
      Security       => True,
      Port           => 4433,
      Callback       => Cert_CB.HW_CB'Access);

   Server.Wait (Server.Q_Key_Pressed);

   Server.Shutdown (WS);
end Cert;
