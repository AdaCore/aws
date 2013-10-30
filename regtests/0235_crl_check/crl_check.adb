------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                        Copyright (C) 2013, AdaCore                       --
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

with Ada.Command_Line;
with Ada.Exceptions;
with Ada.Text_IO;

with AWS.Client;
with AWS.Config.Set;
with AWS.Messages;
with AWS.MIME;
with AWS.Net.SSL.Certificate;
with AWS.Response;
with AWS.Server.Status;
with AWS.Status;

procedure CRL_Check is

   use Ada;
   use Ada.Text_IO;
   use AWS;

   function CB (Request : Status.Data) return Response.Data;

   HTTP : Server.HTTP;
   R    : Response.Data;

   --------
   -- CB --
   --------

   function CB (Request : Status.Data) return Response.Data is
      use Net.SSL.Certificate;
      Sock : constant Net.Socket_Access := AWS.Status.Socket (Request);
      Cert : constant Net.SSL.Certificate.Object :=
               Net.SSL.Certificate.Get (Net.SSL.Socket_Type (Sock.all));
   begin
      Text_IO.Put_Line ("Cert in callback: " & Subject (Cert));
      return Response.Build (MIME.Text_Plain, "authorization OK!");
   end CB;

   ---------------
   -- Test_Cert --
   ---------------

   procedure Test_Cert (Cert : String) is
      Connect : Client.HTTP_Connection;
   begin
      Put_Line ("Trying " & Cert);
      Client.Create
        (Connection  => Connect,
         Host        => Server.Status.Local_URL (HTTP),
         Timeouts    => Client.Timeouts
           (Connect => 5.0, Send => 5.0, Receive => 5.0),
         Certificate => Cert);

      Client.Get (Connect, R, "/");
      Put_Line ("-> " & Messages.Image (Response.Status_Code (R)));
      Client.Close (Connect);
   end Test_Cert;

   -----------------
   -- Verify_Cert --
   -----------------

   function Verify_Cert (Cert : Net.SSL.Certificate.Object) return Boolean is
      use AWS.Net.SSL.Certificate;
      SSL_Verified : constant Boolean := Net.SSL.Certificate.Verified (Cert);
   begin
      Text_IO.Put_Line
        ("Client certificate: " & Subject (Cert)
         & ", verified: " & SSL_Verified'Img);
      return SSL_Verified;
   end Verify_Cert;

   CNF : Config.Object := Config.Get_Current;
   SSL : Net.SSL.config;

begin
   Config.Set.Server_Name    (CNF, "Test SSL client certificate CRL check");
   Config.Set.Server_Host    (CNF, "localhost");
   Config.Set.Server_Port    (CNF, 0);
   Config.Set.Max_Connection (CNF, 3);
   Config.Set.Security       (CNF, True);

   --  First configure SSL layer, this is needed to be able to have a
   --  callback to verify client's certificate.

   Net.SSL.Initialize
     (SSL,
      Certificate_Filename => "cert.pem",
      Exchange_Certificate => Config.Exchange_Certificate (Cnf),
      Certificate_Required => Config.Certificate_Required (Cnf),
      Trusted_CA_Filename  => Config.Trusted_CA (Cnf),
      CRL_Filename         => Command_Line.Argument (1));

   Net.SSL.Certificate.Set_Verify_Callback
     (SSL, Verify_Cert'Unrestricted_Access);
   Server.Set_SSL_Config (HTTP, SSL);

   Server.Start (HTTP, CB'Unrestricted_Access, CNF);

   --  Put_Line ("Server URL: " & Server.Status.Local_URL (HTTP));

   Test_Cert ("aws-client1.pem");
   Test_Cert ("aws-client2.pem");
   Server.Shutdown (HTTP);

exception
   when E : others =>
      Put_Line ("Main Error " & Exceptions.Exception_Information (E));
      Server.Shutdown (HTTP);
end CRL_Check;
