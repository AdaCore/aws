------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2003-2024, AdaCore                     --
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

--  Test for server certificates with Server Name Identification (SNI).

with Ada.Exceptions;
with Ada.Text_IO;

with AWS.Client;
with AWS.Config.Set;
with AWS.Messages;
with AWS.MIME;
with AWS.Net.SSL.Certificate;
with AWS.Parameters;
with AWS.Response;
with AWS.Server.Status;
with AWS.Status;
with AWS.URL;
with AWS.Utils;

procedure Cert is

   use Ada;
   use Ada.Text_IO;
   use AWS;

   function CB (Request : Status.Data) return Response.Data;

   procedure Display_Certificate (Socket : Net.SSL.Socket_Type);

   procedure Display_Certificate (Cert : Net.SSL.Certificate.Object);

   HTTP : Server.HTTP;

   --------
   -- CB --
   --------

   function CB (Request : Status.Data) return Response.Data is
      URI  : constant String                := Status.URI (Request);
      Sock : constant Net.Socket_Type'Class := Status.Socket (Request);
   begin
      if URI = "/simple" then
         New_Line;
         Put_Line ("Client certificate as received by the server:");
         Display_Certificate (Net.SSL.Socket_Type (Sock));

         return Response.Build (MIME.Text_HTML, "simple ok");
      else
         Put_Line ("Unknown URI " & URI);
         return Response.Build
           (MIME.Text_HTML, URI & " not found", Messages.S404);
      end if;
   end CB;

   -------------------------
   -- Display_Certificate --
   -------------------------

   procedure Display_Certificate (Cert : Net.SSL.Certificate.Object) is
      use type Net.SSL.Certificate.Object;
   begin
      if Cert = Net.SSL.Certificate.Undefined then
         Put_Line ("No certificate.");
      else
         Put_Line ("Name    : " & Net.SSL.Certificate.Common_Name (Cert));
         Put_Line ("Subject : " & Net.SSL.Certificate.Subject (Cert));
         Put_Line ("Issuer  : " & Net.SSL.Certificate.Issuer (Cert));
         Put_Line ("Serial  : " & Net.SSL.Certificate.Serial_Number (Cert));
      end if;
   end Display_Certificate;

   procedure Display_Certificate (Socket : Net.SSL.Socket_Type) is
      Cert : constant Net.SSL.Certificate.Object :=
               Net.SSL.Certificate.Get (Socket);
   begin
      Display_Certificate (Cert);
   end Display_Certificate;

   -------------
   -- Request --
   -------------

   procedure Request (URL : String) is
      O_URL  : constant AWS.URL.Object := AWS.URL.Parse (URL);
      R      : Response.Data;
      C      : Client.HTTP_Connection;
      Cert   : Net.SSL.Certificate.Object;
      Config : Net.SSL.Config;
   begin
      Net.SSL.Initialize
        (Config,
         Security_Mode        => Net.SSL.TLS_Client,
         Client_Certificate   => "aws-client.pem",
         Exchange_Certificate => True,
         Trusted_CA_Filename  => "",
         Check_Certificate    => False);

      Client.Create (C, URL, SSL_Config => Config);

      Cert := Client.Get_Certificate (C);

      Put_Line ("Server certificate as received by the client:");
      Display_Certificate (Cert);

      Client.Get (C, R, AWS.URL.Abs_Path (O_URL));

      Put_Line ("=> " & Response.Message_Body (R));

      Client.Close (C);
   end Request;

   Conf : Config.Object;
   SrvCnf : Net.SSL.Config;

begin
   Put_Line ("Start main, wait for server to start...");

   Net.SSL.Initialize
     (SrvCnf,
      Security_Mode        => Net.SSL.TLS_Server,
      Check_Certificate    => False,
      Server_Certificate   => "aws-server.crt",
      Server_Key           => "aws-server.key",
      Exchange_Certificate => True,
      Trusted_CA_Filename  => "private-ca.crt");

   Config.Set.Server_Port (Conf, 0);
   Config.Set.Max_Connection (Conf, 5);
   Config.Set.Security (Conf, True);
   Config.Set.IPv6_Only (Conf, Net.IPv6_Available);
   Config.Set.Protocol_Family
     (Conf, (if Net.IPv6_Available then "Family_Inet6" else "Family_Inet"));

   Server.Set_SSL_Config (HTTP, SrvCnf);

   Server.Start (HTTP, CB'Unrestricted_Access, Conf);

   if Net.IPv6_Available then
      --  Additional Bind and Listen to localhost directly, because when IPv6
      --  available localhost on server side and on client side could be in
      --  different families.

      Server.Add_Listening
        (HTTP, "", Server.Status.Port (HTTP),
         (if Server.Status.Is_IPv6 (HTTP)
          then Net.Family_Inet else Net.Family_Inet6));
   end if;

   --  Connect to phisical IP, Net.Host_Name and to "localhost" lead to
   --  different certificates.

   Net.SSL.Add_Host_Certificate
      (Server.SSL_Config (HTTP).all,
       Net.Host_Name, "aws-server-2.crt", "aws-server-2.key");

   Net.SSL.Add_Host_Certificate
      (Server.SSL_Config (HTTP).all,
       "www.ada-web.org", "aws-server-3.crt", "aws-server-3.key");
   Net.Set_Host_Alias ("www.ada-web.org", Net.Localhost (Net.IPv6_Available));

   Net.SSL.Add_Host_Certificate
     (Server.SSL_Config (HTTP).all,
      "*.ada-web.org", "aws-server-4.crt", "aws-server-4.key");
   Net.Set_Host_Alias ("site.ada-web.org", Net.Localhost (Net.IPv6_Available));

   Net.SSL.Add_Host_Certificate
      (Server.SSL_Config (HTTP).all,
       "localhost", "chain-srv.crt", "test-srv.key");
   --  chain-srv.crt and test-srv.key taken from ../0246_ctr_chain

   Put_Line ("Server started");

   Request (AWS.Server.Status.Local_URL (HTTP) & "/simple");

   Request ("https://" & Net.Host_Name & ':'
            & Utils.Image (Server.Status.Port (HTTP)) & "/simple");

   Request ("https://localhost:"
            & Utils.Image (Server.Status.Port (HTTP)) & "/simple");

   Request ("https://www.ada-web.org:"
            & Utils.Image (Server.Status.Port (HTTP)) & "/simple");

   Request ("https://site.ada-web.org:"
            & Utils.Image (Server.Status.Port (HTTP)) & "/simple");

   Server.Shutdown (HTTP);

   Display_Certificate (Net.SSL.Certificate.Load ("go-daddy.crt"));

exception
   when E : others =>
      Put_Line ("Main Error " & Exceptions.Exception_Information (E));
      Server.Shutdown (HTTP);
end Cert;
