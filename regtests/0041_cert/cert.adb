------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2003-2012, AdaCore                     --
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

--  This must be the exact same test than tgetparam. The only difference is
--  that it uses HTTPS protocol. We test that output is the same as the non
--  secure version.

with Ada.Text_IO;
with Ada.Exceptions;

with AWS.Server.Status;
with AWS.Client;
with AWS.Config.Set;
with AWS.Status;
with AWS.MIME;
with AWS.Response;
with AWS.Parameters;
with AWS.Messages;
with AWS.Net.SSL.Certificate;
with AWS.URL;

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
         Put_Line ("Subject : " & Net.SSL.Certificate.Subject (Cert));
         Put_Line ("Issuer  : " & Net.SSL.Certificate.Issuer (Cert));
      end if;
   end Display_Certificate;

   procedure Display_Certificate (Socket : Net.SSL.Socket_Type) is
      Cert : constant Net.SSL.Certificate.Object
        := Net.SSL.Certificate.Get (Socket);
   begin
      Display_Certificate (Cert);
   end Display_Certificate;

   -------------
   -- Request --
   -------------

   procedure Request (URL : String) is
      O_URL : constant AWS.URL.Object := AWS.URL.Parse (URL);
      R     : Response.Data;
      C     : Client.HTTP_Connection;
      Cert  : Net.SSL.Certificate.Object;
   begin
      Client.Create (C, URL, Certificate => "aws-client.pem");

      Cert := Client.Get_Certificate (C);

      New_Line;
      Put_Line ("Server certificate as received by the client:");
      Display_Certificate (Cert);

      Client.Get (C, R, AWS.URL.Abs_Path (O_URL));

      Put_Line ("=> " & Response.Message_Body (R));
      New_Line;

      Client.Close (C);
   end Request;

   Conf : Config.Object;

begin
   Put_Line ("Start main, wait for server to start...");

   Config.Set.Server_Host (Conf, "localhost");
   Config.Set.Server_Port (Conf, 0);
   Config.Set.Max_Connection (Conf, 5);
   Config.Set.Security (Conf, True);
   Config.Set.Exchange_Certificate (Conf, True);
   Config.Set.Trusted_CA (Conf, "private-ca.crt");
   Config.Set.Certificate (Conf, "aws-server.crt");
   Config.Set.Key (Conf, "aws-server.key");

   Server.Start (HTTP, CB'Unrestricted_Access, Conf);

   Put_Line ("Server started");
   New_Line;

   Request (AWS.Server.Status.Local_URL (HTTP) & "/simple");

   Server.Shutdown (HTTP);

exception
   when E : others =>
      Put_Line ("Main Error " & Exceptions.Exception_Information (E));
      Server.Shutdown (HTTP);
end Cert;
