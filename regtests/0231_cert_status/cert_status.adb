------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                       Copyright (C) 2013, AdaCore                        --
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

with Ada.Calendar.Formatting;
with Ada.Calendar.Time_Zones;
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

procedure Cert_Status is

   use Ada;
   use Ada.Text_IO;
   use AWS;

   function CB (Request : Status.Data) return Response.Data;

   procedure Display_Certificate (Socket : Net.SSL.Socket_Type);

   procedure Display_Certificate (Cert : Net.SSL.Certificate.Object);

   function Image (DT : Calendar.Time) return String;

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
         New_Line;

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
         Put_Line
           ("Subject    : " & Net.SSL.Certificate.Subject (Cert));
         Put_Line
           ("Issuer     : " & Net.SSL.Certificate.Issuer (Cert));
         Put_Line
           ("Activation : "
            & Image (Net.SSL.Certificate.Activation_Time (Cert)));
         Put_Line
           ("Expiration : "
            & Image (Net.SSL.Certificate.Expiration_Time (Cert)));
         Put_Line
           ("Verified   : "
            & Boolean'Image (Net.SSL.Certificate.Verified (Cert)));
      end if;
   end Display_Certificate;

   procedure Display_Certificate (Socket : Net.SSL.Socket_Type) is
      Cert : constant Net.SSL.Certificate.Object :=
               Net.SSL.Certificate.Get (Socket);
   begin
      Display_Certificate (Cert);
      New_Line;
   end Display_Certificate;

   -----------
   -- Image --
   -----------

   function Image (DT : Calendar.Time) return String is
   begin
      return Calendar.Formatting.Image
        (DT, Time_Zone => Calendar.Time_Zones.UTC_Time_Offset (DT));
   end Image;

   -------------
   -- Request --
   -------------

   procedure Request (URL : String) is
      O_URL : constant AWS.URL.Object := AWS.URL.Parse (URL);
      R     : Response.Data;
      C     : Client.HTTP_Connection;
      Cert  : Net.SSL.Certificate.Object;
   begin
      Client.Create (C, URL, Certificate => "aws-client-expired.pem");

      begin
         Cert := Client.Get_Certificate (C);
      exception
         when others =>
            Put_Line ("Client connection closed by peer.");
            return;
      end;

      New_Line;
      Put_Line ("Server certificate as received by the client:");
      Display_Certificate (Cert);
      New_Line;

      Client.Get (C, R, AWS.URL.Abs_Path (O_URL));

      Put_Line ("=> " & Response.Message_Body (R));
      New_Line;

      Client.Close (C);
   end Request;

   -----------------
   -- Verify_Cert --
   -----------------

   function Verify_Cert (Cert : Net.SSL.Certificate.Object) return Boolean is
      use type Calendar.Time;
   begin
      Text_IO.Put_Line ("Client certificate from verify routine:");
      Display_Certificate (Cert);

      Put_Line
        ("Status     : "
         & Long_Integer'Image (Net.SSL.Certificate.Status (Cert)));
      Put_Line
        ("Message    : " & Net.SSL.Certificate.Status_Message (Cert));
      New_Line;

      --  Return verified status from the SSL layer

      return Net.SSL.Certificate.Verified (Cert);
   end Verify_Cert;

   HTTP : Server.HTTP;
   Conf : Config.Object;
   SSL  : Net.SSL.Config;

begin
   Put_Line ("Start main, wait for server to start...");

   Config.Set.Server_Host (Conf, "localhost");
   Config.Set.Server_Port (Conf, 0);
   Config.Set.Max_Connection (Conf, 5);
   Config.Set.Security (Conf, True);

   --  SSL config, no trusted CA

   Net.SSL.Initialize
     (SSL,
      Certificate_Filename => "aws-server.crt",
      Key_Filename         => "aws-server.key",
      Exchange_Certificate => True,
      Certificate_Required => True,
      Trusted_CA_Filename  => "CA-clt.crt");

   Net.SSL.Certificate.Set_Verify_Callback
     (SSL, Verify_Cert'Unrestricted_Access);

   Server.Set_SSL_Config (HTTP, SSL);

   --  Run

   Server.Start (HTTP, CB'Unrestricted_Access, Conf);

   Put_Line ("Server started");
   New_Line;

   Request (AWS.Server.Status.Local_URL (HTTP) & "/simple");

   Server.Shutdown (HTTP);

exception
   when E : others =>
      Put_Line ("Main Error " & Exceptions.Exception_Information (E));
      Server.Shutdown (HTTP);
end Cert_Status;
