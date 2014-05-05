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
with AWS.Net.Log;
with AWS.Net.SSL.Certificate;
with AWS.Parameters;
with AWS.Response;
with AWS.Server.Status;
with AWS.Status;
with AWS.Translator;
with AWS.URL;

with GNAT.Traceback.Symbolic;

procedure Client_Cert is

   use Ada;
   use Ada.Text_IO;
   use AWS;

   package NSC renames AWS.Net.SSL.Certificate;

   Client_Certificate_Name : constant String := "aws-client.pem";
   Client_Certificate      : constant NSC.Object :=
                               NSC.Load (Client_Certificate_Name);

   function CB (Request : Status.Data) return Response.Data;

   procedure Display_Certificate (Cert : NSC.Object);

   function Image (DT : Calendar.Time) return String;

   --------
   -- CB --
   --------

   function CB (Request : Status.Data) return Response.Data is
      use type NSC.Object;
      URI  : constant String                := Status.URI (Request);
      Sock : constant Net.Socket_Type'Class := Status.Socket (Request);
      Cert : constant NSC.Object := NSC.Get (Net.SSL.Socket_Type (Sock));
   begin
      if URI = "/simple" then
         New_Line;
         Put_Line ("Client certificate as received by the server:");

         if NSC.Subject (Cert) /= "" and then Cert /= Client_Certificate then
            Put_Line ("Wrong client certificate !");
         end if;

         Display_Certificate (Cert);

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

   procedure Display_Certificate (Cert : NSC.Object) is
      use type NSC.Object;
   begin
      if Cert = NSC.Undefined then
         Put_Line ("No certificate.");
      else
         Put_Line ("Name       : " & NSC.Common_Name (Cert));
         Put_Line ("Subject    : " & NSC.Subject (Cert));
         Put_Line ("Issuer     : " & NSC.Issuer (Cert));
         Put_Line ("Activation : " & Image (NSC.Activation_Time (Cert)));
         Put_Line ("Expiration : " & Image (NSC.Expiration_Time (Cert)));
         Put_Line ("Verified   : " & Boolean'Image (NSC.Verified (Cert)));
         --  Put_Line ("Status: " & NSC.Status_Message (Cert));
         New_Line;
      end if;
   end Display_Certificate;

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

   -----------
   -- Image --
   -----------

   function Image (DT : Ada.Calendar.Time) return String is
   begin
      return Calendar.Formatting.Image
        (DT, Time_Zone => Calendar.Time_Zones.UTC_Time_Offset (DT));
   end Image;

   -------------
   -- Request --
   -------------

   procedure Request
     (URL : String; CA : String := ""; Crt : String := Client_Certificate_Name)
   is
      O_URL : constant AWS.URL.Object := AWS.URL.Parse (URL);
      R     : Response.Data;
      C     : Client.HTTP_Connection;
      Cert  : NSC.Object;
      Cfg   : Net.SSL.Config;
   begin
      Net.SSL.Initialize
        (Cfg,
         Certificate_Filename => Crt,
         Trusted_CA_Filename  => CA);

      Client.Create (C, URL, SSL_Config => Cfg);

      begin
         Cert := Client.Get_Certificate (C);
      exception
         when others =>
            Put_Line ("Client connection closed by peer.");
            return;
      end;

      if NSC."=" (Cert, Client_Certificate) then
         Put_Line ("Client certificate could not be on client side.");
      end if;

      New_Line;
      Put_Line ("Server certificate as received by the client:");
      Display_Certificate (Cert);

      Client.Get (C, R, AWS.URL.Abs_Path (O_URL));

      Put_Line ("=> " & Response.Message_Body (R));
      New_Line;

      Client.Close (C);
   end Request;

   -----------------
   -- Verify_Cert --
   -----------------

   function Verify_Cert (Cert : NSC.Object) return Boolean is
      use type Calendar.Time;
   begin
      Text_IO.Put_Line ("Client certificate from verify routine:");
      Display_Certificate (Cert);

      --  Return verified status from the SSL layer

      return NSC.Verified (Cert);
   end Verify_Cert;

   HTTP1, HTTP2, HTTP3 : Server.HTTP;
   Conf                : Config.Object;
   SSL1, SSL2, SSL3    : Net.SSL.Config;

begin
   Put_Line ("Start main, wait for server to start...");

   Config.Set.Server_Host (Conf, "localhost");
   Config.Set.Server_Port (Conf, 0);
   Config.Set.Max_Connection (Conf, 5);
   Config.Set.Security (Conf, True);

   --  SSL1 config, no trusted CA

   Net.SSL.Initialize
     (SSL1,
      Certificate_Filename => "aws-server.crt",
      Key_Filename         => "aws-server.key",
      Exchange_Certificate => True,
      Certificate_Required => True);

   NSC.Set_Verify_Callback (SSL1, Verify_Cert'Unrestricted_Access);

   Server.Set_SSL_Config (HTTP1, SSL1);

   --  SSL2 config, with a trusted CA

   Net.SSL.Initialize
     (SSL2,
      Certificate_Filename => "aws-server.crt",
      Key_Filename         => "aws-server.key",
      Exchange_Certificate => True,
      Certificate_Required => True,
      Trusted_CA_Filename  => "CA-clt.crt");

   NSC.Set_Verify_Callback (SSL2, Verify_Cert'Unrestricted_Access);

   Server.Set_SSL_Config (HTTP2, SSL2);

   --  Run 1

   Server.Start (HTTP1, CB'Unrestricted_Access, Conf);

   Put_Line ("Server 1 started");
   New_Line;

   Request (AWS.Server.Status.Local_URL (HTTP1) & "/simple");

   Net.Log.Start (Error => Error'Unrestricted_Access, Write => null);

   Server.Shutdown (HTTP1);

   --  Run 2

   Server.Start (HTTP2, CB'Unrestricted_Access, Conf);

   Put_Line ("Server 2 started");
   New_Line;

   Request (AWS.Server.Status.Local_URL (HTTP2) & "/simple", "CA-srv.crt");

   Server.Shutdown (HTTP2);

   Net.SSL.Initialize
     (SSL3,
      Certificate_Filename => "aws-server.crt",
      Key_Filename         => "aws-server.key",
      Exchange_Certificate => True,
      Certificate_Required => False);

   Server.Set_SSL_Config (HTTP3, SSL3);

   Server.Start (HTTP3, CB'Unrestricted_Access, Conf);

   Put_Line ("Server 3 started");
   New_Line;

   Request (AWS.Server.Status.Local_URL (HTTP3) & "/simple", "CA-srv.crt");
   Request (AWS.Server.Status.Local_URL (HTTP3) & "/simple", "CA-srv.crt", "");

   --  Looks like in the last request client see the server certificate as a
   --  non trusted, but has a correct truster authority, in both OpenSSL and
   --  GNUTLS. Need investigate.

   Server.Shutdown (HTTP3);

   Set_Line_Length (79);
   Put_Line (AWS.Translator.Base64_Encode (NSC.DER (Client_Certificate)));

exception
   when E : others =>
      Put_Line ("Main Error " & Exceptions.Exception_Information (E));
      Server.Shutdown (HTTP1);
      Server.Shutdown (HTTP2);
end Client_Cert;
