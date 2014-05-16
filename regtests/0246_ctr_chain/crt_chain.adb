------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                      Copyright (C) 2013-2014, AdaCore                    --
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

--  Test for client/server certificates chain authorization

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

procedure Crt_Chain is

   use Ada;
   use Ada.Text_IO;
   use AWS;

   function CB (Request : Status.Data) return Response.Data;

   HTTP : Server.HTTP;
   R    : Response.Data;

   procedure Print (Title : String; Item : Net.SSL.Certificate.Object);

   --------
   -- CB --
   --------

   function CB (Request : Status.Data) return Response.Data is
   begin
      Print
        ("Server side: ",
         Net.SSL.Certificate.Get
           (Net.SSL.Socket_Type (AWS.Status.Socket (Request).all)));
      return Response.Build (MIME.Text_Plain, "OK");
   end CB;

   -----------
   -- Print --
   -----------

   procedure Print (Title : String; Item : Net.SSL.Certificate.Object) is
      use Net.SSL.Certificate;
      Issuer : constant String := Net.SSL.Certificate.Issuer (Item);
      Prefix : constant String :=
        "C=FR,ST=Ile de France,L=Paris,O=AdaCore,OU=CA-office,CN=test-";
   begin
      if Issuer'Length >= Prefix'Length
        and then Issuer (Prefix'Range) /= Prefix
      then
         Text_IO.Put_Line ("Wrong Issuer " & Issuer);
      end if;

      Text_IO.Put_Line
        (Title & Common_Name (Item) & ' ' & Serial_Number (Item) & ' '
         & Issuer (Prefix'Length .. Issuer'Last) & ' '
         & (if Verified (Item) then "OK" else Status_Message (Item)));
   end Print;

   ---------------
   -- Test_Cert --
   ---------------

   procedure Test_Cert is
      Connect : Client.HTTP_Connection;
      SSL : Net.SSL.config;
   begin
      Net.SSL.Initialize
        (SSL,
         Certificate_Filename => "chain-clt.crt",
         Key_Filename         => "test-clt.key",
         Trusted_CA_Filename  => "ca-srv.crt");

      Client.Create
        (Connection => Connect,
         Host       => Server.Status.Local_URL (HTTP),
         Timeouts   => Client.Timeouts (Each => 5.0),
         SSL_Config => SSL);

      Client.Get (Connect, R, "/");

      Print ("Client side: ", Client.Get_Certificate (Connect));

      Put_Line ("-> " & Messages.Image (Response.Status_Code (R)));
      Put_Line ("-> " & Response.Message_Body (R));

      Client.Close (Connect);
   end Test_Cert;

   -----------------
   -- Verify_Cert --
   -----------------

   function Verify_Cert (Cert : Net.SSL.Certificate.Object) return Boolean is
   begin
      Print ("Verify call: ", Cert);
      return Net.SSL.Certificate.Verified (Cert);
   end Verify_Cert;

   CNF : Config.Object := Config.Get_Current;
   SSL : Net.SSL.config;

begin
   Net.SSL.Initialize
     (SSL,
      Certificate_Filename => Config.Certificate (Cnf),
      Key_Filename         => Config.Key (Cnf),
      Exchange_Certificate => Config.Exchange_Certificate (Cnf),
      Certificate_Required => Config.Certificate_Required (Cnf),
      Trusted_CA_Filename  => Config.Trusted_CA (Cnf));

   Net.SSL.Certificate.Set_Verify_Callback
     (SSL, Verify_Cert'Unrestricted_Access);
   Server.Set_SSL_Config (HTTP, SSL);

   Server.Start (HTTP, CB'Unrestricted_Access, CNF);

   Test_Cert;
   Server.Shutdown (HTTP);

exception
   when E : others =>
      Put_Line ("Main Error " & Exceptions.Exception_Information (E));
      Server.Shutdown (HTTP);
end Crt_Chain;
