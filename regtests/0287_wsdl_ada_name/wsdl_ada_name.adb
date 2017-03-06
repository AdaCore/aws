------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                      Copyright (C) 2017, AdaCore                         --
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

--  SOAP/WSDL test

with Ada.Text_IO;

with AWS.Config.Set;
with AWS.MIME;
with AWS.Net;
with AWS.Response;
with AWS.Server.Status;
with AWS.Status;
with SOAP.Utils;

with R.Hello_Demo.Client;
with R.Hello_Demo.Server;

procedure WSDL_Ada_Name is

   use AWS;

   H_Server : Server.HTTP;
   CNF      : Config.Object;

   procedure WSDL_Demo_Client is
      use Ada;
   begin
      Text_IO.Put_Line
        (R.Hello_Demo.Client.Say_Hello
           (First_Name => "AWS",
            Endpoint   => AWS.Server.Status.Local_URL (H_Server)));
   end WSDL_Demo_Client;

   function Say_Hello (Firstname : String) return String;

   -------------
   -- SOAP_CB --
   -------------

   function SOAP_CB is new R.Hello_Demo.Server.Say_Hello_CB (Say_Hello);

   function SOAP_Wrapper is new SOAP.Utils.SOAP_Wrapper (SOAP_CB);

   --------
   -- CB --
   --------

   function CB (Request : Status.Data) return Response.Data is
      SOAPAction : constant String := Status.SOAPAction (Request);
   begin
      if SOAPAction = "sayHello" then
         return SOAP_Wrapper (Request);
      else
         return Response.Build (MIME.Text_HTML, "<p>Not a SOAP request");
      end if;
   end CB;

   ---------------
   -- Say_Hello --
   ---------------

   function Say_Hello (Firstname : String) return String is
   begin
      return "Hello " & Firstname & " and welcome!";
   end Say_Hello;

begin
   Config.Set.Server_Name (CNF, "WSDL Hello demo");
   Config.Set.Server_Host (CNF, "localhost");
   Config.Set.Server_Port (CNF, 0);

   Server.Start (H_Server, CB'Unrestricted_Access, CNF);

   if Net.IPv6_Available then
      --  Need to start second server on same port but on the different
      --  Protocol_Family because we do not know which family would client try
      --  to connect.

      if AWS.Server.Status.Is_IPv6 (H_Server) then
         Server.Add_Listening
           (H_Server, "localhost",
            AWS.Server.Status.Port (H_Server), Net.FAMILY_INET);
      else
         Server.Add_Listening
           (H_Server, "localhost",
            AWS.Server.Status.Port (H_Server), Net.FAMILY_INET6);
      end if;
   end if;

   WSDL_Demo_Client;

   Server.Shutdown (H_Server);
end WSDL_Ada_Name;
