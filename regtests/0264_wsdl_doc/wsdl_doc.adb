------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                        Copyright (C) 2015, AdaCore                       --
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

with Ada.Strings.Unbounded;
with Ada.Text_IO;

with AWS.Config.Set;
with AWS.MIME;
with AWS.Net;
with AWS.Response;
with AWS.Server.Status;
with AWS.Status;

with SOAP.Types;
with SOAP.Utils;

with R_Hello_Demo.Client;
with R_Hello_Demo.Server;
with R_Hello_Demo.Types;

procedure WSDL_Doc is

   use Ada.Strings.Unbounded;
   use AWS;
   use R_Hello_Demo.Types;

   H_Server : Server.HTTP;
   CNF      : Config.Object;

   procedure WSDL_Demo_Client is
      use Ada;
      R : Sayhello_Result;
   begin
      R := R_Hello_Demo.Client.sayHello (Firstname => "AWS");
      Text_IO.Put_Line
        (To_String (R.Message) & SOAP.Types.Short'Image (R.Token));
   end WSDL_Demo_Client;

   function sayHello (Firstname : String) return Sayhello_Result;

   -------------
   -- SOAP_CB --
   -------------

   function SOAP_CB is new R_Hello_Demo.Server.sayHello_CB (sayHello);

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

   --------------
   -- sayHello --
   --------------

   function sayHello (Firstname : String) return Sayhello_Result is
   begin
      return (To_Unbounded_String ("Hello " & Firstname & " and welcome!"),
              12);
   end sayHello;

begin
   Config.Set.Server_Name (CNF, "WSDL Hello demo");
   Config.Set.Server_Host (CNF, "localhost");
   Config.Set.Server_Port (CNF, R_Hello_Demo.Server.Port);

   Server.Start (H_Server, CB'Unrestricted_Access, CNF);

   if Net.IPv6_Available then
      --  Need to start second server on same port but on the different
      --  Protocol_Family because we do not know which family would client try
      --  to connect.

      if AWS.Server.Status.Is_IPv6 (H_Server) then
         Server.Add_Listening
           (H_Server, "localhost", R_Hello_Demo.Server.Port, Net.FAMILY_INET);
      else
         Server.Add_Listening
           (H_Server, "localhost", R_Hello_Demo.Server.Port, Net.FAMILY_INET6);
      end if;
   end if;

   WSDL_Demo_Client;

   Server.Shutdown (H_Server);
end WSDL_Doc;
