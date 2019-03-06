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

with Ada.Strings.Unbounded;
with Ada.Text_IO;

with AWS.Config.Set;
with AWS.MIME;
with AWS.Net;
with AWS.Response;
with AWS.Server.Status;
with AWS.Status;

with SOAP.Dispatchers.Callback;
with SOAP.Utils;

with API.Child_Service.CB;
with API.Child_Service.Client;
with API.Child_Service.Server;
with API.Child_Service.Types;

procedure Multiple_Schema is

   use Ada.Strings.Unbounded;
   use AWS;

   H_Server : Server.HTTP;
   CNF      : Config.Object;
   Disp     : API.Child_Service.CB.Handler;

   procedure WSDL_Demo_Client is
      use Ada;
      R : API.Child_Service.Types.Rec_Type :=
        (V => To_Unbounded_String ("from client"),
         A => 11#AAAA#,
         B => To_Unbounded_String ("B value"),
         C => 5,
         D => 9912);
   begin
      API.Child_Service.Client.Call
        (O        => R,
         Endpoint => AWS.Server.Status.Local_URL (H_Server));
   end WSDL_Demo_Client;

   --------
   -- CB --
   --------

   function CB (Request : Status.Data) return Response.Data is
      SOAPAction : constant String := Status.SOAPAction (Request);
   begin
      return Response.Build (MIME.Text_HTML, "<p>Not a SOAP request");
   end CB;

begin
   Config.Set.Server_Name (CNF, "WSDL Multiple Schema");
   Config.Set.Server_Host (CNF, "localhost");
   Config.Set.Server_Port (CNF, 0);

   Disp := SOAP.Dispatchers.Callback.Create
     (CB'Unrestricted_Access,
      API.Child_Service.CB.SOAP_CB'Access,
      API.Child_Service.Schema);

   Server.Start (H_Server, Disp, CNF);

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
end Multiple_Schema;
