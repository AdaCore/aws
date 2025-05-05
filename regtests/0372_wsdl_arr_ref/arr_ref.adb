------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                       Copyright (C) 2025, AdaCore                        --
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

pragma Ada_2022;

with Ada.Text_IO;

with AWS.Config.Set;
with AWS.MIME;
with AWS.Net;
with AWS.Response;
with AWS.Server.Status;
with AWS.Status;
with SOAP.Utils;

with ESService.Client;
with ESService.Server;
with ESService.Types;

procedure Arr_Ref is

   use Ada;
   use AWS;
   use ESService;
   use ESService.Types;

   H_Server : AWS.Server.HTTP;

   function OESResp
     (ESResp : tns_ESResp_Type)
      return OESResp_Result;

   -------------
   -- SOAP_CB --
   -------------

   function SOAP_CB is
      new ESService.Server.OESResp_CB (OESResp);

   function SOAP_Wrapper is new SOAP.Utils.SOAP_Wrapper (SOAP_CB);

   --------
   -- CB --
   --------

   function CB (Request : Status.Data) return Response.Data is
      SOAPAction : constant String := Status.SOAPAction (Request);
   begin
      if SOAPAction = "http://localhost/esp" then
         return SOAP_Wrapper (Request);
      else
         return Response.Build (MIME.Text_HTML, "<p>Not a SOAP request");
      end if;
   end CB;

   -----------------------
   -- GetLastTradePrice --
   -----------------------

   function OESResp
     (ESResp : tns_ESResp_Type)
      return OESResp_Result
   is
      R : OESResp_Result;
   begin
      Text_IO.Put_Line ("E:" & ESResp.EGSI'Image);
      return R;
   end OESResp;

   ----------------------
   -- WSDL_Demo_Client --
   ----------------------

   procedure WSDL_Demo_Client is
      Result : Types.OESResp_Result;
      P      : Types.ESResp_Type;
   begin
      P.EGSI.Append (ESI_Type'(V => 1));
      P.EGSI.Append (ESI_Type'(V => 4));
      Result := ESService.Client.OESResp (P);
   end WSDL_Demo_Client;

   CNF : Config.Object;

begin
   Config.Set.Server_Name (CNF, "Array of Array as parameter");
   Config.Set.Server_Host (CNF, "localhost");
   Config.Set.Server_Port (CNF, ESService.Server.Port);

   AWS.Server.Start (H_Server, CB'Unrestricted_Access, CNF);

   if Net.IPv6_Available and then AWS.Server.Status.Is_IPv6 (H_Server) then
      --  Need to start second server on same port but on the different
      --  Protocol_Family because we do not know which family would client try
      --  to connect.

      AWS.Server.Add_Listening
        (H_Server, "localhost", ESService.Server.Port,
         Net.FAMILY_INET);
   end if;

   WSDL_Demo_Client;

   AWS.Server.Shutdown (H_Server);
end Arr_Ref;
