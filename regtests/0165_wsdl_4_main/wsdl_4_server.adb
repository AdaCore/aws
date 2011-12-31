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

with Ada.Text_IO;

with AWS.MIME;
with SOAP.Message.Response.Error;

with WSDL_4;
with WSDL_4_Service.Server;
with WSDL_4_Service.Types;

package body WSDL_4_Server is

   use Ada;
   use SOAP;

   function Try_CB is
      new WSDL_4_Service.Server.Try_CB (WSDL_4.Try);

   function Try2_CB is
      new WSDL_4_Service.Server.Try2_CB (WSDL_4.Try2);

   function Try3_CB is
      new WSDL_4_Service.Server.Try3_CB (WSDL_4.Try3);

   function Try4_CB is
      new WSDL_4_Service.Server.Try4_CB (WSDL_4.Try4);

   -------------
   -- HTTP_CB --
   -------------

   function HTTP_CB (Request : Status.Data) return Response.Data is
   begin
      return Response.Build
        (MIME.Text_HTML, "No HTTP request should be called.");
   end HTTP_CB;

   -------------
   -- SOAP_CB --
   -------------

   function SOAP_CB
     (SOAPAction : String;
      Payload    : Message.Payload.Object;
      Request    : Status.Data)
      return Response.Data is
   begin
      if SOAPAction = "Try" then
         return Try_CB (SOAPAction, Payload, Request);

      elsif SOAPAction = "Try2" then
         return Try2_CB (SOAPAction, Payload, Request);

      elsif SOAPAction = "Try3" then
         return Try3_CB (SOAPAction, Payload, Request);

      elsif SOAPAction = "Try4" then
         return Try4_CB (SOAPAction, Payload, Request);

      else
         return Message.Response.Build
           (Message.Response.Error.Build
              (Message.Response.Error.Client,
               "Wrong SOAP action " & SOAPAction));
      end if;
   end SOAP_CB;

end WSDL_4_Server;
