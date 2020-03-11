------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                       Copyright (C) 2020, AdaCore                        --
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

with WSDL_NaN;
with WSDL_NaN_Service.Server;

package body WSDL_NaN_Server is

   use Ada;
   use SOAP;

   function Echo_F_CB is
      new WSDL_NaN_Service.Server.Echo_F_CB (WSDL_Nan.Echo_F);
   function Echo_D_CB is
      new WSDL_NaN_Service.Server.Echo_D_CB (WSDL_Nan.Echo_D);

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
      if SOAPAction = "Echo_F" then
         return Echo_F_CB (SOAPAction, Payload, Request);

      elsif SOAPAction = "Echo_D" then
         return Echo_D_CB (SOAPAction, Payload, Request);

      else
         return Message.Response.Build
           (Message.Response.Error.Build
              (Message.Response.Error.Client,
               "Wrong SOAP action " & SOAPAction));
      end if;
   end SOAP_CB;

end WSDL_NaN_Server;
