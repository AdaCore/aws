------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                            Copyright (C) 2003                            --
--                                ACT-Europe                                --
--                                                                          --
--  Authors: Dmitriy Anisimokv - Pascal Obry                                --
--                                                                          --
--  This library is free software; you can redistribute it and/or modify    --
--  it under the terms of the GNU General Public License as published by    --
--  the Free Software Foundation; either version 2 of the License, or (at   --
--  your option) any later version.                                         --
--                                                                          --
--  This library is distributed in the hope that it will be useful, but     --
--  WITHOUT ANY WARRANTY; without even the implied warranty of              --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU       --
--  General Public License for more details.                                --
--                                                                          --
--  You should have received a copy of the GNU General Public License       --
--  along with this library; if not, write to the Free Software Foundation, --
--  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.          --
--                                                                          --
--  As a special exception, if other files instantiate generics from this   --
--  unit, or you link this unit with other files to produce an executable,  --
--  this  unit  does not  by itself cause  the resulting executable to be   --
--  covered by the GNU General Public License. This exception does not      --
--  however invalidate any other reasons why the executable file  might be  --
--  covered by the  GNU Public License.                                     --
------------------------------------------------------------------------------

--  $Id$

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

   -------------
   -- HTTP_CB --
   -------------

   function HTTP_CB (Request : in Status.Data) return Response.Data is
   begin
      return Response.Build
        (MIME.Text_HTML, "No HTTP request should be called.");
   end HTTP_CB;

   -------------
   -- SOAP_CB --
   -------------

   function SOAP_CB
     (SOAPAction : in String;
      Payload    : in Message.Payload.Object;
      Request    : in Status.Data)
      return Response.Data is
   begin
      if SOAPAction = "Try" then
         return Try_CB (SOAPAction, Payload, Request);

      elsif SOAPAction = "Try2" then
         return Try2_CB (SOAPAction, Payload, Request);

      elsif SOAPAction = "Try3" then
         return Try3_CB (SOAPAction, Payload, Request);

      else
         return Message.Response.Build
           (Message.Response.Error.Build
              (Message.Response.Error.Client,
               "Wrong SOAP action " & SOAPAction));
      end if;
   end SOAP_CB;

end WSDL_4_Server;
