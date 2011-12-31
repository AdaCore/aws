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

with WSDL_3;
with WSDL_3_Service.Server;
with WSDL_3_Service.Types;

package body WSDL_3_Server is

   use Ada;
   use SOAP;

   function Image_Rec1_CB is
      new WSDL_3_Service.Server.Image_Rec1_CB (WSDL_3.Image_Rec1);

   function Image_Rec2_CB is
      new WSDL_3_Service.Server.Image_Rec2_CB (WSDL_3.Image_Rec2);

   function Image_Rec3_CB is
      new WSDL_3_Service.Server.Image_Rec3_CB (WSDL_3.Image_Rec3);

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
      if SOAPAction = "Image_Rec1" then
         return Image_Rec1_CB (SOAPAction, Payload, Request);

      elsif SOAPAction = "Image_Rec2" then
         return Image_Rec2_CB (SOAPAction, Payload, Request);

      elsif SOAPAction = "Image_Rec3" then
         return Image_Rec3_CB (SOAPAction, Payload, Request);

      else
         return Message.Response.Build
           (Message.Response.Error.Build
              (Message.Response.Error.Client,
               "Wrong SOAP action " & SOAPAction));
      end if;
   end SOAP_CB;

end WSDL_3_Server;
