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

with AWS.Response;
with AWS.Status;

with SOAP.Dispatchers.Callback;
with SOAP.Message.Payload;

package API.Child_Service.CB is

   use AWS;
   use SOAP;

   pragma Style_Checks (Off);

   subtype Handler is SOAP.Dispatchers.Callback.Handler;

   function Is_SOAPAction_Defined
     (SOAPAction : String) return Boolean;
   --  Returns True if SOAPAction handled by SOAP_CB below

   function SOAP_CB
     (SOAPAction : String;
      Payload    : Message.Payload.Object;
      Request    : AWS.Status.Data)
      return Response.Data;

end API.Child_Service.CB;
