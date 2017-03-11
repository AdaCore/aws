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

with Ada.Exceptions;

with SOAP.Message.Response.Error;

with API_Imp;

with API.Child_Service.Server;
with API.Child_Service.Types;

package body API.Child_Service.CB is

   use Ada.Exceptions;
   use SOAP;

   pragma Warnings (Off, API.Child_Service.Server);
   pragma Warnings (Off, API.Child_Service.Types);

   pragma Style_Checks (Off);

   function Call_CB is
     new API.Child_Service.Server.Call_CB (API_Imp.Call);

   ---------------------------
   -- Is_SOAPAction_Defined --
   ---------------------------

   function Is_SOAPAction_Defined
     (SOAPAction : String) return Boolean is
   begin
      if SOAPAction = "Call" then
         return True;
      else
         return False;
      end if;
   end Is_SOAPAction_Defined;

   -------------
   -- SOAP_CB --
   -------------

   function SOAP_CB
     (SOAPAction : String;
      Payload    : Message.Payload.Object;
      Request    : AWS.Status.Data)
      return Response.Data is
   begin
      if SOAPAction = "Call" then
         return Call_CB (SOAPAction, Payload, Request);

      else
         return Message.Response.Build
           (Message.Response.Error.Build
             (Message.Response.Error.Client,
              "Wrong SOAP action " & SOAPAction));
      end if;
   exception
      when E : others =>
         return Message.Response.Build
           (Message.Response.Error.Build
             (Message.Response.Error.Client,
              "Error in SOAP_CB for SOAPAction " & SOAPAction
                & " (" & Exception_Information (E) & ")"));
   end SOAP_CB;

end API.Child_Service.CB;
