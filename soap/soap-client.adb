------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2000-2001                          --
--                                ACT-Europe                                --
--                                                                          --
--  Authors: Dmitriy Anisimkov - Pascal Obry                                --
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

with Ada.Strings.Unbounded;

with SOAP.Message.XML;
with SOAP.Message.Payload;

with AWS.Client;
with AWS.Response;
with AWS.URL;

package body SOAP.Client is

   use Ada.Strings.Unbounded;

   ----------
   -- Call --
   ----------

   function Call
     (URL        : in String;
      P          : in Message.Payload.Object;
      SOAPAction : in String         := Not_Specified)
     return Message.Response.Object'Class
   is

      procedure RPC_Call;
      --  Does the actual RPC over HTTP call.

      Message_Body : Unbounded_String;
      Response     : AWS.Response.Data;

      --------------
      -- RPC_Call --
      --------------

      procedure RPC_Call is
      begin
         if SOAPAction = Not_Specified then
            declare
               URL_Object : AWS.URL.Object := AWS.URL.Parse (URL);
            begin
               Response := AWS.Client.SOAP_Post
                 (URL,
                  To_String (Message_Body),
                  AWS.URL.URL (URL_Object));
            end;

         else
            Response := AWS.Client.SOAP_Post
              (URL,
               To_String (Message_Body),
               SOAPAction);
         end if;
      end RPC_Call;

   begin
      Message_Body := SOAP.Message.XML.Image (P);

      RPC_Call;

      return Message.XML.Load_Response (AWS.Response.Message_Body (Response));
   end Call;

end SOAP.Client;
