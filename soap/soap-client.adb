------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2000-2004                          --
--                                ACT-Europe                                --
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

with SOAP.Message.XML;
with SOAP.Message.Payload;

with AWS.Client;
with AWS.Response;
with AWS.URL;

package body SOAP.Client is

   ----------
   -- Call --
   ----------

   function Call
     (URL        : in String;
      P          : in Message.Payload.Object;
      SOAPAction : in String                     := No_SOAPAction;
      User       : in String                     := Not_Specified;
      Pwd        : in String                     := Not_Specified;
      Proxy      : in String                     := Not_Specified;
      Proxy_User : in String                     := Not_Specified;
      Proxy_Pwd  : in String                     := Not_Specified;
      Timeouts   : in AWS.Client.Timeouts_Values := AWS.Client.No_Timeout)
      return Message.Response.Object'Class
   is

      function SOAP_Action return String;
      --  Returns the proper SOAPAction string for this call

      Connection : AWS.Client.HTTP_Connection;

      -----------------
      -- SOAP_Action --
      -----------------

      function SOAP_Action return String is
      begin
         if SOAPAction = No_SOAPAction then
            declare
               URL_Object : constant AWS.URL.Object := AWS.URL.Parse (URL);
            begin
               return AWS.URL.URL (URL_Object) & '#'
                      & SOAP.Message.Payload.Procedure_Name (P);
            end;

         elsif SOAPAction = "" then
            --  Empty SOAP Action
            return """""";

         else
            return SOAPAction;
         end if;
      end SOAP_Action;

   begin
      AWS.Client.Create
        (Connection,
         URL, User, Pwd, Proxy, Proxy_User, Proxy_Pwd,
         Persistent => False,
         Timeouts   => Timeouts);

      declare
         Result : constant Message.Response.Object'Class
           := Call (Connection, SOAP_Action, P);
      begin
         AWS.Client.Close (Connection);
         return Result;
      end;
   exception
      when others =>
         AWS.Client.Close (Connection);
         raise;
   end Call;

   ----------
   -- Call --
   ----------

   function Call
     (Connection : in AWS.Client.HTTP_Connection;
      SOAPAction : in String;
      P          : in Message.Payload.Object)
      return Message.Response.Object'Class
   is
      Response : AWS.Response.Data;
   begin
      AWS.Client.SOAP_Post
        (Connection, Response, SOAPAction, SOAP.Message.XML.Image (P), True);
      return Message.XML.Load_Response (Connection);
   end Call;

end SOAP.Client;
