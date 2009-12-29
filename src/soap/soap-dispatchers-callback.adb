------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2003-2009, AdaCore                     --
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

package body SOAP.Dispatchers.Callback is

   -----------
   -- Clone --
   -----------

   overriding function Clone (Dispatch : Handler) return Handler is
   begin
      return (Dispatchers.Handler
              with Dispatch.HTTP_Callback, Dispatch.SOAP_Callback);
   end Clone;

   ------------
   -- Create --
   ------------

   function Create
     (HTTP_Callback : AWS.Response.Callback;
      SOAP_Callback : Dispatchers.SOAP_Callback) return Handler is
   begin
      return (Dispatchers.Handler with HTTP_Callback, SOAP_Callback);
   end Create;

   -------------------
   -- Dispatch_HTTP --
   -------------------

   overriding function Dispatch_HTTP
     (Dispatcher : Handler;
      Request    : AWS.Status.Data) return AWS.Response.Data is
   begin
      return Dispatcher.HTTP_Callback (Request);
   end Dispatch_HTTP;

   -------------------
   -- Dispatch_SOAP --
   -------------------

   overriding function Dispatch_SOAP
     (Dispatcher : Handler;
      SOAPAction : String;
      Payload    : Message.Payload.Object;
      Request    : AWS.Status.Data) return AWS.Response.Data is
   begin
      return Dispatcher.SOAP_Callback (SOAPAction, Payload, Request);
   end Dispatch_SOAP;

end SOAP.Dispatchers.Callback;
