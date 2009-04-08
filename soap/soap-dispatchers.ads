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

--  Dispatcher for SOAP requests

with AWS.Dispatchers;
with AWS.Response;
with AWS.Status;
with SOAP.Message.Payload;

package SOAP.Dispatchers is

   type Handler is abstract new AWS.Dispatchers.Handler with private;
   --  This dispatcher will send SOAP and HTTP requests to different routines

   type SOAP_Callback is
     access function (SOAPAction : String;
                      Payload    : Message.Payload.Object;
                      Request    : AWS.Status.Data)
                      return AWS.Response.Data;
   --  This is the SOAP Server callback type. SOAPAction is the HTTP header
   --  SOAPAction value, Payload is the parsed XML payload, request is the
   --  HTTP request status.

   function Dispatch_SOAP
     (Dispatcher : Handler;
      SOAPAction : String;
      Payload    : Message.Payload.Object;
      Request    : AWS.Status.Data)
      return AWS.Response.Data is abstract;
   --  This dispatch function is called for SOAP requests

   function Dispatch_HTTP
     (Dispatcher : Handler;
      Request    : AWS.Status.Data)
      return AWS.Response.Data is abstract;
   --  This dispatch function is called for standard HTTP requests

private

   overriding function Dispatch
     (Dispatcher : Handler;
      Request    : AWS.Status.Data) return AWS.Response.Data;

   type Handler is abstract new AWS.Dispatchers.Handler with null record;

end SOAP.Dispatchers;
