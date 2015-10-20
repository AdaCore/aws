------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2003-2015, AdaCore                     --
--                                                                          --
--  This library is free software;  you can redistribute it and/or modify   --
--  it under terms of the  GNU General Public License  as published by the  --
--  Free Software  Foundation;  either version 3,  or (at your  option) any --
--  later version. This library is distributed in the hope that it will be  --
--  useful, but WITHOUT ANY WARRANTY;  without even the implied warranty of --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                    --
--                                                                          --
--  As a special exception under Section 7 of GPL version 3, you are        --
--  granted additional permissions described in the GCC Runtime Library     --
--  Exception, version 3.1, as published by the Free Software Foundation.   --
--                                                                          --
--  You should have received a copy of the GNU General Public License and   --
--  a copy of the GCC Runtime Library Exception along with this program;    --
--  see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see   --
--  <http://www.gnu.org/licenses/>.                                         --
--                                                                          --
--  As a special exception, if other files instantiate generics from this   --
--  unit, or you link this unit with other files to produce an executable,  --
--  this  unit  does not  by itself cause  the resulting executable to be   --
--  covered by the GNU General Public License. This exception does not      --
--  however invalidate any other reasons why the executable file  might be  --
--  covered by the  GNU Public License.                                     --
------------------------------------------------------------------------------

--  Dispatch on a SOAP Callback procedures

with SOAP.Message;
with SOAP.WSDL.Schema;

package SOAP.Dispatchers.Callback is

   type Handler is new Dispatchers.Handler with private;
   --  This is a simple wrapper around standard callback procedure (access to
   --  function). It will be used to build dispatchers services and for the
   --  main server callback.

   overriding function Schema
     (Dispatcher : Handler;
      SOAPAction : String)
      return WSDL.Schema.Definition;

   function Create
     (HTTP_Callback : AWS.Response.Callback;
      SOAP_Callback : Dispatchers.SOAP_Callback;
      Schema        : WSDL.Schema.Definition :=
                        WSDL.Schema.Empty) return Handler;
   --  Build a dispatcher for the specified callback

private

   overriding function Clone (Dispatch : Handler) return Handler;
   --  Returns a deep copy of the dispatcher

   overriding function Dispatch_SOAP
     (Dispatcher : Handler;
      SOAPAction : String;
      Payload    : Message.Payload.Object;
      Request    : AWS.Status.Data) return AWS.Response.Data;
   --  This dispatch function is called for SOAP requests

   overriding function Dispatch_HTTP
     (Dispatcher : Handler;
      Request    : AWS.Status.Data) return AWS.Response.Data;
   --  This dispatch function is called for standard HTTP requests

   type Handler is new Dispatchers.Handler with record
      HTTP_Callback : AWS.Response.Callback;
      SOAP_Callback : Dispatchers.SOAP_Callback;
   end record;

end SOAP.Dispatchers.Callback;
