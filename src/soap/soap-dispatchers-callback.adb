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

package body SOAP.Dispatchers.Callback is

   -----------
   -- Clone --
   -----------

   overriding function Clone (Dispatch : Handler) return Handler is
   begin
      return H : Handler do
         H.Schema        := Dispatch.Schema.Copy;
         H.HTTP_Callback := Dispatch.HTTP_Callback;
         H.SOAP_Callback := Dispatch.SOAP_Callback;
      end return;
   end Clone;

   ------------
   -- Create --
   ------------

   function Create
     (HTTP_Callback : AWS.Response.Callback;
      SOAP_Callback : Dispatchers.SOAP_Callback;
      Schema        : WSDL.Schema.Definition :=
                        WSDL.Schema.Empty) return Handler is
   begin
      return (AWS.Dispatchers.Handler
              with Schema, HTTP_Callback, SOAP_Callback);
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

   ------------
   -- Schema --
   ------------

   overriding function Schema
     (Dispatcher : Handler;
      SOAPAction : String)
      return WSDL.Schema.Definition
   is
      pragma Unreferenced (SOAPAction);
   begin
      return Dispatcher.Schema;
   end Schema;

end SOAP.Dispatchers.Callback;
