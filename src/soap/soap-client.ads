------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2000-2015, AdaCore                     --
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

pragma Ada_2012;

with AWS.Client;
with SOAP.Message.Payload;
with SOAP.Message.Response;
with SOAP.WSDL.Schema;

package SOAP.Client is

   Not_Specified : String renames AWS.Client.No_Data;

   function Call
     (URL          : String;
      P            : Message.Payload.Object;
      SOAPAction   : String                     := No_SOAPAction;
      User         : String                     := Not_Specified;
      Pwd          : String                     := Not_Specified;
      Proxy        : String                     := Not_Specified;
      Proxy_User   : String                     := Not_Specified;
      Proxy_Pwd    : String                     := Not_Specified;
      Timeouts     : AWS.Client.Timeouts_Values := AWS.Client.No_Timeout;
      Asynchronous : Boolean                    := False;
      Schema       : WSDL.Schema.Definition     := WSDL.Schema.Empty)
      return Message.Response.Object'Class
   with Pre => URL'Length > 0;
   --  Send a SOAP HTTP request to URL address. The P is the Payload and
   --  SOAPAction is the required HTTP field. If it is not specified then the
   --  URI (URL resource) will be used for the SOAPAction field. The complete
   --  format is "URL & '#' & Procedure_Name" (Procedure_Name is retrieved
   --  from the Payload object.
   --
   --  If Asynchronous is set to True the response from the server may be
   --  empty. In this specific case the success of the call depends on the
   --  HTTP status code.

   function Call
     (Connection   : AWS.Client.HTTP_Connection;
      SOAPAction   : String;
      P            : Message.Payload.Object;
      Asynchronous : Boolean := False;
      Schema       : WSDL.Schema.Definition := WSDL.Schema.Empty)
      return Message.Response.Object'Class
   with Pre => AWS.Client.Host (Connection)'Length > 0;
   --  Idem as above, but use an already opened connection

end SOAP.Client;
