------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2000-2003                          --
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

with AWS.Client;
with SOAP.Message.Payload;
with SOAP.Message.Response;

package SOAP.Client is

   Not_Specified : String renames AWS.Client.No_Data;

   function Call
     (URL        : in String;
      P          : in Message.Payload.Object;
      SOAPAction : in String                     := Not_Specified;
      User       : in String                     := Not_Specified;
      Pwd        : in String                     := Not_Specified;
      Proxy      : in String                     := Not_Specified;
      Proxy_User : in String                     := Not_Specified;
      Proxy_Pwd  : in String                     := Not_Specified;
      Timeouts   : in AWS.Client.Timeouts_Values := AWS.Client.No_Timeout)
      return Message.Response.Object'Class;
   --  Send a SOAP HTTP request to URL address. The P is the Payload and
   --  SOAPAction is the required HTTP field. If it is not specified then the
   --  URI (URL resource) will be used for the SOAPAction field. The complete
   --  format is "URL & '#' & Procedure_Name" (Procedure_Name is retrieved
   --  from the Payload object.

   function Call
     (Connection : in AWS.Client.HTTP_Connection;
      SOAPAction : in String;
      P          : in Message.Payload.Object)
      return Message.Response.Object'Class;
   --  Idem as above, but use an already opened connection

end SOAP.Client;
