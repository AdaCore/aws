------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2003-2008, AdaCore                     --
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

with Ada.Text_IO;

with AWS.Config.Set;
with AWS.Server;

with SOAP.Dispatchers.Callback;

with WSDL_1_Server;
with WSDL_1_Service.Client;

procedure WSDL_1_Main is

   use Ada;
   use AWS;

   WS   : Server.HTTP;

   H    : WSDL_1_Server.Handler;

   Conf : Config.Object := Config.Get_Current;

begin
   H := SOAP.Dispatchers.Callback.Create
     (WSDL_1_Server.HTTP_CB'Access, WSDL_1_Server.SOAP_CB'Access);

   Config.Set.Server_Port (Conf, 7701);

   Server.Start (WS, H, Conf);

   WSDL_1_Service.Client.Print (12);
   WSDL_1_Service.Client.Print (98712);
   WSDL_1_Service.Client.Print_Small (122);
   Text_IO.Put_Line (WSDL_1_Service.Client.Image (789));
   Text_IO.Put_Line (WSDL_1_Service.Client.Image (-1));

   Server.Shutdown (WS);
end WSDL_1_Main;
