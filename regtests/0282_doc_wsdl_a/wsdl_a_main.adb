------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2016-2020, AdaCore                     --
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

with Ada.Text_IO;

with AWS.Config.Set;
with AWS.Server;

with SOAP.Dispatchers.Callback;

with WSDL_a_Server;
with WSDL_a_Service.Client;
with WSDL_a_Service.Server;
with WSDL_a_Service.Types;

procedure WSDL_a_Main is

   use Ada;
   use AWS;

   WS   : Server.HTTP;

   H    : WSDL_a_Server.Handler;

   Conf : Config.Object := Config.Get_Current;

   R1, R2 : WSDL_a_Service.Types.R_Type;

begin
   H := SOAP.Dispatchers.Callback.Create
     (WSDL_a_Server.HTTP_CB'Access,
      WSDL_a_Server.SOAP_CB'Access,
      Schema => WSDL_a_Service.Schema);

   Config.Set.Server_Host (Conf, "localhost");
   Config.Set.Server_Port (Conf, WSDL_a_Service.Server.Port);

   Server.Start (WS, H, Conf);

   WSDL_a_Service.Client.Print (12);
   WSDL_a_Service.Client.Print (98712);
   WSDL_a_Service.Client.Print_Small (122);
   Text_IO.Put_Line (WSDL_a_Service.Client.Image (789));
   Text_IO.Put_Line (WSDL_a_Service.Client.Image (-1));

   R1.A := 8;
   R1.B := 2;
   R2 := WSDL_a_Service.Client.Call (R1);

   Server.Shutdown (WS);
end WSDL_a_Main;
