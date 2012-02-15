------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2003-2012, AdaCore                     --
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

with Ada.Strings.Unbounded;
with Ada.Text_IO;

with AWS.Config.Set;
with AWS.Server;

with SOAP.Dispatchers.Callback;

with WSDL_5;
with WSDL_5_Server;
with WSDL_5_Service.Client;
with WSDL_5_Service.Types;
with WSDL_5_Service.Cb;

procedure WSDL_5_Main is

   use Ada;
   use Ada.Strings.Unbounded;
   use AWS;

   WS   : Server.HTTP;

   H    : WSDL_5_Service.Cb.Handler;

   Conf : Config.Object := Config.Get_Current;

   Res  : WSDL_5.Color;

begin
   H := SOAP.Dispatchers.Callback.Create
     (WSDL_5_Server.HTTP_CB'Access, WSDL_5_Service.Cb.SOAP_CB'Access);

   Config.Set.Server_Host (Conf, "localhost");
   Config.Set.Server_Port (Conf, 7705);

   Server.Start (WS, H, Conf);

   WSDL_5_Service.Client.Register ("pascal", WSDL_5.Red);

   Res := WSDL_5_Service.Client.One_Color;

   Text_IO.Put_Line ("Color = " & WSDL_5.Color'Image (Res));

   Server.Shutdown (WS);
end WSDL_5_Main;
