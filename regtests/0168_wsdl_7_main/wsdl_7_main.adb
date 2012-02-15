------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2004-2012, AdaCore                     --
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
with SOAP.Types;

with WSDL_7_Server;
with WSDL_7_Service.CB;
with WSDL_7_Service.Client;

procedure WSDL_7_Main is

   use Ada;
   use AWS;
   use SOAP.Types;

   WS   : Server.HTTP;

   H    : WSDL_7_Service.CB.Handler;

   Conf : Config.Object := Config.Get_Current;

begin
   H := SOAP.Dispatchers.Callback.Create
     (WSDL_7_Server.HTTP_CB'Access, WSDL_7_Service.CB.SOAP_CB'Access);

   Config.Set.Server_Host (Conf, "localhost");
   Config.Set.Server_Port (Conf, 7707);

   Server.Start (WS, H, Conf);

   WSDL_7_Service.Client.Proc ("un essai");

   declare
      Result : constant String
        := WSDL_7_Service.Client.Func (987);
   begin
      Text_IO.Put_Line ("Result " & Result);
   end;

   Server.Shutdown (WS);
end WSDL_7_Main;
