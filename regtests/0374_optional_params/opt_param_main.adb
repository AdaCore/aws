------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                       Copyright (C) 2025, AdaCore                        --
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

with AWS.Client;
with AWS.Server.Status;
with AWS.Config.Set;
with AWS.Server.Status;
with SOAP.Dispatchers.Callback;

with urn.naws.wsdl_opt.recd_type_pkg;

with WSDL_Opt.Client;
with WSDL_Opt.Server;
with WSDL_Opt.Types;

with WSDL_Srv;

procedure Opt_Param_Main is
   use Ada;
   use Ada.Strings.Unbounded;
   use AWS;

   use WSDL_Opt;
   use WSDL_Opt.Types;
   use urn.naws.wsdl_opt.recd_type_pkg;

   WS   : AWS.Server.HTTP;
   H    : WSDL_Srv.Handler;
   Conf : Config.Object := Config.Get_Current;

   N   : Integer := 12;
   Rd  : WSDL_Opt.Types.tns_Recd_Type;
   Ra  : WSDL_Opt.Types.tns_RecA_Type;
   Nni : WSDL_Opt.Types.tns_NonNegativeInt_Type;

begin
   H := SOAP.Dispatchers.Callback.Create
     (WSDL_Srv.HTTP_CB'Access,
      WSDL_Srv.SOAP_CB'Access,
      Schema => WSDL_Opt.Schema);

   Config.Set.Server_Host (Conf, "localhost");
   Config.Set.Server_Port (Conf, 0);

   AWS.Server.Start (WS, H, Conf);

   --  All optional not set

   Text_IO.Put_Line ("=============== All optional unset");
   Rd.f2 := 2;

   WSDL_Opt.Client.Try
     (20, Rd,
      Endpoint => AWS.Server.Status.Local_URL (WS));

   --  All optional set

   Text_IO.Put_Line ("=============== All optional set");
   Ra.f := 102;
   Rd := (Rd with delta
          f1 => [1],
          f3 => [3],
          f4 => [Ra],
          f5 => [9]);
   Nni := 9;

   WSDL_Opt.Client.Try
     (20, Rd, [N], [Ra], [Nni],
      Endpoint => AWS.Server.Status.Local_URL (WS));

   --  Some optional set, some not

   Text_IO.Put_Line ("=============== Mixed optional unset/set");
   Ra.f := 102;
   Rd := (Rd with delta
          f3 => [],
          f5 => []);
   Nni := 9;

   WSDL_Opt.Client.Try
     (20, Rd, [], [], [Nni],
      Endpoint => AWS.Server.Status.Local_URL (WS));

   AWS.Server.Shutdown (WS);
end Opt_Param_Main;
