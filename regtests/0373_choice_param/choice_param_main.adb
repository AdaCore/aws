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

with urn.naws.wsdl_choice.state_type_pkg;
with urn.naws.wsdl_choice.status_type_pkg;

with WSDL_Choice.Client;
with WSDL_Choice.Server;
with WSDL_Choice.Types;

with WSDL_Srv;

procedure Choice_Param_Main is
   use Ada;
   use Ada.Strings.Unbounded;
   use AWS;

   use WSDL_Choice;
   use WSDL_Choice.Types;
   use urn.naws.wsdl_choice.state_type_pkg;
   use urn.naws.wsdl_choice.status_type_pkg;

   WS   : AWS.Server.HTTP;
   H    : WSDL_Srv.Handler;
   Conf : Config.Object := Config.Get_Current;

   S1 : constant WSDL_Choice.Types.Status_Type :=
          (C1, RUNNING);
   S2 : constant WSDL_Choice.Types.Status_Type :=
          (C2, 12.7);
   S3 : constant WSDL_Choice.Types.Status_Type :=
          (C3, To_Unbounded_String ("whatever"));
begin
   H := SOAP.Dispatchers.Callback.Create
     (WSDL_Srv.HTTP_CB'Access,
      WSDL_Srv.SOAP_CB'Access,
      Schema => WSDL_Choice.Schema);

   Config.Set.Server_Host (Conf, "localhost");
   Config.Set.Server_Port (Conf, 0);

   AWS.Server.Start (WS, H, Conf);

   WSDL_Choice.Client.Try (S1, Endpoint => AWS.Server.Status.Local_URL (WS));
   WSDL_Choice.Client.Try (S2, Endpoint => AWS.Server.Status.Local_URL (WS));
   WSDL_Choice.Client.Try (S3, Endpoint => AWS.Server.Status.Local_URL (WS));

   AWS.Server.Shutdown (WS);
end Choice_Param_Main;
