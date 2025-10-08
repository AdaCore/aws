------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2024-2025, AdaCore                     --
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
with AWS.Server.Status;
with SOAP.Dispatchers.Callback;

with urn.naws.wsdl_choice.r1_type_pkg;
with urn.naws.wsdl_choice.r2_type_pkg;

with WSDL_Choice_Server;
with WSDL_Choice.Client;
with WSDL_Choice.Types;

procedure WSDL_Choice_Main is
   use Ada;
   use Ada.Strings.Unbounded;
   use AWS;

   use urn.naws.wsdl_choice.r1_type_pkg;
   use urn.naws.wsdl_choice.r2_type_pkg;

   WS   : Server.HTTP;
   H    : WSDL_Choice_Server.Handler;
   Conf : Config.Object := Config.Get_Current;

   S11 : constant R1_Type := (C0, 6);
   S12 : constant R1_Type := (C1, 2, 12.7);
   S13 : constant R1_Type := (C2, 3, To_Unbounded_String ("R1:whatever"));

   S21 : constant R2_Type := (C0, 1, 2, 3);
   S22 : constant R2_Type := (C1, 1, 2, 3, 25.1);
   S23 : constant R2_Type := (C2, 1, 2, 3, To_Unbounded_String ("R2:whatever"));

begin
   H := SOAP.Dispatchers.Callback.Create
     (WSDL_Choice_Server.HTTP_CB'Access,
      WSDL_Choice_Server.SOAP_CB'Access,
      WSDL_Choice.Schema);

   Config.Set.Server_Host (Conf, "localhost");
   Config.Set.Server_Port (Conf, 0);

   Server.Start (WS, H, Conf);

   Text_IO.Put_Line ("====== Try C1 - C0");
   WSDL_Choice.Client.Try (S12, S21, Endpoint => Server.Status.Local_URL (WS));

   Text_IO.Put_Line ("====== Try C2 - C1");
   WSDL_Choice.Client.Try (S13, S22, Endpoint => Server.Status.Local_URL (WS));

   Text_IO.Put_Line ("====== Try C0 - C2");
   WSDL_Choice.Client.Try (S11, S23, Endpoint => Server.Status.Local_URL (WS));

   Text_IO.Put_Line ("Run OK");

   Server.Shutdown (WS);
end WSDL_Choice_Main;
