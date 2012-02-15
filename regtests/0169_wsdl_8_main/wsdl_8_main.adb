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

with Ada.Strings.Unbounded;

with AWS.Config.Set;
with AWS.Server;

with SOAP.Dispatchers.Callback;
with SOAP.Types;

with WSDL_8;
with WSDL_8_Service.CB;
with WSDL_8_Service.Client;

procedure WSDL_8_Main is

   use Ada;
   use Ada.Strings.Unbounded;
   use AWS;
   use SOAP.Types;

   WS   : Server.HTTP;

   H    : WSDL_8_Service.CB.Handler;

   Conf : Config.Object := Config.Get_Current;

   F    : WSDL_8.Set_Of_Files (1 .. 2);

begin
   H := SOAP.Dispatchers.Callback.Create
     (null, WSDL_8_Service.CB.SOAP_CB'Access);

   Config.Set.Server_Host (Conf, "localhost");
   Config.Set.Server_Port (Conf, 7708);

   Server.Start (WS, H, Conf);

   F := (1 => (To_Unbounded_String ("first_file"),
               To_Unbounded_String ("this is the content of file 1")),
         2 => (To_Unbounded_String ("second_file"),
               To_Unbounded_String ("this is the content of file 2")));

   WSDL_8_Service.Client.Proc ("File_Test", F);

   Server.Shutdown (WS);
end WSDL_8_Main;
