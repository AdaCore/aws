------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                       Copyright (C) 2024, AdaCore                        --
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
with SOAP.Utils;

with WSDL_D;

with WSDL_D_Server;
with WSDL_D_Service.Client;
with WSDL_D_Service.Server;

procedure WSDL_D_Main is

   use Ada;
   use AWS;
   use type SOAP.Types.Decimal;

   WS   : Server.HTTP;

   H    : WSDL_D_Server.Handler;

   Conf : Config.Object := Config.Get_Current;
   D    : SOAP.Types.Decimal := 0.0;
   P    : WSDL_D.Price := 12.99;

   Step : constant := (if Standard'Max_Integer_Size >= 128
                       then 0.000001
                       else 0.0001);

begin
   H := SOAP.Dispatchers.Callback.Create
     (WSDL_D_Server.HTTP_CB'Access,
      WSDL_D_Server.SOAP_CB'Access,
      Schema => WSDL_D_Service.Schema);

   Config.Set.Server_Host (Conf, "localhost");
   Config.Set.Server_Port (Conf, WSDL_D_Service.Server.Port);

   Server.Start (WS, H, Conf);

   WSDL_D_Service.Client.Print (0.0);
   WSDL_D_Service.Client.Print (12.0);
   WSDL_D_Service.Client.Print (12.1);
   WSDL_D_Service.Client.Print (12.2);
   WSDL_D_Service.Client.Print (12.35);
   WSDL_D_Service.Client.Print (98712.0);
   Text_IO.Put_Line (WSDL_D_Service.Client.Image (789.0));
   Text_IO.Put_Line (WSDL_D_Service.Client.Image (-2.0));
   WSDL_D_Service.Client.Call (WSDL_D.R'(D => 88.0, I => 1));

   WSDL_D_Service.Client.Print_Price (P);

   for J in 1 .. 15 loop
      D := (D + Step * J) * 10;
      if D /= SOAP.Types.XSD_Decimal'(SOAP.Types.D (D)).V then
         Text_IO.Put_Line ("Wrong decimal " & D'Img & " convertion");
      end if;
      WSDL_D_Service.Client.Print (D);
   end loop;

   begin
      WSDL_D_Service.Client.Print_Price (1235.0);
   exception
      when Constraint_Error =>
         Text_IO.Put_Line ("Expected constaint error");
   end;

   Server.Shutdown (WS);
end WSDL_D_Main;
