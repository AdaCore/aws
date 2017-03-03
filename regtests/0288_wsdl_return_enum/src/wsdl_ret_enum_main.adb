------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                       Copyright (C) 2017, AdaCore                        --
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

with AWS;
with AWS.Config.Set;
with AWS.Response;
with AWS.Server.Status;
with AWS.Status;
with SOAP.Dispatchers.Callback;

with WSDL_Ret_Enum_Data_Service.CB;
with WSDL_Ret_Enum_Data_Service.Client;
with WSDL_Ret_Enum_Data_Service.Server;
with WSDL_Ret_Enum_Data_Service.Types;

procedure WSDL_Ret_Enum_Main is

   use Ada.Text_IO;
   use WSDL_Ret_Enum_Data_Service.Client;
   use WSDL_Ret_Enum_Data_Service.Types;

   use AWS;

   function CB (Request : Status.Data) return Response.Data is
      R : Response.Data;
   begin
      return R;
   end CB;

   WS   : AWS.Server.HTTP;
   Conf : Config.Object;
   Disp : WSDL_Ret_Enum_Data_Service.CB.Handler;

   V : Data_Type;
begin
   Config.Set.Server_Port (Conf, 0);

   Disp := SOAP.Dispatchers.Callback.Create
     (CB'Unrestricted_Access,
      WSDL_Ret_Enum_Data_Service.CB.SOAP_CB'Access,
      WSDL_Ret_Enum_Data_Service.Schema);

   AWS.Server.Start (WS, Disp, Conf);

   V.Identity := "My id     ";
   V.Name     := "My name                                 ";

   declare
      Result : Return_Type_Type;
   begin
      Result := Update (V, Endpoint => AWS.Server.Status.Local_URL (WS));
      Put_Line ("Return value: " & Return_Type_Type'Image (Result));
   end;

   AWS.Server.Shutdown (WS);
end WSDL_Ret_Enum_Main;
