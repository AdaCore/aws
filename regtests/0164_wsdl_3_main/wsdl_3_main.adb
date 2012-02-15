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

with WSDL_3;
with WSDL_3_Server;
with WSDL_3_Service.Client;
with WSDL_3_Service.Types;

procedure WSDL_3_Main is

   use Ada;
   use Ada.Strings.Unbounded;
   use AWS;

   WS   : Server.HTTP;

   H    : WSDL_3_Server.Handler;

   Conf : Config.Object := Config.Get_Current;

   R1   : WSDL_3_Service.Types.Rec1_Type;
   R2   : WSDL_3_Service.Types.Rec2_Type;
   R3   : WSDL_3_Service.Types.Rec3_Type;

begin
   H := SOAP.Dispatchers.Callback.Create
     (WSDL_3_Server.HTTP_CB'Access, WSDL_3_Server.SOAP_CB'Access);

   Config.Set.Server_Host (Conf, "localhost");
   Config.Set.Server_Port (Conf, 7703);

   Server.Start (WS, H, Conf);

   R1 := (-4, 2, 89);
   R2 := (R1, 'c', To_Unbounded_String ("toto"), 1.45);
   R3 := (S => WSDL_3.My_Set_Safe_Pointer.To_Safe_Pointer
            (WSDL_3.My_Set'(1, 7, 8, 10, 0, 0, 3)));

   Text_IO.Put_Line ("R1 = " & WSDL_3_Service.Client.Image_Rec1 (R1));
   Text_IO.Put_Line ("R2 = " & WSDL_3_Service.Client.Image_Rec2 (R2));
   Text_IO.Put_Line ("R3 = " & WSDL_3_Service.Client.Image_Rec3 (R3));

   Server.Shutdown (WS);
end WSDL_3_Main;
