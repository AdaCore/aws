------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2003-2008, AdaCore                     --
--                                                                          --
--  This library is free software; you can redistribute it and/or modify    --
--  it under the terms of the GNU General Public License as published by    --
--  the Free Software Foundation; either version 2 of the License, or (at   --
--  your option) any later version.                                         --
--                                                                          --
--  This library is distributed in the hope that it will be useful, but     --
--  WITHOUT ANY WARRANTY; without even the implied warranty of              --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU       --
--  General Public License for more details.                                --
--                                                                          --
--  You should have received a copy of the GNU General Public License       --
--  along with this library; if not, write to the Free Software Foundation, --
--  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.          --
--                                                                          --
--  As a special exception, if other files instantiate generics from this   --
--  unit, or you link this unit with other files to produce an executable,  --
--  this  unit  does not  by itself cause  the resulting executable to be   --
--  covered by the GNU General Public License. This exception does not      --
--  however invalidate any other reasons why the executable file  might be  --
--  covered by the  GNU Public License.                                     --
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
