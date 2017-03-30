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

with WSDL_Enum_Array_Data_Service.CB;
with WSDL_Enum_Array_Data_Service.Client;
with WSDL_Enum_Array_Data_Service.Server;
with WSDL_Enum_Array_Data_Service.Types;

with WSDL_Enum_Array_Types;

procedure WSDL_Enum_Array_Main is

   use Ada;
   use WSDL_Enum_Array_Data_Service.Client;
   use WSDL_Enum_Array_Data_Service.Types;

   package WCRT renames WSDL_Enum_Array_Types;

   use AWS;

   function CB (Request : Status.Data) return Response.Data is
      R : Response.Data;
   begin
      return R;
   end CB;

   WS   : AWS.Server.HTTP;
   Conf : Config.Object;
   Disp : WSDL_Enum_Array_Data_Service.CB.Handler;

begin
   Config.Set.Server_Port (Conf, 0);

   Disp := SOAP.Dispatchers.Callback.Create
     (CB'Unrestricted_Access,
      WSDL_Enum_Array_Data_Service.CB.SOAP_CB'Access,
      WSDL_Enum_Array_Data_Service.Schema);

   AWS.Server.Start (WS, Disp, Conf);

   declare
      O    : constant WCRT.List_Type := (WCRT.First, WCRT.Third);
      List : constant WCRT.List_Type_Safe_Pointer.Safe_Pointer :=
               WCRT.List_Type_Safe_Pointer.To_Safe_Pointer (O);
      L : WCRT.Record_Type :=
            (Value => 2,
             C     => 'C',
             L     => List);
   begin
      L := Test_1 (L, Endpoint => Server.Status.Local_URL (WS));
      Text_IO.Put_Line (Integer'Image (L.Value));
      Text_IO.Put_Line (String'(1 => L.C));
      Text_IO.Put_Line (WCRT.Num'Image (L.L.Item (1)));
      Text_IO.Put_Line (WCRT.Num'Image (L.L.Item (2)));
   end;

   AWS.Server.Shutdown (WS);
end WSDL_Enum_Array_Main;
