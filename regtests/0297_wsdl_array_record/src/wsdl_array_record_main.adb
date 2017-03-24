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

with WSDL_Array_Record_Data_Service.CB;
with WSDL_Array_Record_Data_Service.Client;
with WSDL_Array_Record_Data_Service.Server;
with WSDL_Array_Record_Data_Service.Types;

with WSDL_Array_Record_Types;

procedure WSDL_Array_Record_Main is

   use Ada;
   use WSDL_Array_Record_Data_Service.Client;
   use WSDL_Array_Record_Data_Service.Types;

   package WART renames WSDL_Array_Record_Types;

   use AWS;

   function CB (Request : Status.Data) return Response.Data is
      R : Response.Data;
   begin
      return R;
   end CB;

   WS   : AWS.Server.HTTP;
   Conf : Config.Object;
   Disp : WSDL_Array_Record_Data_Service.CB.Handler;

begin
   Config.Set.Server_Port (Conf, 0);

   Disp := SOAP.Dispatchers.Callback.Create
     (CB'Unrestricted_Access,
      WSDL_Array_Record_Data_Service.CB.SOAP_CB'Access,
      WSDL_Array_Record_Data_Service.Schema);

   AWS.Server.Start (WS, Disp, Conf);

   declare
      R : WART.Enumeration_Record_Type;
      E : WART.Enumeration_Type;

      L : WART.Sub_Record_List_Type (1 .. 2) :=
            (1 => (Value => WART.Third),
             2 => (Value => WART.First));
   begin
      R.Value := WART.First;
      R.List  := WART.Sub_Record_List_Type_Safe_Pointer.To_Safe_Pointer (L);
      E := Test_1 (R, Endpoint => Server.Status.Local_URL (WS));
      Text_IO.Put_Line (WART.Enumeration_Type'Image (E));
   end;

   declare
      E : constant WART.Enumeration_Type := WART.Second;
      R : WART.Enumeration_Record_Type;
   begin
      R := Test_2 (E, Endpoint => Server.Status.Local_URL (WS));
      Text_IO.Put_Line (WART.Enumeration_Type'Image (R.Value));
      Text_IO.Put_Line (WART.Enumeration_Type'Image (R.List.Item (1).Value));
      Text_IO.Put_Line (WART.Enumeration_Type'Image (R.List.Item (2).Value));
   end;

   AWS.Server.Shutdown (WS);
end WSDL_Array_Record_Main;
