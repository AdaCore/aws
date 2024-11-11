------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2022-2024, AdaCore                     --
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

with Array_Rec_Int_Data_Service.CB;
with Array_Rec_Int_Data_Service.Client;
with Array_Rec_Int_Data_Service.Server;
with Array_Rec_Int_Data_Service.Types;

with Array_Rec_Int_Types;

procedure Array_Rec_Int_Main is

   use Ada;
   use Array_Rec_Int_Data_Service.Client;
   use Array_Rec_Int_Data_Service.Types;

   use AWS;

   function CB (Request : Status.Data) return Response.Data is
      R : Response.Data;
   begin
      return R;
   end CB;

   WS   : AWS.Server.HTTP;
   Conf : Config.Object;
   Disp : Array_Rec_Int_Data_Service.CB.Handler;

   AI : Integer_List_Type_Type;
   AM : My_Int_List_Type_Type;

begin
   Config.Set.Server_Port (Conf, 0);
   Config.Set.Server_Host (Conf, "localhost");

   Disp := SOAP.Dispatchers.Callback.Create
     (CB'Unrestricted_Access,
      Array_Rec_Int_Data_Service.CB.SOAP_CB'Access,
      Array_Rec_Int_Data_Service.Schema);

   AWS.Server.Start (WS, Disp, Conf);

   AI.Append (1);
   AI.Append (2);

   AM.Append (11);
   AM.Append (12);

   declare
      L : Array_Integer_Type_Type := (Value1 => AI, Value2 => AM);
   begin
      Test_Array (L, Endpoint => Server.Status.Local_URL (WS));
   end;

   AWS.Server.Shutdown (WS);
end Array_Rec_Int_Main;
