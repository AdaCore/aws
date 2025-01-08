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

with AWS.Config.Set;
with AWS.Server.Status;
with SOAP.Dispatchers.Callback;

with Nested_Types_Server;
with Data_Service.Client;
with Data_Service.Types;

with myCompany.myDataService.myDataService_Type_Pkg;
with myCompany.myDataService.myIncludedData_Type_Pkg;
with Nested_Types;

procedure Nested_Types_Main is

   use Ada;
   use Ada.Strings.Unbounded;
   use AWS;

   use myCompany.myDataService;
   use Data_Service.Types;

   WS   : Server.HTTP;
   H    : Nested_Types_Server.Handler;
   Conf : Config.Object := Config.Get_Current;

   procedure Call
     (Msg : String;
      P   : Data_Service.Types.myDataService_Type)
   is
      R : Data_Service.Types.Store_Result;
   begin
      Text_IO.Put_Line ("====== " & Msg);

      R := Data_Service.Client.Store (P, Endpoint => Server.Status.Local_URL (WS));

      Nested_Types.Dump (R);
   end Call;

begin
   H := SOAP.Dispatchers.Callback.Create
     (Nested_Types_Server.HTTP_CB'Access,
      Nested_Types_Server.SOAP_CB'Access,
      Schema => Data_Service.Schema);

   Config.Set.Server_Host (Conf, "localhost");
   Config.Set.Server_Port (Conf, 0);

   Server.Start (WS, H, Conf);

   Call ("Store C1",
         myDataService_Type'
           (C        => myDataService_Type_Pkg.C1,
            myOutput => 12));

   Call ("Store C2-C1",
         myDataService_Type'
           (C              => myDataService_Type_Pkg.C2,
            myIncludedData => myIncludedData_Type'
              (C         => myIncludedData_Type_Pkg.C1,
               intdInput => 6)));

   Call ("Store C2-C2",
         myDataService_Type'
           (C              => myDataService_Type_Pkg.C2,
            myIncludedData => myIncludedData_Type'
              (C          => myIncludedData_Type_Pkg.C2,
               intdOutput => 9)));

   Text_IO.Put_Line ("Run OK");

   Server.Shutdown (WS);
end Nested_Types_Main;
