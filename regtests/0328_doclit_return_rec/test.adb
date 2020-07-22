------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                      Copyright (C) 2020, AdaCore                         --
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
pragma Elaborate_All (AWS.Config.Set);
with AWS.Server;
pragma Elaborate_All (AWS.Server);
with AWS.Status;
pragma Elaborate_All (AWS.Status);
with AWS.Response;
pragma Elaborate_All (AWS.Response);
with SOAP.Dispatchers.Callback;
pragma Elaborate_All (SOAP.Dispatchers.Callback);

with Test_AWS_Service.CB;
pragma Elaborate_All (Test_AWS_Service.CB);
with Test_AWS_Service.Server;
pragma Elaborate_All (Test_AWS_Service.Server);

with Test_AWS_Service.Client;
with Types_AWS;

procedure Test is

   use Ada.Strings.Unbounded;

   use AWS;

   function CB
      (Request : Status.Data)
       return Response.Data
   is
      R : Response.Data;
   begin
      return R;
   end CB;

   WS   : AWS.Server.HTTP;
   Conf : Config.Object := Config.Get_Current;
   Disp : Test_AWS_Service.CB.Handler;

   S    : constant Types_AWS.Status_Type := Types_AWS.Failure;
   R    : Types_AWS.Rec_Type :=
            (Types_AWS.Success, To_Unbounded_String ("good"));

begin
   Config.Set.Server_Port
      (Conf, Test_AWS_Service.Server.Port);
   Disp := SOAP.Dispatchers.Callback.Create
     (CB'Unrestricted_Access,
      Test_AWS_Service.CB.SOAP_CB'Access,
      Test_AWS_Service.Schema);

   AWS.Server.Start (WS, Disp, Conf);

   --  Some testing

   Test_AWS_Service.Client.Test1 (R);

   Test_AWS_Service.Client.Test2 (S);

   R := Test_AWS_Service.Client.Test3 (9);

   AWS.Server.Shutdown (WS);
end Test;
