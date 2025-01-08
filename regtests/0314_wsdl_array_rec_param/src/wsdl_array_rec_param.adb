------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2018-2025, AdaCore                     --
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
with Ada.Strings.Unbounded;

with AWS.Config.Set;
with AWS.Response;
with AWS.Server.Status;
with AWS.Status;
with SOAP.Dispatchers.Callback;

with AWS_Test_Service.Client;
with AWS_Test_Service.Cb;
with AWS_Test_Service.Server;

procedure WSDL_Array_Rec_Param is

   use Ada.Text_IO;
   use Ada.Strings.Unbounded;
   use AWS;

   function CB (Request : Status.Data) return Response.Data is
      R : Response.Data;
   begin
      return R;
   end CB;

   WS   : AWS.Server.HTTP;
   Conf : Config.Object := Config.Get_Current;
   Disp : AWS_Test_Service.CB.Handler;

begin
   --  Start server

   Config.Set.Server_Port (Conf, 0);
   Config.Set.Server_Host (Conf, "localhost");

   Disp := SOAP.Dispatchers.Callback.Create
     (CB'Unrestricted_access,
      AWS_Test_Service.CB.SOAP_CB'Access,
      AWS_Test_Service.Schema);

   AWS.Server.Start (WS, Disp, Conf);

   --  Some client calls

   Put_Line ("==========");
   Put_Line (AWS_Test_Service.Client.Get_Test_Name
               ((5, 5.0, To_Unbounded_String ("REC")),
                Endpoint => Server.Status.Local_URL (WS)));

   Put_Line ("==========");
   Put_Line (AWS_Test_Service.Client.Get_Test_Name2
               ((1 => ((6, 6.1, To_Unbounded_String ("ARR1")))),
                Endpoint => Server.Status.Local_URL (WS)));

   Put_Line ("==========");
   Put_Line (AWS_Test_Service.Client.Get_Test_Name2
               ((1 => ((6, 6.1, To_Unbounded_String ("ARR2"))),
                 2 => ((7, 7.2, To_Unbounded_String ("ARR3")))),
                Endpoint => Server.Status.Local_URL (WS)));

   AWS.Server.Shutdown (WS);
end WSDL_Array_Rec_Param;
