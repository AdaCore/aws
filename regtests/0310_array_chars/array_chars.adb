------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                      Copyright (C) 2017, AdaCore                         --
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

with AWS.Config.Set;
with AWS.MIME;
with AWS.Response;
with AWS.Server;
with AWS.Status;
with SOAP.Dispatchers.Callback;

with Character_Services.Test;
with Character_Services_Service.CB;

procedure Array_Chars is

   use AWS;

   use Character_Services;

   CNF  : Config.Object;
   Disp : Character_Services_Service.CB.Handler;

   --------
   -- CB --
   --------

   function CB (Request : Status.Data) return Response.Data is
      SOAPAction : constant String := Status.SOAPAction (Request);
   begin
      return Response.Build (MIME.Text_HTML, "<p>Not a SOAP request");
   end CB;

begin
   Config.Set.Server_Name (CNF, "WSDL Routine Schema");
   Config.Set.Server_Host (CNF, "localhost");
   Config.Set.Server_Port (CNF, 0);

   Disp := SOAP.Dispatchers.Callback.Create
     (CB'Unrestricted_Access,
      Character_Services_Service.CB.SOAP_CB'Access,
      Character_Services_Service.Schema);

   Server.Start (Test.H_Server, Disp, CNF);

   Test.Test_Record;
   Test.Test_Parameter;
   Test.Test_Array_Record;
   Test.Test_Array_Parameter;
   Test.Test_Array_Record_Record;
   Test.Test_Array_Record_Parameter;

   Server.Shutdown (Test.H_Server);
end Array_Chars;
