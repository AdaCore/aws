------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                       Copyright (C) 2015, AdaCore                        --
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

with Ada.Exceptions;
with Ada.Text_IO;

with AWS.Client;
with AWS.Config.Set;
with AWS.Response;
with AWS.Server.Status;
with AWS.Status;
with AWS.Utils;

with SOAP.Client;
with SOAP.Dispatchers.Callback;
with SOAP.Message.Payload;
with SOAP.Message.Response;
with SOAP.Name_Space;
with SOAP.Parameters;
with SOAP.Types;

with Pck.Serv1_Service.Client;
with Pck.Serv1_Service.Types;

with Pck.Serv2_Service.Client;
with Pck.Serv2_Service.Types;

with Agg_Server_CB;

procedure Mwsdl is
   use Ada;
   use Ada.Exceptions;

   use AWS;
   use SOAP;

   function CB (Request : Status.Data) return Response.Data is
      R : Response.Data;
   begin
      Text_IO.Put_Line ("Should never be called!");
      return R;
   end CB;

   WS   : Server.HTTP;
   Conf : Config.Object;
   Disp : Agg_Server_CB.Handler;

begin
   Config.Set.Server_Port (Conf, 0);

   Disp := Agg_Server_CB.Create (CB'Unrestricted_Access);

   AWS.Server.Start (WS, Disp, Conf);

   begin
      Pck.Serv1_Service.Client.Call
        (12, Endpoint => AWS.Server.Status.Local_URL (WS));
   exception
      when E : others =>
         Text_IO.Put_Line (Exception_Message (E));
   end;

   begin
      Pck.Serv2_Service.Client.Call
        (93, Endpoint => AWS.Server.Status.Local_URL (WS));
   exception
      when E : others =>
         Text_IO.Put_Line (Exception_Message (E));
   end;

   AWS.Server.Shutdown (WS);
end Mwsdl;
