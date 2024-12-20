------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2023-2024, AdaCore                     --
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

with AWS;
with AWS.Config.Set;
with AWS.Response;
with AWS.Server.Status;
with AWS.Status;
with SOAP.Dispatchers.Callback;

with Unbounded_String_Subtype_Service.CB;
with Unbounded_String_Subtype_Service.Client;
with Unbounded_String_Subtype_Service.Server;
with Unbounded_String_Subtype_Service.Types;

with Unbounded_String_Subtype;

procedure Unbounded_String_Subtype_Main is

   use Ada;
   use Ada.Strings.Unbounded;

   use Unbounded_String_Subtype_Service.Client;
   use Unbounded_String_Subtype_Service.Types;

   use AWS;

   function CB (Request : Status.Data) return Response.Data is
      R : Response.Data;
   begin
      return R;
   end CB;

   WS   : AWS.Server.HTTP;
   Conf : Config.Object;
   Disp : Unbounded_String_Subtype_Service.CB.Handler;

begin
   Config.Set.Server_Port (Conf, 0);
   Config.Set.Server_Host (Conf, "localhost");

   Disp := SOAP.Dispatchers.Callback.Create
     (CB'Unrestricted_Access,
      Unbounded_String_Subtype_Service.CB.SOAP_CB'Access,
      Unbounded_String_Subtype_Service.Schema);

   AWS.Server.Start (WS, Disp, Conf);

   declare
      R : constant Unbounded_String_Subtype.Rec :=
            (N     => 5,
             Key   => To_Unbounded_String ("MyKey"),
             Value => To_Unbounded_String ("MyValue"));
   begin
      Test_Rec (R, Endpoint => Server.Status.Local_URL (WS));
   end;

   AWS.Server.Shutdown (WS);
end Unbounded_String_Subtype_Main;
