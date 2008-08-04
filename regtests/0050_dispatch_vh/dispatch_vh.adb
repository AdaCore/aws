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

with Ada.Text_IO;

with AWS.Client;
with AWS.Config.Set;
with AWS.Dispatchers.Callback;
with AWS.Server;
with AWS.Services.Dispatchers.Virtual_Host;
with AWS.Status;
with AWS.Response;
with AWS.Utils;

with Get_Free_Port;

procedure Dispatch_VH is

   use Ada;
   use AWS;
   use AWS.Services;

   Cfg       : Config.Object;
   Free_Port : Positive;

   function CB1
     (Request : in AWS.Status.Data)
      return AWS.Response.Data
   is
      pragma Unreferenced (Request);
   begin
      return AWS.Response.Build ("text/html", "Dispatch 1 !");
   end CB1;

   function CB2
     (Request : in AWS.Status.Data)
      return AWS.Response.Data
   is
      pragma Unreferenced (Request);
   begin
      return AWS.Response.Build ("text/html", "Dispatch 2 !");
   end CB2;

   procedure Test (URL : in String) is
      R : Response.Data;
   begin
      Text_IO.Put_Line (URL (URL'First .. URL'First + 15));
      R := Client.Get (URL);
      Text_IO.Put_Line ("> " & Response.Message_Body (R));
   end Test;

   H  : AWS.Services.Dispatchers.Virtual_Host.Handler;

   WS : AWS.Server.HTTP;

begin
   Services.Dispatchers.Virtual_Host.Register
     (H, "localhost", CB1'Unrestricted_Access);

   Services.Dispatchers.Virtual_Host.Register
     (H, "127.0.0.1", CB2'Unrestricted_Access);

   Cfg := AWS.Config.Get_Current;

   Free_Port := AWS.Config.Server_Port (Cfg);
   Get_Free_Port (Free_Port);
   AWS.Config.Set.Server_Port (Cfg, Free_Port);

   AWS.Server.Start
     (WS,
      Dispatcher => H,
      Config     => Cfg);

   Test ("http://localhost:" & Utils.Image (Free_Port) & "/thisone");
   Test ("http://127.0.0.1:" & Utils.Image (Free_Port) & "/thisone");

   --  Close servers

   AWS.Server.Shutdown (WS);
end Dispatch_VH;
