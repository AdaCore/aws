------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2007-2008, AdaCore                     --
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
with AWS.Services.Dispatchers.URI;
with AWS.Status;
with AWS.Response;
with AWS.Utils;

with Get_Free_Port;

procedure Dispatch_With_Unregister is

   use Ada;
   use AWS;
   use AWS.Services;

   Cfg       : Config.Object;
   Free_Port : Positive := 1245;

   ---------
   -- CB1 --
   ---------

   function CB1
     (Request : in AWS.Status.Data)
      return AWS.Response.Data
   is
      pragma Unreferenced (Request);
   begin
      return AWS.Response.Build ("text/html", "Dispatch 1 !");
   end CB1;

   ---------
   -- CB2 --
   ---------

   function CB2
     (Request : in AWS.Status.Data)
      return AWS.Response.Data
   is
      pragma Unreferenced (Request);
   begin
      return AWS.Response.Build ("text/html", "Dispatch 2 !");
   end CB2;

   --------------
   -- Default1 --
   --------------

   function Default1
     (Request : in AWS.Status.Data)
      return AWS.Response.Data
   is
      URI : constant String := Status.URI (Request);
   begin
      return AWS.Response.Build ("text/html", "Default1 " & URI);
   end Default1;

   --------------
   -- Default2 --
   --------------

   function Default2
     (Request : in AWS.Status.Data)
      return AWS.Response.Data
   is
      URI : constant String := Status.URI (Request);
   begin
      return AWS.Response.Build ("text/html", "Default2 " & URI);
   end Default2;

   ----------
   -- Test --
   ----------

   procedure Test (URI : in String) is
      R : Response.Data;
   begin
      Text_IO.Put_Line (URI);
      R := Client.Get
        ("http://localhost:" & AWS.Utils.Image (Free_Port) & URI);
      Text_IO.Put_Line ("> " & Response.Message_Body (R));
   end Test;

   H      : AWS.Services.Dispatchers.URI.Handler;
   H_Temp : AWS.Services.Dispatchers.URI.Handler;

   WS     : AWS.Server.HTTP;

begin
   Services.Dispatchers.URI.Register
     (H, "/disp1", AWS.Dispatchers.Callback.Create (CB1'Unrestricted_Access));

   Services.Dispatchers.URI.Register
     (H, "/disp2", CB2'Unrestricted_Access);

   Services.Dispatchers.URI.Register_Default_Callback
     (H, AWS.Dispatchers.Callback.Create (Default1'Unrestricted_Access));

   Services.Dispatchers.URI.Register_Default_Callback
     (H_Temp, AWS.Dispatchers.Callback.Create (Default2'Unrestricted_Access));

   Cfg := AWS.Config.Get_Current;

   Get_Free_Port (Free_Port);
   AWS.Config.Set.Server_Port (Cfg, Free_Port);

   AWS.Server.Start
     (WS,
      Dispatcher => H,
      Config     => Cfg);

   Test ("/disp1");

   Services.Dispatchers.URI.Unregister (H, "/disp1");
   Services.Dispatchers.URI.Unregister (H, "/disp2");

   Test ("/disp1");

   AWS.Server.Set (WS, H);

   Test ("/disp1");
   Test ("/disp2");
   Test ("/unknown");

   AWS.Server.Set (WS, H_Temp);

   Test ("/disp1");
   Test ("/unknown");

   --  Close servers

   AWS.Server.Shutdown (WS);
end Dispatch_With_Unregister;
