------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2007-2013, AdaCore                     --
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

with AWS.Client;
with AWS.Config.Set;
with AWS.Dispatchers.Callback;
with AWS.Response;
with AWS.Server.Status;
with AWS.Services.Dispatchers.URI;
with AWS.Status;
with AWS.Utils;

procedure Dispatch_With_Unregister is

   use Ada;
   use AWS;
   use AWS.Services;

   WS  : AWS.Server.HTTP;
   Cfg : Config.Object;

   ---------
   -- CB1 --
   ---------

   function CB1
     (Request : AWS.Status.Data) return AWS.Response.Data
   is
      pragma Unreferenced (Request);
   begin
      return AWS.Response.Build ("text/html", "Dispatch 1 !");
   end CB1;

   ---------
   -- CB2 --
   ---------

   function CB2
     (Request : AWS.Status.Data) return AWS.Response.Data
   is
      pragma Unreferenced (Request);
   begin
      return AWS.Response.Build ("text/html", "Dispatch 2 !");
   end CB2;

   --------------
   -- Default1 --
   --------------

   function Default1
     (Request : AWS.Status.Data) return AWS.Response.Data
   is
      URI : constant String := Status.URI (Request);
   begin
      return AWS.Response.Build ("text/html", "Default1 " & URI);
   end Default1;

   --------------
   -- Default2 --
   --------------

   function Default2
     (Request : AWS.Status.Data) return AWS.Response.Data
   is
      URI : constant String := Status.URI (Request);
   begin
      return AWS.Response.Build ("text/html", "Default2 " & URI);
   end Default2;

   ----------
   -- Test --
   ----------

   procedure Test (URI : String) is
      R : Response.Data;
   begin
      Text_IO.Put_Line (URI);
      R := Client.Get (Server.Status.Local_URL (WS) & URI);
      Text_IO.Put_Line ("> " & Response.Message_Body (R));
   end Test;

   H      : AWS.Services.Dispatchers.URI.Handler;
   H_Temp : AWS.Services.Dispatchers.URI.Handler;

begin
   Services.Dispatchers.URI.Register
     (H, "/disp1", AWS.Dispatchers.Callback.Create (CB1'Unrestricted_Access));

   Services.Dispatchers.URI.Register
     (H, "/disp2", CB2'Unrestricted_Access);

   Services.Dispatchers.URI.Register_Default_Callback
     (H, AWS.Dispatchers.Callback.Create (Default1'Unrestricted_Access));

   Services.Dispatchers.URI.Register_Default_Callback
     (H_Temp, AWS.Dispatchers.Callback.Create (Default2'Unrestricted_Access));

   AWS.Config.Set.Server_Port (Cfg, 0);

   AWS.Server.Start (WS, Dispatcher => H, Config => Cfg);

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
