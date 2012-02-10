------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2007-2012, AdaCore                     --
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
with AWS.Server.Status;
with AWS.Services.Dispatchers.URI;
with AWS.Status;
with AWS.Response;
with AWS.Utils;

procedure Change_Dispatch is

   use Ada;
   use AWS;
   use AWS.Services;

   WS  : AWS.Server.HTTP;
   Cfg : Config.Object;

   function CB1
     (Request : AWS.Status.Data)
      return AWS.Response.Data
   is
      pragma Unreferenced (Request);
      H : AWS.Services.Dispatchers.URI.Handler;
   begin
      return AWS.Response.Build ("text/html", "Dispatch 1 !");
   end CB1;

   function CB2
     (Request : AWS.Status.Data)
      return AWS.Response.Data
   is
      pragma Unreferenced (Request);
      H : AWS.Services.Dispatchers.URI.Handler;
   begin
      Services.Dispatchers.URI.Register
        (H, "/thisone", CB1'Unrestricted_Access);
      Server.Set (WS, H);
      return AWS.Response.Build ("text/html", "Dispatch 2 !");
   end CB2;

   function Default
     (Request : AWS.Status.Data)
      return AWS.Response.Data
   is
      URI : constant String := Status.URI (Request);
      H   : AWS.Services.Dispatchers.URI.Handler;
   begin
      Services.Dispatchers.URI.Register
        (H, "/thisone", CB2'Unrestricted_Access);
      Server.Set (WS, H);
      return AWS.Response.Build ("text/html", "Default " & URI);
   end Default;

   procedure Test (URI : String) is
      R : Response.Data;
   begin
      Text_IO.Put_Line (URI);
      R := Client.Get (AWS.Server.Status.Local_URL (WS) & URI);
      Text_IO.Put_Line ("> " & Response.Message_Body (R));
   end Test;

   H  : AWS.Services.Dispatchers.URI.Handler;

begin
   Services.Dispatchers.URI.Register
     (H, "/thisone", Default'Unrestricted_Access);

   AWS.Config.Set.Server_Host (Cfg, "localhost");
   AWS.Config.Set.Server_Port (Cfg, 0);

   AWS.Server.Start (WS, Dispatcher => H, Config => Cfg);

   Test ("/thisone");
   Test ("/thisone");
   Test ("/thisone");

   --  Close servers

   AWS.Server.Shutdown (WS);
end Change_Dispatch;
