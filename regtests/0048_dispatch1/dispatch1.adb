------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2003-2009, AdaCore                     --
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

procedure Dispatch1 is

   use Ada;
   use AWS;
   use AWS.Services;

   Cfg       : Config.Object;
   Free_Port : Positive;

   function CB1
     (Request : AWS.Status.Data)
      return AWS.Response.Data
   is
      pragma Unreferenced (Request);
   begin
      return AWS.Response.Build ("text/html", "Dispatch 1 !");
   end CB1;

   function CB2
     (Request : AWS.Status.Data)
      return AWS.Response.Data
   is
      pragma Unreferenced (Request);
   begin
      return AWS.Response.Build ("text/html", "Dispatch 2 !");
   end CB2;

   function CB3
     (Request : AWS.Status.Data)
      return AWS.Response.Data
   is
      pragma Unreferenced (Request);
   begin
      return AWS.Response.Build ("text/html", "Dispatch 3 !");
   end CB3;

   function CB4
     (Request : AWS.Status.Data)
      return AWS.Response.Data
   is
      pragma Unreferenced (Request);
   begin
      return AWS.Response.Build ("text/html", "Dispatch 4 !");
   end CB4;

   function CB5
     (Request : AWS.Status.Data)
      return AWS.Response.Data
   is
      pragma Unreferenced (Request);
   begin
      return AWS.Response.Build ("text/html", "Dispatch 5 !");
   end CB5;

   function Default
     (Request : AWS.Status.Data)
      return AWS.Response.Data
   is
      URI : constant String := Status.URI (Request);
   begin
      return AWS.Response.Build ("text/html", "Default " & URI);
   end Default;

   procedure Test (URI : String) is
      R : Response.Data;
   begin
      Text_IO.Put_Line (URI);
      R := Client.Get
        ("http://localhost:" & AWS.Utils.Image (Free_Port) & URI);
      Text_IO.Put_Line ("> " & Response.Message_Body (R));
   end Test;

   H  : AWS.Services.Dispatchers.URI.Handler;

   WS : AWS.Server.HTTP;

begin
   Services.Dispatchers.URI.Register
     (H, "/disp", CB1'Unrestricted_Access);

   Services.Dispatchers.URI.Register
     (H, "/prefix/", CB5'Unrestricted_Access, Prefix => True);

   Services.Dispatchers.URI.Register_Regexp
     (H, "/disp.*", CB4'Unrestricted_Access);

   Services.Dispatchers.URI.Register
     (H, "/thisone", CB2'Unrestricted_Access);

   Services.Dispatchers.URI.Register_Regexp
     (H, ".*abc.*", CB3'Unrestricted_Access);

   Services.Dispatchers.URI.Register_Default_Callback
     (H, AWS.Dispatchers.Callback.Create (Default'Unrestricted_Access));

   Cfg := AWS.Config.Get_Current;

   Free_Port := AWS.Config.Server_Port (Cfg);
   Get_Free_Port (Free_Port);
   AWS.Config.Set.Server_Port (Cfg, Free_Port);

   AWS.Server.Start
     (WS,
      Dispatcher => H,
      Config     => Cfg);

   Test ("/thisone");
   Test ("/disp");
   Test ("/disp_xyz");
   Test ("/dispabc");
   Test ("/dis_abc_x");
   Test ("/abc");
   Test ("/abcdisp");
   Test ("/notknown");
   Test ("/prefix/");
   Test ("/prefix/toto");
   Test ("/prefix/azerty/tutu");

   --  Close servers

   AWS.Server.Shutdown (WS);
end Dispatch1;
