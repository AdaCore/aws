------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2003-2012, AdaCore                     --
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

with Ada.Strings.Fixed;
with Ada.Text_IO;

with AWS.Client;
with AWS.Config.Set;
with AWS.Dispatchers.Callback;
with AWS.Net;
with AWS.Server.Status;
with AWS.Services.Dispatchers.Virtual_Host;
with AWS.Status;
with AWS.Response;
with AWS.Utils;

procedure Dispatch_VH is

   use Ada;
   use AWS;
   use AWS.Services;

   Cfg : Config.Object;

   function Remove_Port (Addr : String) return String;

   -----------------
   -- Remove_Port --
   -----------------

   function Remove_Port (Addr : String) return String is
      use Ada.Strings;
      K : constant Natural := Fixed.Index (Addr, ":", Backward);
   begin
      if K = 0 then
         return Addr;
      else
         return Addr (Addr'First .. K - 1);
      end if;
   end Remove_Port;

   ---------
   -- CB1 --
   ---------

   function CB1 (Request : AWS.Status.Data) return AWS.Response.Data is
   begin
      return AWS.Response.Build
        ("text/plain",
         "http://" & Remove_Port (AWS.Status.Host (Request)) & ASCII.LF
         & "> Dispatch 1 !");
   end CB1;

   ---------
   -- CB2 --
   ---------

   function CB2 (Request : AWS.Status.Data) return AWS.Response.Data is
   begin
      return AWS.Response.Build
        ("text/plain",
         "http://" & Remove_Port (AWS.Status.Host (Request)) & ASCII.LF
         & "> Dispatch 2 !");
   end CB2;

   ----------
   -- Test --
   ----------

   procedure Test (URL : String) is
      R : Response.Data;
   begin
      R := Client.Get (URL);
      Text_IO.Put_Line (Response.Message_Body (R));
   end Test;

   H  : Services.Dispatchers.Virtual_Host.Handler;

   WS : Server.HTTP;
   W6 : Server.HTTP;

begin
   Services.Dispatchers.Virtual_Host.Register
     (H, "localhost", CB1'Unrestricted_Access);

   Services.Dispatchers.Virtual_Host.Register
     (H, "127.0.0.1", CB2'Unrestricted_Access);

   Config.Set.Server_Host (Cfg, "localhost");
   Config.Set.Server_Port (Cfg, 0);

   Server.Start (WS, Dispatcher => H, Config => Cfg);

   if Net.IPv6_Available then
      --  Need to start server in opposite protocol family because we do not
      --  know which family would bind localhost.

      if Server.Status.Is_IPv6 (WS) then
         Config.Set.Protocol_Family (Cfg, "FAMILY_INET");
      else
         Config.Set.Protocol_Family (Cfg, "FAMILY_INET6");
      end if;

      Config.Set.Server_Port (Cfg, Server.Status.Port (WS));

      Server.Start (W6, Dispatcher => H, Config => Cfg);
   end if;

   declare
      Port_Img : constant String := Utils.Image (Server.Status.Port (WS));
   begin
      Test ("http://localhost:" & Port_Img & "/thisone");
      Test ("http://127.0.0.1:" & Port_Img & "/thisone");
   end;

   --  Close servers

   Server.Shutdown (WS);

   if Net.IPv6_Available then
      Server.Shutdown (W6);
   end if;
end Dispatch_VH;
