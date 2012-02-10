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
with AWS.Server.Status;
with AWS.Services.Dispatchers.Virtual_Host;
with AWS.Status;
with AWS.Response;
with AWS.Utils;

procedure Dispatch_VH is

   use Ada;
   use AWS;
   use AWS.Services;

   Cfg       : Config.Object;
   Free_Port : Positive;

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

      function Pretend_IPv4 (Addr : String) return String;
      --  This callback would catche either IPv4 or IPv6 numeric local address.
      --  Wee need to make stable output just renaming IPv6 numeric address
      --  into IPv4.

      function Pretend_IPv4 (Addr : String) return String is
      begin
         if Addr = "[::1]" then
            return "127.0.0.1";
         else
            return Addr;
         end if;
      end Pretend_IPv4;

   begin
      return AWS.Response.Build
        ("text/plain",
         "http://" & Pretend_IPv4 (Remove_Port (AWS.Status.Host (Request)))
         & ASCII.LF & "> Dispatch 2 !");
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

   H  : AWS.Services.Dispatchers.Virtual_Host.Handler;

   WS : AWS.Server.HTTP;

begin
   Services.Dispatchers.Virtual_Host.Register
     (H, "localhost", CB1'Unrestricted_Access);

   Services.Dispatchers.Virtual_Host.Register
     (H, "127.0.0.1", CB2'Unrestricted_Access);

   Services.Dispatchers.Virtual_Host.Register
     (H, "[::1]", CB2'Unrestricted_Access);

   AWS.Config.Set.Server_Host (Cfg, "localhost");
   AWS.Config.Set.Server_Port (Cfg, 0);

   AWS.Server.Start (WS, Dispatcher => H, Config => Cfg);

   Test ("http://localhost:" & Utils.Image (Server.Status.Port (WS))
         & "/thisone");
   Test (Server.Status.Local_URL (WS) & "/thisone");

   --  Close servers

   AWS.Server.Shutdown (WS);
end Dispatch_VH;
