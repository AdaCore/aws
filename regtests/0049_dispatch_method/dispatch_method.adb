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

with Ada.Exceptions;
with Ada.Text_IO;

with AWS.Client;
with AWS.Config.Set;
with AWS.Dispatchers.Callback;
with AWS.Server;
with AWS.Services.Dispatchers.Method;
with AWS.Status;
with AWS.Response;
with AWS.Utils;

with Get_Free_Port;

procedure Dispatch_Method is

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
      --  Head response, we will not see the response, it takes 23 bytes
      return AWS.Response.Build ("text/html", "Dispatch 2 ! (23 chars)");
   end CB2;

   function CB3
     (Request : AWS.Status.Data)
      return AWS.Response.Data
   is
      pragma Unreferenced (Request);
   begin
      return AWS.Response.Build ("text/html", "Dispatch 3 !");
   end CB3;

   H  : AWS.Services.Dispatchers.Method.Handler;

   WS : AWS.Server.HTTP;

   R : Response.Data;

begin
   Services.Dispatchers.Method.Register
     (H, Status.GET, CB1'Unrestricted_Access);

   Services.Dispatchers.Method.Register
     (H, Status.HEAD, CB2'Unrestricted_Access);

   Services.Dispatchers.Method.Register
     (H, Status.POST, CB3'Unrestricted_Access);

   Free_Port := AWS.Config.Server_Port (Cfg);
   Get_Free_Port (Free_Port);
   AWS.Config.Set.Server_Port (Cfg, Free_Port);

   AWS.Server.Start
     (WS,
      Dispatcher => H,
      Config     => Cfg);

   declare
      URL : constant String
        := "http://localhost:" & Utils.Image (Free_Port) & "/test";
   begin
      R := Client.Get (URL);
      Text_IO.Put_Line ("> " & Response.Message_Body (R));

      R := Client.Post (URL, "go");
      Text_IO.Put_Line ("> " & Response.Message_Body (R));

      R := Client.Head (URL);
      Text_IO.Put_Line ("> " & Response.Message_Body (R));
      Text_IO.Put_Line ("> " & Integer'Image (Response.Content_Length (R)));
   end;

   --  Close servers

   AWS.Server.Shutdown (WS);

exception
   when E : others =>
      Text_IO.Put_Line
        ("Main task " & Ada.Exceptions.Exception_Information (E));
end Dispatch_Method;
