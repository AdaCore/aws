------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2000-2001                          --
--                                ACT-Europe                                --
--                                                                          --
--  Authors: Dmitriy Anisimkov - Pascal Obry                                --
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

--  $Id$

--  This program test the Virtual Host service.
--
--  To use this demo change "pascal" by your hostname. Then using
--  http://localhost:8080 and http://your.hostname:8080 will redirect to two
--  differents callback.

with Ada.Text_IO;

with AWS.Config;
with AWS.Response;
with AWS.Server;
with AWS.Status;
with AWS.Default;
with AWS.Parameters;
with AWS.Services.Dispatchers.Callback;
with AWS.Services.Dispatchers.Virtual_Host;

procedure VH_Demo is

   WS : AWS.Server.HTTP;
   VH : AWS.Services.Dispatchers.Virtual_Host.Handler;

   -----------
   -- H1_CB --
   -----------

   function H1_CB
     (Request : in AWS.Status.Data)
     return AWS.Response.Data
   is
      P : AWS.Parameters.List := AWS.Status.Parameters (Request);
   begin
      return AWS.Response.Build
        ("text/html",
         "<p>This is server 1"
         & "<br>" & AWS.Parameters.Get (P, "PARAM"));
   end H1_CB;

   -----------
   -- H2_CB --
   -----------

   function H2_CB
     (Request : in AWS.Status.Data)
     return AWS.Response.Data
   is
      P : AWS.Parameters.List := AWS.Status.Parameters (Request);
   begin
      return AWS.Response.Build
        ("text/html",
         "<p>This is server 2"
         & "<br>" & AWS.Parameters.Get (P, "PARAM"));
   end H2_CB;

begin
   Ada.Text_IO.Put_Line
     ("Call me on port"
      & Positive'Image (AWS.Default.Server_Port)
      & ", I will stop in 60 seconds...");

   AWS.Services.Dispatchers.Virtual_Host.Register
     (VH, "localhost",
      AWS.Services.Dispatchers.Callback.Create (H1_CB'Unrestricted_Access));
   AWS.Services.Dispatchers.Virtual_Host.Register
     (VH, "pascal",
      AWS.Services.Dispatchers.Callback.Create (H2_CB'Unrestricted_Access));

--     AWS.Services.Dispatchers.Virtual_Host.Register
--       (VH, "pascal", "localhost:8080");

   AWS.Server.Start
     (WS,
      Dispatcher => VH,
      Config     => AWS.Config.Default_Config);

   delay 60.0;

   AWS.Server.Shutdown (WS);
end VH_Demo;
