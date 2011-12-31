------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2000-2012, AdaCore                     --
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

--  This program test the Virtual Host service.
--
--  To use this demo change "pascal" by your hostname. Then using
--  http://localhost:8080 and http://your.hostname:8080 will redirect to two
--  differents callback.

with Ada.Text_IO;

with AWS.Config;
with AWS.Default;
with AWS.Server;
with AWS.Services.Dispatchers.Virtual_Host;

with VH_Demo_CB;

procedure VH_Demo is

   WS : AWS.Server.HTTP;
   VH : AWS.Services.Dispatchers.Virtual_Host.Handler;

begin
   Ada.Text_IO.Put_Line
     ("Call me on port"
      & Positive'Image (AWS.Default.Server_Port)
      & ", I will stop in 60 seconds...");

   AWS.Services.Dispatchers.Virtual_Host.Register
     (VH, "localhost", VH_Demo_CB.H1_CB'Access);

   AWS.Services.Dispatchers.Virtual_Host.Register
     (VH, "pascal", VH_Demo_CB.H2_CB'Access);

--     AWS.Services.Dispatchers.Virtual_Host.Register
--       (VH, "pascal", "localhost:8080");

   AWS.Server.Start
     (WS,
      Dispatcher => VH,
      Config     => AWS.Config.Default_Config);

   delay 60.0;

   AWS.Server.Shutdown (WS);
end VH_Demo;
