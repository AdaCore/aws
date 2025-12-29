------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                       Copyright (C) 2025, AdaCore                        --
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

with Ada.Exceptions;
with Ada.Text_IO;

with AWS.Client.Hotplug;
with AWS.Messages;
with AWS.MIME;
with AWS.Server.Hotplug;
with AWS.Server.Status;
with AWS.Response;
with AWS.Utils;

with Main;
with Hotplug;
with Data;

procedure Driver is

   use Ada;
   use Ada.Exceptions;

   use AWS;

   use type AWS.Messages.Status_Code;

   procedure Check;

   procedure Check is
      R : Response.Data;
   begin
      R := Client.Post
        ("http://localhost:"
         & Utils.Image (AWS.Server.Status.Port (Data.M_WS)) & "/AWS",
         "v1=one&v2=two",
         Content_Type => MIME.Application_Form_Data);
      Text_IO.Put_Line ("R:" & AWS.Response.Message_Body (R));
   end Check;

   R : Response.Data;

begin
   Main;

   --  check with main only
   Check;

   Hotplug;

   --  check with hotplug registered
   Check;

   R := AWS.Client.Hotplug.Unregister
     ("hp_test", Data.Password,
      "http://localhost:2222", Data.Filter);

   if AWS.Response.Status_Code (R) /= AWS.Messages.S200 then
      Text_IO.Put_Line
        ("Unregister Error : " & AWS.Response.Message_Body (R));
   end if;

   --  check with hotplug unregistered
   Check;

   AWS.Server.Hotplug.Shutdown;
   AWS.Server.Shutdown (Data.M_WS);
   AWS.Server.Shutdown (Data.H_WS);

exception
   when E : others =>
      Text_IO.Put_Line ("Error: " & Exception_Information (E));
      AWS.Server.Hotplug.Shutdown;
      AWS.Server.Shutdown (Data.M_WS);
end Driver;
