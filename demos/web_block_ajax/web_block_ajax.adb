------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2010-2012, AdaCore                     --
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

with AWS.MIME;
with AWS.Server;
with AWS.Services.Web_Block.Registry;

with Web_Callbacks;

procedure Web_Block_Ajax is

   use Ada;
   use AWS;
   use AWS.Services;

   HTTP : AWS.Server.HTTP;

begin
   Services.Web_Block.Registry.Register
     ("/", "page.thtml", null);
   Services.Web_Block.Registry.Register
     ("WIDGET_COUNTER", "widget_counter.thtml",
      Web_Callbacks.Widget_Counter'Access);
   Services.Web_Block.Registry.Register
     ("/onclick$next", "r_widget_counter.txml",
      Web_Callbacks.Onclick_Next'Access,
      Content_Type     => MIME.Text_XML,
      Context_Required => True);
   Services.Web_Block.Registry.Register
     ("/onclick$previous", "r_widget_counter.txml",
      Web_Callbacks.Onclick_Previous'Access,
      Content_Type     => MIME.Text_XML,
      Context_Required => True);

   Server.Start (HTTP, "web_block_ajax", Web_Callbacks.Main'Access);

   Text_IO.Put_Line ("Press Q to terminate.");

   Server.Wait (Server.Q_Key_Pressed);

   Server.Shutdown (HTTP);
end Web_Block_Ajax;
