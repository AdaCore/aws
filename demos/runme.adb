------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                            Copyright (C) 2000                            --
--                      Dmitriy Anisimov - Pascal Obry                      --
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

--  How to run this demo ?
--
--  On the server side:
--  $ runme
--
--  On the client side:
--  * launch your Web Browser (this should work with any browser)
--  * enter the URL : http://<servername>:1234/ or https://<servername>:4433/
--
--  You can ask for whatever URI
--  http://<servername>:1234/give_me_that
--
--  And the server should reply with the following message (which should be
--  displayed in your browser window):
--
--     Hello, I'am glad you ask for /give_me_that.
--     I'am the runme demo. Note that this message could have been fetched
--     on my file system...
--
--  To get the administrative page:
--  http://<servername>:1234/Admin-Page
--
--  To test the HTTPS/SSL server:
--  https://<servername>:4433/give_me_this
--
--  That's all for now. Enjoy !

with Ada.Text_IO;

with AWS.Log;
with AWS.Server;

with Runme_CB;

procedure Runme is

   use Ada;

   WSS  : AWS.Server.HTTP;
   WS   : AWS.Server.HTTP;

begin
   Text_IO.Put_Line ("AWS " & AWS.Version);
   Text_IO.Put_Line ("Enter any key to exit...");

   AWS.Server.Start (WSS, "Runme Secure",
                     Max_Connection => 3,
                     Port           => 4433,
                     Security       => True,
                     Callback       => Runme_CB.Service_Sec'Access);

   AWS.Server.Start (WS, "Runme",
                     Max_Connection => 3,
                     Admin_URI      => "/Admin-Page",
                     Port           => 1234,
                     Session        => True,
                     Callback       => Runme_CB.Service'Access);

   AWS.Server.Start_Log (WS, Split_Mode => AWS.Log.Daily);
   AWS.Server.Start_Log (WSS, Filename_Prefix => "runme-secure");

   --  Do not exit from this procedure

   declare
      C : Character;
   begin
      Text_IO.Get_Immediate (C);
   end;

   --  Close servers.

   AWS.Server.Shutdown (WS);
   AWS.Server.Shutdown (WSS);

end Runme;
