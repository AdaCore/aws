------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                            Copyright (C) 2000                            --
--                               Pascal Obry                                --
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

--  Here is the first documentation.
--
--  To build you need to have the Ada socket implementation. Either the one
--  for Windows or the one for UNIX.
--
--  This has been tested with the Windows version, GNAT and Internet Explorer
--  4.01. Please let me know if it does not work with other tools.
--
--  Some pointers:
--
--  Ada Windows sockets : http://perso.wanadoo.fr/archive/sockets.tar.gz
--  Ada UNIX socket     : http://www.infres.enst.fr/ANC/
--
--  How to build this demo ?
--
--  $ gnatmake runme -I/whatever/path/to/sockets
--
--  How to run this demo ?
--
--  On the server side:
--  $ runme
--
--  On the client side:
--  * launch your browser
--  * enter the URL : http://<servername>:1234/
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
--  That's all for now. Enjoy !

with Ada.Text_IO;

with AWS.Server;
with Runme_CB;

procedure Runme is

   use Ada;

   WS : AWS.Server.HTTP (10, 1234, Runme_CB.Service'Access);

begin
   Text_IO.Put_Line ("Kill me when you want me to stop...");
   AWS.Server.Start (WS);
end Runme;
