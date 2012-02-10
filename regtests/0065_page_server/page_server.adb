------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2007-2012, AdaCore                     --
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

with Ada.Directories;
with Ada.Text_IO;

with AWS.Client;
with AWS.Config.Set;
with AWS.Messages;
with AWS.Response;
with AWS.Server.Status;
with AWS.Services.Page_Server;
with AWS.Utils;

procedure Page_Server is
   use Ada;
   use AWS;
   use type Messages.Status_Code;

   Conf : Config.Object;
   WS   : Server.HTTP;
   R    : Response.Data;
   File : Text_IO.File_Type;

begin
   --  Populate the server Web Root

   Directories.Create_Directory ("icons");
   Text_IO.Create (File, Text_IO.Out_File, "icons/sound1.gif");
   Text_IO.Close (File);

   Config.Set.WWW_Root (Conf, "icons");
   Config.Set.Server_Port (Conf, 0);

   Server.Start (WS, Services.Page_Server.Callback'Access, Conf);

   R := Client.Get (Server.Status.Local_URL (WS) & "/sound1.gif");

   if Response.Status_Code (R) = Messages.S200  then
      Text_IO.Put_Line ("OK for sound1.gif");
   else
      Text_IO.Put_Line ("NOK for sound1.gif");
   end if;

   R := Client.Get (Server.Status.Local_URL (WS) & "/nothere.gif");

   if Response.Status_Code (R) = Messages.S404  then
      Text_IO.Put_Line ("OK for nothere.gif");
   else
      Text_IO.Put_Line ("NOK for nothere.gif");
   end if;

   Server.Shutdown (WS);
end Page_Server;
