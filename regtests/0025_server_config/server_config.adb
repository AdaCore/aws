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

with Ada.Text_IO;

with AWS.Config.Set;
with AWS.Log;
with AWS.MIME;
with AWS.Response;
with AWS.Server;
with AWS.Status;
with AWS.Utils;

procedure Server_Config is

   use Ada;
   use AWS;

   WS : Server.HTTP;

   Conf : constant Config.Object := Config.Get_Current;
   --  Config as read from the ini files

   Server_Conf : Config.Object := Config.Get_Current;
   --  Server config

   --------
   -- CB --
   --------

   function CB (Request : Status.Data) return Response.Data is
   begin
      return Response.Build (MIME.Text_HTML, "Ok");
   end CB;

   -------------
   -- Display --
   -------------

   procedure Display (O : Config.Object) is
   begin
      Text_IO.Put_Line (Config.Server_Name (O));
      Text_IO.Put_Line (Config.WWW_Root (O));
      Text_IO.Put_Line (Config.Log_File_Directory (O));
      Text_IO.Put_Line (Config.Log_Filename_Prefix (O));
      Text_IO.Put_Line (Config.Log_Split_Mode (O));
      Text_IO.Put_Line (Utils.Image (Config.Server_Port (O)));
      Text_IO.Put_Line (Utils.Image (Config.Max_Connection (O)));
      Text_IO.Put_Line (Config.Status_Page (O));
      Text_IO.Put_Line (Config.Directory_Browser_Page (O));
   end Display;

begin
   Config.Set.Directory_Browser_Page (Server_Conf, "from_ada_file");
   Config.Set.Server_Name (Server_Conf, "Server Config");
   Config.Set.Server_Port (Server_Conf, 0);
   Config.Set.Log_Split_Mode (Server_Conf, "Monthly");

   Server.Start (WS, CB'Unrestricted_Access, Server_Conf);

   Display (Conf);

   Text_IO.New_Line;

   Display (Server.Config (WS));

   Server.Shutdown (WS);
end Server_Config;
