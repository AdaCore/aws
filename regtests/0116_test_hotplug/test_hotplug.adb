------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2004-2012, AdaCore                     --
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

--  Test hotplug feature

with Ada.Exceptions;
with Ada.Text_IO;

with AWS.Client.Hotplug;
with AWS.Config.Set;
with AWS.Messages;
with AWS.Response;
with AWS.Server.Hotplug;
with AWS.Server.Status;
with AWS.Utils;

with Hotplug_Pack;
with Get_Free_Port;

procedure Test_Hotplug is

   use Ada;
   use AWS;

   Filter    : constant String := "/H.*";
   Localhost : constant String := "127.0.0.1";

   procedure Request (URI : String);
   --  Request URI resource to main server, output result

   Hotplug_Port : Natural := 1235;
   Com_Port     : Natural := 2222;

   -------------
   -- Request --
   -------------

   procedure Request (URI : String) is
      R : Response.Data;
   begin
      R := Client.Get
        (Server.Status.Local_URL (Hotplug_Pack.Main_Server) & "/" & URI);
      Text_IO.Put_Line ("Response: " & Response.Message_Body (R));
   end Request;

   use type Messages.Status_Code;

   WS  : Server.HTTP;
   CFG : Config.Object;
   R   : Response.Data;

begin
   Text_IO.Put_Line ("Starting main server...");

   Get_Free_Port (Hotplug_Port);
   Get_Free_Port (Com_Port);

   --  Write access file

   declare
      F : Text_IO.File_Type;
   begin
      Text_IO.Create (F, Text_IO.Out_File, "hotplug_access.ini");
      Text_IO.Put_Line
        (F, "hp_test:f8de61f1f97df3613fbe29b031eb52c6:" & Localhost & ':'
         & Utils.Image (Hotplug_Port));
      Text_IO.Close (F);
   end;

   Config.Set.Server_Name    (CFG, "Main");
   Config.Set.Admin_URI      (CFG, "/Admin-Page");
   Config.Set.Server_Host    (CFG, Localhost);
   Config.Set.Server_Port    (CFG, 0);
   Config.Set.Max_Connection (CFG, 3);

   Server.Start (Hotplug_Pack.Main_Server, Hotplug_Pack.Main'Access, CFG);

   Server.Hotplug.Activate
     (Hotplug_Pack.Main_Server'Access, Com_Port, "hotplug_access.ini",
      Host => Localhost);

   --  Send some requests

   Request ("MkHuoi");
   Request ("toto");
   Request ("Hotplug");

   --  Start hotplug now

   Config.Set.Server_Name    (CFG, "Hotplug");
   Config.Set.Server_Port    (CFG, Hotplug_Port);

   Server.Start (WS, Hotplug_Pack.Hotplug'Access, CFG);

   R := Client.Hotplug.Register
          ("hp_test", Hotplug_Pack.Password,
           "http://" & Localhost & ':' & Utils.Image (Com_Port),
           Filter, "http://" & Localhost & ':' & Utils.Image (Hotplug_Port));

   if Response.Status_Code (R) = Messages.S200 then
      Text_IO.Put_Line ("Register OK");
   else
      Text_IO.Put_Line
         ("Register Error : " & Response.Message_Body (R));
      raise Constraint_Error;
   end if;

   --  Hotplug started

   Request ("MkHuoi");
   Request ("toto");
   Request ("Hotplug");

   Text_IO.Put_Line ("Stop hotplug server");

   R := AWS.Client.Hotplug.Unregister
          ("hp_test", Hotplug_Pack.Password,
           "http://" & Localhost & ':' & Utils.Image (Com_Port), Filter);

   if Response.Status_Code (R) = Messages.S200 then
      Text_IO.Put_Line ("Unregister OK");
   else
      Text_IO.Put_Line
        ("Unregister Error : " & Response.Message_Body (R));
   end if;

   Server.Shutdown (WS);

   Text_IO.Put_Line ("Shutdown hotplug server support");

   Server.Hotplug.Shutdown;

   Text_IO.Put_Line ("Shutdown main server");

   Server.Shutdown (Hotplug_Pack.Main_Server);

exception
   when E : others =>
      Text_IO.Put_Line
        ("Main task " & Ada.Exceptions.Exception_Information (E));
      AWS.Server.Hotplug.Shutdown;
      Server.Shutdown (Hotplug_Pack.Main_Server);
end Test_Hotplug;
