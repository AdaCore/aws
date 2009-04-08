------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2007-2009, AdaCore                     --
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

with Ada.Calendar;
with Ada.Exceptions;
with Ada.Text_IO;

with AWS.Client;
with AWS.Config.Set;
with AWS.MIME;
with AWS.Parameters;
with AWS.Response;
with AWS.Server;
with AWS.Session;
with AWS.Status;
with AWS.Utils;

procedure multiple_Sessions is

   use Ada;
   use AWS;

   Port_A         : constant Natural := 1200;
   Port_B         : constant Natural := 1201;
   WS_A, WS_B     : Server.HTTP;
   Conf_A, Conf_B : Config.Object;

   --------
   -- CB --
   --------

   function CB (Request : Status.Data) return Response.Data is
      SID : constant Session.Id := Status.Session (Request);
      Key : constant String := "key";
      N   : Natural := 0;
   begin
      if Session.Exist (SID, Key) then
         N := Session.Get (SID, Key);
         N := N + 1;
      end if;

      Session.Set (SID, Key, N);

      return Response.Build
        (MIME.Text_HTML, "<p>Ok, this is call " & Natural'Image (N));
   end CB;

begin
   Config.Set.Server_Name    (Conf_A, "Session_A");
   Config.Set.Server_Port    (Conf_A, Port_A);
   Config.Set.Max_Connection (Conf_A, 5);
   Config.Set.Session        (Conf_A, True);
   Config.Set.Session_Name   (Conf_A, "AWS_A");

   Server.Start (WS_A, CB'Unrestricted_Access, Conf_A);

   Config.Set.Server_Name    (Conf_B, "Session_B");
   Config.Set.Server_Port    (Conf_B, Port_B);
   Config.Set.Max_Connection (Conf_B, 5);
   Config.Set.Session        (Conf_B, True);
   Config.Set.Session_Name   (Conf_B, "AWS_B");

   Server.Start (WS_B, CB'Unrestricted_Access, Conf_B);

   Text_IO.Put_Line ("Servers are started on port ");
   Text_IO.Put_Line ("   server A :" & Integer'Image (Port_A));
   Text_IO.Put_Line ("   server B :" & Integer'Image (Port_B));
   Text_IO.New_Line;
   Text_IO.Put_Line ("connect to the server and check that the counter");
   Text_IO.Put_Line ("are updated independantly.");
   Text_IO.New_Line;

   Server.Wait (Mode => Server.Q_Key_Pressed);

   Server.Shutdown (WS_A);
   Server.Shutdown (WS_B);

   Ada.Text_IO.Put_Line ("shutdown");
end multiple_Sessions;
