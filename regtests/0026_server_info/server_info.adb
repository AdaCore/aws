------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2003-2008, AdaCore                     --
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

with Ada.Exceptions;
with Ada.Text_IO;

with AWS.Client;
with AWS.MIME;
with AWS.Response;
with AWS.Server.Status;
with AWS.Status;
with AWS.Utils;

with Get_Free_Port;

procedure Server_Info is

   use Ada;
   use AWS;

   WS    : Server.HTTP;
   Port1 : Natural := 1258;
   Port2 : Natural := 1259;

   task type T_Client is
      entry Start;
      entry Connected;
      entry Stopped;
   end T_Client;

   task Server is
      entry Started;
      entry Stop;
   end Server;

   Clients : array (1 .. 5) of T_Client;

   --------
   -- CB --
   --------

   function CB (Request : in Status.Data) return Response.Data is
   begin
      return Response.Build (MIME.Text_HTML, "Ok");
   end CB;

   --------------
   -- T_Client --
   --------------

   task body T_Client is
      R : Response.Data;
      C : Client.HTTP_Connection;
   begin
      accept Start;

      Client.Create (C, "http://localhost:" & Utils.Image (Port1));

      Client.Get (C, R, "/");

      accept Connected;

      for K in 1 .. 4 loop
         Client.Get (C, R, "/");
      end loop;

      accept Stopped;

      Client.Close (C);

   exception
      when E : others =>
         Text_IO.Put_Line (Exceptions.Exception_Information (E));
   end T_Client;

   ------------
   -- Server --
   ------------

   task body Server is
   begin
      Get_Free_Port (Port1);

      AWS.Server.Start
        (WS, "Server Info",
         CB'Unrestricted_Access,
         Port           => Port1,
         Max_Connection => 6);

      Text_IO.Put_Line ("started");

      accept Started;

      accept Stop;

      Text_IO.Put_Line ("Ready to stop");
   end Server;

begin
   Server.Started;

   Text_IO.Put_Line
     ("Shutdown " & Boolean'Image (AWS.Server.Status.Is_Shutdown (WS)));

   Clients (1).Start;
   Clients (2).Start;

   Clients (1).Connected;
   Clients (2).Connected;

   Text_IO.Put_Line
     ("Current connection "
        & Natural'Image (AWS.Server.Status.Current_Connections (WS)));

   Clients (3).Start;
   Clients (4).Start;
   Clients (5).Start;

   Clients (3).Connected;
   Clients (4).Connected;
   Clients (5).Connected;

   Text_IO.Put_Line
     ("Current connection "
        & Natural'Image (AWS.Server.Status.Current_Connections (WS)));

   Text_IO.Put_Line
     ("Server session  "
        & Boolean'Image (AWS.Server.Status.Is_Session_Activated (WS)));
   Text_IO.Put_Line
     ("Server security "
        & Boolean'Image (AWS.Server.Status.Is_Security_Activated (WS)));

   for K in Clients'Range loop
      Clients (K).Stopped;
   end loop;

   delay 1.0;

   Text_IO.Put_Line
     ("Resources served "
        & Natural'Image (AWS.Server.Status.Resources_Served (WS)));

   Server.Stop;

   AWS.Server.Shutdown (WS);

   Text_IO.Put_Line
     ("Shutdown " & Boolean'Image (AWS.Server.Status.Is_Shutdown (WS)));

   Text_IO.New_Line;

   Get_Free_Port (Port2);

   AWS.Server.Start
     (WS, "Server Info",
      CB'Unrestricted_Access,
      Port           => Port2,
      Max_Connection => 2,
      Session        => True);

   Text_IO.Put_Line
     ("Shutdown " & Boolean'Image (AWS.Server.Status.Is_Shutdown (WS)));

   Text_IO.Put_Line
     ("Server session  "
        & Boolean'Image (AWS.Server.Status.Is_Session_Activated (WS)));
   Text_IO.Put_Line
     ("Server security "
        & Boolean'Image (AWS.Server.Status.Is_Security_Activated (WS)));

   AWS.Server.Shutdown (WS);

   Text_IO.Put_Line ("shutdown");
end Server_Info;
