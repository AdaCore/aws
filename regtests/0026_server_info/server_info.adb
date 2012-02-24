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

with Ada.Exceptions;
with Ada.Text_IO;

with AWS.Config.Set;
with AWS.Client;
with AWS.MIME;
with AWS.Response;
with AWS.Server.Status;
with AWS.Status;
with AWS.Utils;

with Stack_Size;

procedure Server_Info is

   use Ada;
   use AWS;

   WS  : Server.HTTP;
   CNF : Config.Object;

   task type T_Client is
      pragma Storage_Size (Stack_Size.Value);
      entry Start;
      entry Connected;
      entry Stopped;
   end T_Client;

   Clients : array (1 .. 5) of T_Client;

   --------
   -- CB --
   --------

   function CB (Request : Status.Data) return Response.Data is
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

      Client.Create (C, AWS.Server.Status.Local_URL (WS));

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

begin
   Config.Set.Server_Name    (CNF, "Server Info");
   Config.Set.Server_Host    (CNF, "localhost");
   Config.Set.Server_Port    (CNF, 0);
   Config.Set.Max_Connection (CNF, 6);

   Server.Start (WS, CB'Unrestricted_Access, CNF);

   Text_IO.Put_Line ("started");

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

   Text_IO.Put_Line ("Ready to stop");

   Server.Shutdown (WS);

   Text_IO.Put_Line
     ("Shutdown " & Boolean'Image (AWS.Server.Status.Is_Shutdown (WS)));

   Text_IO.New_Line;

   Config.Set.Server_Port    (CNF, 0);
   Config.Set.Session        (CNF, True);
   Config.Set.Max_Connection (CNF, 2);

   Server.Start (WS, CB'Unrestricted_Access, CNF);

   Text_IO.Put_Line
     ("Shutdown " & Boolean'Image (AWS.Server.Status.Is_Shutdown (WS)));

   Text_IO.Put_Line
     ("Server session  "
        & Boolean'Image (AWS.Server.Status.Is_Session_Activated (WS)));
   Text_IO.Put_Line
     ("Server security "
        & Boolean'Image (AWS.Server.Status.Is_Security_Activated (WS)));

   Server.Shutdown (WS);

   Text_IO.Put_Line ("shutdown");
end Server_Info;
