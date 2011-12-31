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

with Ada.Calendar;
with Ada.Exceptions;
with Ada.Text_IO;

with AWS.Client;
with AWS.MIME;
with AWS.Response;
with AWS.Server;
with AWS.Session;
with AWS.Status;
with AWS.Utils;

with Get_Free_Port;
with Stack_Size;

procedure Sessions is

   use Ada;
   use AWS;

   WS   : Server.HTTP;
   Port : Natural := 1273;

   task type T_Client is
      pragma Storage_Size (Stack_Size.Value);
      entry Start;
      entry Stopped;
   end T_Client;

   task Server is
      pragma Storage_Size (Stack_Size.Value);
      entry Started;
      entry Stop;
   end Server;

   Clients : array (1 .. 10) of T_Client;

   --------
   -- CB --
   --------

   function CB (Request : Status.Data) return Response.Data is
      SID : constant Session.ID := Status.Session (Request);
      N   : Natural := 0;
   begin
      if Session.Exist (SID, "toto") then
         N := Session.Get (SID, "toto");
         N := N + 1;
      end if;

      Session.Set (SID, "toto", N);

      return Response.Build
        (MIME.Text_HTML, "Ok, this is call " & Natural'Image (N));
   end CB;

   --------------
   -- T_Client --
   --------------

   task body T_Client is
      R : Response.Data;
      C : Client.HTTP_Connection;
   begin
      accept Start;

      Client.Create (C, "http://localhost:" & Utils.Image (Port));

      for K in 1 .. 10 loop
         Client.Get (C, R, "/");
         delay 0.1;
      end loop;

      accept Stopped;

      Client.Close (C);

   exception
      when E : others =>
         Text_IO.Put_Line (Exceptions.Exception_Information (E));
   end T_Client;

   ------------------
   -- Get_Sessions --
   ------------------

   procedure Get_Sessions is

      N : Natural := 0;

      procedure Action
        (N          : Positive;
         SID        : Session.ID;
         Time_Stamp : Ada.Calendar.Time;
         Quit       : in out Boolean)
      is
         K : Natural;
      begin
         K := Session.Get (SID, "toto");

         if Session.Exist (SID, "toto") then
            null;
         end if;

         Get_Sessions.N := Get_Sessions.N + 1;
      end Action;

      procedure Go is new AWS.Session.For_Every_Session (Action);

   begin
      Go;

      if N /= 0 then
         Text_IO.Put_Line ("OK");
      else
         Text_IO.Put_Line ("NOK");
      end if;
   end Get_Sessions;

   ------------
   -- Server --
   ------------

   task body Server is
   begin
      Get_Free_Port (Port);

      AWS.Server.Start
        (WS, "session",
         CB'Unrestricted_Access,
         Port           => Port,
         Max_Connection => 5,
         Session        => True);

      accept Started;

      Ada.Text_IO.Put_Line ("started");

      accept Stop;

      Ada.Text_IO.Put_Line ("Ready to stop");
   exception
      when E : others =>
         Ada.Text_IO.Put_Line ("Server error");
   end Server;

begin
   Server.Started;

   for K in Clients'Range loop
      Clients (K).Start;
   end loop;

   delay 1.0;

   Get_Sessions;

   delay 1.0;

   Get_Sessions;

   for K in Clients'Range loop
      Clients (K).Stopped;
   end loop;

   Server.Stop;

   AWS.Server.Shutdown (WS);

   Session.Clear;

   Ada.Text_IO.Put_Line ("shutdown");
end Sessions;
