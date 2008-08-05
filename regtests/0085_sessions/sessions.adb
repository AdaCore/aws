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

procedure Sessions is

   use Ada;
   use AWS;

   WS   : Server.HTTP;
   Port : Natural := 1273;

   task type T_Client is
      entry Start;
      entry Stopped;
   end T_Client;

   task Server is
      entry Started;
      entry Stop;
   end Server;

   Clients : array (1 .. 10) of T_Client;

   --------
   -- CB --
   --------

   function CB (Request : in Status.Data) return Response.Data is
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
        (N          : in     Positive;
         SID        : in     Session.ID;
         Time_Stamp : in     Ada.Calendar.Time;
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
