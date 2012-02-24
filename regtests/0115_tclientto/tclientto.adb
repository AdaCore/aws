------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2000-2012, AdaCore                     --
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
with Ada.Exceptions;

with AWS.Net;
with AWS.Server.Status;
with AWS.Client;
with AWS.Status;
with AWS.MIME;
with AWS.Response;
with AWS.Parameters;
with AWS.Messages;
with AWS.Utils;

procedure Tclientto is

   use Ada;
   use Ada.Text_IO;
   use AWS;

   function CB (Request : Status.Data) return Response.Data;

   HTTP : Server.HTTP;

   --------
   -- CB --
   --------

   function CB (Request : Status.Data) return Response.Data is
      URI    : constant String          := Status.URI (Request);
      P_List : constant Parameters.List := Status.Parameters (Request);
   begin
      if URI = "/3sec" then
         delay 3.0;
         return Response.Build (MIME.Text_HTML, "3sec ok");

      elsif URI = "/10sec" then
         delay 10.0;
         return Response.Build (MIME.Text_HTML, "10sec ok");

      else
         Put_Line ("Unknown URI " & URI);
         return Response.Build
           (MIME.Text_HTML, URI & " not found", Messages.S404);
      end if;
   end CB;

   -------------
   -- Request --
   -------------

   procedure Request is
      R : Response.Data;
      Local_URL : constant String := AWS.Server.Status.Local_URL (HTTP);
   begin
      R := Client.Get (Local_URL & "/3sec",
                       Timeouts => Client.Timeouts
                         (Connect => 1.0, Send  => 1.0, Receive => 1.0));
      Put_Line ("=> " & Response.Message_Body (R));
      New_Line;

      R := Client.Get (Local_URL & "/3sec",
                       Timeouts => Client.Timeouts
                         (Connect => 1.0, Send  => 10.0, Receive => 10.0));
      Put_Line ("=> " & Response.Message_Body (R));
      New_Line;

      R := Client.Get (Local_URL & "/10sec",
                       Timeouts => Client.Timeouts
                         (Connect => 1.0, Send  => 1.0, Receive => 20.0));
      Put_Line ("=> " & Response.Message_Body (R));
      New_Line;

      R := Client.Get (Local_URL & "/10sec",
                       Timeouts => Client.Timeouts
                         (Connect => 1.0, Send  => 10.0, Receive => 7.0));
      Put_Line ("=> " & Response.Message_Body (R));
      New_Line;
   end Request;

   -------------------
   -- Alive_Request --
   -------------------

   procedure Alive_Request is
      R       : Response.Data;
      Connect : Client.HTTP_Connection;
   begin
      Client.Create
        (Connection => Connect,
         Host       => AWS.Server.Status.Local_URL (HTTP),
         Timeouts   => Client.Timeouts (Connect => 1.0, Receive => 5.0));

      Client.Get (Connect, R, "/3sec");
      Put_Line ("-> " & Response.Message_Body (R));
      New_Line;

      Client.Get (Connect, R, "/10sec");
      Put_Line ("-> " & Response.Message_Body (R));
      New_Line;

      Client.Get (Connect, R, "/3sec");
      Put_Line ("-> " & Response.Message_Body (R));
      New_Line;

      Client.Get (Connect, R, "/10sec");
      Put_Line ("-> " & Response.Message_Body (R));
      New_Line;

      Client.Close (Connect);
   end Alive_Request;

begin
   Put_Line ("Start main, wait for server to start...");

   Server.Start
     (HTTP, "Test Client Timeouts", CB'Unrestricted_Access, Port => 0,
      Max_Connection => 5);

   Put_Line ("Server started");
   New_Line;

   Request;

   Alive_Request;

   Server.Shutdown (HTTP);

exception
   when E : others =>
      Put_Line ("Main Error " & Exceptions.Exception_Information (E));
      Server.Shutdown (HTTP);
end Tclientto;
