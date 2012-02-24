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

with AWS.Server.Status;
with AWS.Client;
with AWS.Status;
with AWS.MIME;
with AWS.Response;
with AWS.Messages;
with AWS.Utils;

procedure Moved is

   use Ada;
   use Ada.Text_IO;
   use AWS;

   function CB (Request : Status.Data) return Response.Data;

   Valid_URI : constant String := "/I_know_you_have_it";

   HTTP    : Server.HTTP;
   Connect : Client.HTTP_Connection;
   R       : Response.Data;

   --------
   -- CB --
   --------

   function CB (Request : Status.Data) return Response.Data is
   begin
      if Status.URI (Request) = Valid_URI then
         return Response.Build (MIME.Text_HTML, "You are right.");
      else
         return Response.Moved (Valid_URI,
         "HTTP/1.1 200 it is the message body,"
           & ASCII.LF & "Client should not interpret it as a header line."
           & ASCII.LF & ASCII.LF);
      end if;
   end CB;

begin
   Server.Start
     (HTTP, "Testing ""moved"" answer.", CB'Unrestricted_Access, Port => 0,
      Max_Connection => 3);

   Client.Create
     (Connection => Connect,
      Host       => Server.Status.Local_URL (HTTP),
      Timeouts   => Client.Timeouts
        (Connect => 5.0, Send => 5.0, Receive => 5.0, Response => 5.0));

   Client.Get (Connect, R, "/do_you_have_it");

   Put_Line ("-> " & Response.Message_Body (R));
   Put_Line ("-> " & Response.Location (R));

   Client.Get (Connect, R, Valid_URI);
   Put_Line ("-> " & Response.Message_Body (R));

   Client.Close (Connect);

   Server.Shutdown (HTTP);

exception
   when E : others =>
      Put_Line ("Main Error " & Exceptions.Exception_Information (E));
      Server.Shutdown (HTTP);
end Moved;
