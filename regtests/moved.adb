------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2000-2001                          --
--                               ACT-Europe                                 --
--                                                                          --
--  Authors: Dmitriy Anisimokv - Pascal Obry                                --
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

--  $Id$

with Ada.Text_IO;
with Ada.Exceptions;

with AWS.Server;
with AWS.Client;
with AWS.Status;
with AWS.MIME;
with AWS.Response;
with AWS.Messages;

procedure Moved is

   use Ada;
   use Ada.Text_IO;
   use AWS;

   function CB (Request : in Status.Data) return Response.Data;

   task Server is
      entry Wait_Start;
      entry Stop;
   end Server;

   Valid_URI : constant String := "/I_know_you_have_it";

   HTTP    : AWS.Server.HTTP;
   Connect : Client.HTTP_Connection;
   R       : Response.Data;

   --------
   -- CB --
   --------

   function CB (Request : in Status.Data) return Response.Data is
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

   ------------
   -- Server --
   ------------

   task body Server is
   begin
      AWS.Server.Start
        (HTTP, "Testing ""moved"" answer.",
         CB'Unrestricted_Access, Port => 7645, Max_Connection => 3);

      accept Wait_Start;
      accept Stop;

   exception
      when E : others =>
         Put_Line ("Server Error " & Exceptions.Exception_Information (E));
   end Server;

begin
   Server.Wait_Start;

   Client.Create
     (Connection => Connect,
      Host       => "http://localhost:7645",
      Timeouts   => (5, 5));

   Client.Get (Connect, R, "/do_you_have_it");

   Put_Line ("-> " & Response.Message_Body (R));
   Put_Line ("-> " & Response.Location (R));

   Client.Get (Connect, R, Valid_URI);
   Put_Line ("-> " & Response.Message_Body (R));

   Client.Close (Connect);

   Server.Stop;

exception
   when E : others =>
      Server.Stop;
      Put_Line ("Main Error " & Exceptions.Exception_Information (E));
end Moved;
