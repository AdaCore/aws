------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2003-2009, AdaCore                     --
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

--  Test for user defined stream raising an exception

with Ada.Text_IO;
with Ada.Exceptions;
with Ada.Streams;

with AWS.Server;
with AWS.Client;
with AWS.Status;
with AWS.MIME;
with AWS.Response;
with AWS.Messages;

with AWS.Resources.Streams;

with Error_Strm;

procedure Strm2 is

   use Ada;
   use Ada.Text_IO;
   use AWS;

   function CB (Request : Status.Data) return Response.Data;

   task Server is
      entry Wait_Start;
      entry Stop;
   end Server;

   HTTP    : AWS.Server.HTTP;
   Connect : Client.HTTP_Connection;
   R       : Response.Data;

   --------
   -- CB --
   --------

   function CB (Request : Status.Data) return Response.Data is
      File : AWS.Resources.Streams.Stream_Access := new Error_Strm.File_Tagged;
   begin
      if Status.URI (Request) = "/toto" then
         return AWS.Response.Stream ("text/plain", File);
      else
         return AWS.Response.Build
           ("text/plain", "Unknown resource", Messages.S404);
      end if;
   end CB;

   ------------
   -- Server --
   ------------

   task body Server is
   begin
      AWS.Server.Start
        (HTTP, "Testing user defined stream.",
         CB'Unrestricted_Access, Port => 1250, Max_Connection => 3);

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
      Host       => "http://localhost:1250",
      Timeouts   => Client.Timeouts
        (Connect => 1.0, Send => 5.0, Receive => 5.0));

   Client.Get (Connect, R, "/toto");

   Client.Close (Connect);

   Text_IO.Put_Line ("> " & Response.Message_Body (R));

   Server.Stop;

exception
   when E : others =>
      Put_Line ("Main Error " & Exceptions.Exception_Information (E));
      Server.Stop;
end Strm2;
