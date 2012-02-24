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

--  Test for user defined stream raising an exception

with Ada.Text_IO;
with Ada.Exceptions;
with Ada.Streams;

with AWS.Server.Status;
with AWS.Client;
with AWS.Status;
with AWS.MIME;
with AWS.Response;
with AWS.Messages;

with AWS.Resources.Streams;
with AWS.Utils;

with Error_Strm;

procedure Strm2 is

   use Ada;
   use Ada.Text_IO;
   use AWS;

   function CB (Request : Status.Data) return Response.Data;

   HTTP    : Server.HTTP;
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

begin
   Server.Start
     (HTTP, "Testing user defined stream.", CB'Unrestricted_Access, Port => 0,
      Max_Connection => 3);

   Client.Create
     (Connection => Connect,
      Host       => AWS.Server.Status.Local_URL (HTTP),
      Timeouts   => Client.Timeouts
        (Connect => 1.0, Send => 5.0, Receive => 5.0));

   Client.Get (Connect, R, "/toto");

   Client.Close (Connect);

   Text_IO.Put_Line
     ("> " & Utils.Head_Before
               (Response.Message_Body (R), "Call stack traceback locations:"));

   Server.Shutdown (HTTP);

exception
   when E : others =>
      Put_Line ("Main Error " & Exceptions.Exception_Information (E));
      Server.Shutdown (HTTP);
end Strm2;
