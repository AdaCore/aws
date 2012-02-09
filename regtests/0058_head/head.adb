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

procedure Head is

   use Ada;
   use Ada.Text_IO;
   use AWS;
   use type Response.Content_Length_Type;

   function CB (Request : Status.Data) return Response.Data;

   My_Name : constant String := "head.txt";
   HTTP    : AWS.Server.HTTP;
   Connect : Client.HTTP_Connection;
   R_Get   : Response.Data;
   R_Head  : Response.Data;

   --------
   -- CB --
   --------

   function CB (Request : Status.Data) return Response.Data is
   begin
      return Response.File ("text/plain", "." & Status.URI (Request));
   end CB;

begin
   AWS.Server.Start
     (HTTP, "Testing head request.",
      CB'Unrestricted_Access, Port => 0, Max_Connection => 3);

   Client.Create
     (Connection => Connect,
      Host       => Server.Status.Local_URL (HTTP),
      Timeouts   => Client.Timeouts
        (Connect => 5.0, Send => 5.0, Receive => 5.0));

   Client.Get (Connect, R_Get,  '/' & My_Name);
   Client.Head (Connect, R_Head, '/' & My_Name);

   if Response.Content_Length (R_Get)
     /= Response.Content_Length (R_Head)
   then
      Put_Line ("Length difference between GET and HEAD.");
      Put_Line ("   GET  " & Utils.Image (Response.Content_Length (R_Get)));
      Put_Line ("   HEAD " & Utils.Image (Response.Content_Length (R_Head)));

   elsif Response.Content_Length (R_Get)
     /= Response.Content_Length_Type (Utils.File_Size (My_Name))
   then
      Put_Line ("Length difference between GET and real length.");

   else
      Put_Line ("Ok.");
   end if;

   Client.Close (Connect);

exception
   when E : others =>
      Put_Line ("Error " & Exceptions.Exception_Information (E));
end Head;
