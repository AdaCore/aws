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
with AWS.OS_Lib;

procedure Head is

   use Ada;
   use Ada.Text_IO;
   use AWS;

   function CB (Request : in Status.Data) return Response.Data;

   My_Name : constant String := "head.adb";
   HTTP    : AWS.Server.HTTP;
   Connect : Client.HTTP_Connection;
   R_Get   : Response.Data;
   R_Head  : Response.Data;

   --------
   -- CB --
   --------

   function CB (Request : in Status.Data) return Response.Data is
   begin
      return Response.File ("text/plain", "." & Status.URI (Request));
   end CB;

begin
   AWS.Server.Start
     (HTTP, "Testing head request.",
      CB'Unrestricted_Access, Port => 7645, Max_Connection => 3);

   Client.Create
     (Connection => Connect,
      Host       => "http://localhost:7645",
      Timeouts   => (5, 5));

   Client.Get (Connect, R_Get,  '/' & My_Name);
   Client.Head (Connect, R_Head, '/' & My_Name);

   if Response.Content_Length (R_Get)
     /= Response.Content_Length (R_Head)
   then
      Put_Line ("Length difference between GET and HEAD.");
      Put_Line ("   GET  " & Response.Content_Length (R_Get)'Img);
      Put_Line ("   HEAD " & Response.Content_Length (R_Head)'Img);

   elsif Response.Content_Length (R_Get)
     /= Integer (OS_Lib.File_Size (My_Name))
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
