------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                            Copyright (C) 2003                            --
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

with AWS.Client;
with AWS.MIME;
with AWS.Response;
with AWS.Server;
with AWS.Status;

procedure File is

   use Ada;
   use AWS;

   WS : Server.HTTP;

   function CB (Request : in Status.Data) return Response.Data is
      URI : constant String := Status.URI (Request);
   begin
      if URI = "/zero" then
         return Response.File (MIME.Text_HTML, "zerolength.html");
      else
         return Response.Build (MIME.Text_HTML, "URI not supported");
      end if;
   end CB;

   procedure Call_It is
      R : Response.Data;
   begin
      R := Client.Get ("http://localhost:4567/zero");

      if Response.Content_Length (R) = 0 then
         Text_IO.Put_Line ("OK length is 0");
      else
         Text_IO.Put_Line
           ("NOK length is " & Natural'Image (Response.Content_Length (R)));
         Text_IO.Put_Line
           ("=> " & Response.Message_Body (R));
      end if;
   end Call_It;

begin
   Server.Start
     (WS, "file", CB'Unrestricted_Access, Port => 4567, Max_Connection => 5);
   Text_IO.Put_Line ("started"); Ada.Text_IO.Flush;

   delay 1.0;

   Call_It;

   Server.Shutdown (WS);
   Text_IO.Put_Line ("shutdown");
end File;
