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

with Ada.Text_IO;

with AWS.Client;
with AWS.MIME;
with AWS.Response;
with AWS.Server.Status;
with AWS.Status;
with AWS.Utils;

procedure File is

   use Ada;
   use AWS;

   WS : Server.HTTP;

   function CB (Request : Status.Data) return Response.Data is
      URI : constant String := Status.URI (Request);
   begin
      if URI = "/zero" then
         return Response.File (MIME.Text_HTML, "zerolength.html");
      else
         return Response.Build (MIME.Text_HTML, "URI not supported");
      end if;
   end CB;

   procedure Call_It is
      use type Response.Content_Length_Type;
      R : Response.Data;
   begin
      R := Client.Get (Server.Status.Local_URL (WS) & "/zero");

      if Response.Content_Length (R) = 0 then
         Text_IO.Put_Line ("OK length is 0");
      else
         Text_IO.Put_Line
           ("NOK length is " & Utils.Image (Response.Content_Length (R)));
         Text_IO.Put_Line
           ("=> " & Response.Message_Body (R));
      end if;
   end Call_It;

begin
   Server.Start
     (WS, "file", CB'Unrestricted_Access, Port => 0, Max_Connection => 5);
   Text_IO.Put_Line ("started"); Ada.Text_IO.Flush;

   delay 1.0;

   Call_It;

   Server.Shutdown (WS);
   Text_IO.Put_Line ("shutdown");
end File;
