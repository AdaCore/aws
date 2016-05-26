------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                      Copyright (C) 2016, AdaCore                         --
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

with Ada.Strings.Unbounded;
with Ada.Text_IO;

with AWS.Client;
with AWS.MIME;
with AWS.Response;
with AWS.Server.Status;
with AWS.Status;

procedure Put_Post is

   use Ada;
   use Ada.Strings.Unbounded;
   use AWS;

   WS         : Server.HTTP;
   Connection : Client.HTTP_Connection;
   Reply      : Response.Data;

   function CB (Request : Status.Data) return Response.Data is
      Mes : constant String := To_String (Status.Binary_Data (Request));
   begin
      return Response.Build (MIME.Text_HTML, Mes);
   end CB;

begin
   Server.Start
     (WS, "simple", CB'Unrestricted_Access, Port => 0, Max_Connection => 5);
   Text_IO.Put_Line ("started"); Ada.Text_IO.Flush;
   delay 1.0;

   Client.Create
     (Connection => Connection,
      Host       => Server.Status.Local_URL (WS));

   Client.Put  (Connection, Reply, "put: some dara", "/put");

   Text_IO.Put_Line (String'(Response.Message_Body (Reply)));

   Client.Post (Connection, Reply, "post: some data", "/post");

   Text_IO.Put_Line (String'(Response.Message_Body (Reply)));

   Server.Shutdown (WS);
   Text_IO.Put_Line ("shutdown");
end Put_Post;
