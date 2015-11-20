------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                       Copyright (C) 2015, AdaCore                        --
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

--  Test broken request line

with Ada.Text_IO;

with AWS.Client;
with AWS.Messages;
with AWS.MIME;
with AWS.Response;
with AWS.Server.Status;
with AWS.Status;

procedure Broken_Request_Line is

   use Ada;
   use AWS;

   function CB (Request : Status.Data) return Response.Data;

   --------
   -- CB --
   --------

   function CB (Request : Status.Data) return Response.Data is
   begin
      return Response.Build (MIME.Text_HTML, "should not be called");
   end CB;

   WS : Server.HTTP;
   R  : Response.Data;

begin
   Server.Start
     (WS, "Broken Request Line", CB'Unrestricted_Access,
      Session => True, Port => 0);

   declare
      URL : constant String := Server.Status.Local_URL (WS);
   begin
      R := Client.Get (URL & "/toto" & ASCII.LF);
      Text_IO.Put_Line
        ("> " & Messages.Image (Response.Status_Code (R))
         & ", " & Response.Message_Body (R));
   end;

   Server.Shutdown (WS);
end Broken_Request_Line;
