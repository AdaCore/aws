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
with AWS.Config.Set;
with AWS.Messages;
with AWS.MIME;
with AWS.Response;
with AWS.Server.Status;
with AWS.Status;
with AWS.Utils;

procedure Once is

   use Ada;
   use AWS;

   WS : Server.HTTP;
   WP : Config.Object;

   --------
   -- CB --
   --------

   function CB (Request : Status.Data) return Response.Data is
      URI : constant String := Status.URI (Request);
   begin
      if URI = "/once" then
         if Utils.Is_Regular_File ("once.html") then
            return Response.File (MIME.Text_HTML, "once.html", Once => True);
         else
            return Response.Acknowledge (Messages.S404);
         end if;
      else
         return Response.Build (MIME.Text_HTML, "URI not supported");
      end if;
   end CB;

   -------------
   -- Call_It --
   -------------

   procedure Call_It is
      use type Messages.Status_Code;
      R : Response.Data;
   begin
      R := Client.Get (Server.Status.Local_URL (WS) & "/once");

      if Response.Status_Code (R) = Messages.S404 then
         Text_IO.Put_Line ("404 not found");
      else
         Text_IO.Put_Line
           ("Found: " & Response.Message_Body (R));
      end if;
   end Call_It;

   File : Text_IO.File_Type;

begin
   Config.Set.Server_Name    (WP, "once");
   Config.Set.Server_Host    (WP, "localhost");
   Config.Set.Server_Port    (WP, 0);
   Config.Set.Max_Connection (WP, 5);

   Server.Start (WS, CB'Unrestricted_Access, WP);
   Text_IO.Put_Line ("started"); Ada.Text_IO.Flush;

   delay 1.0;

   Call_It;

   Text_IO.Create (File, Text_IO.Out_File, "once.html");
   Text_IO.Put_Line (File, "<p>This is once file's content");
   Text_IO.Close (File);

   Call_It;

   Call_It;

   Server.Shutdown (WS);
   Text_IO.Put_Line ("shutdown");
end Once;
