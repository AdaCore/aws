------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                       Copyright (C) 2014, AdaCore                        --
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

procedure Client_Mode is

   use Ada;
   use AWS;

   WS : Server.HTTP;

   --------
   -- CB --
   --------

   function CB (Request : Status.Data) return Response.Data is
      URI : constant String := Status.URI (Request);
   begin
      if URI = "/file" then
         return Response.File (MIME.Text_Plain, "file.txt");
      elsif URI = "/redirect" then
         return Response.URL ("/go/here");
      elsif URI = "/message" then
         return Response.Build (MIME.Text_Plain, "some text");
      elsif URI = "/ack" then
         return Response.Acknowledge (Messages.S200, "some text");
      elsif URI = "/ackno" then
         return Response.Acknowledge (Messages.S404);
      end if;

      return Response.Acknowledge (Messages.S404);
   end CB;

   ----------
   -- Dump --
   ----------

   procedure Display (URI : String) is
      R : constant Response.Data := Client.Get (URI);
   begin
      Text_IO.Put (Messages.Status_Code'Image (Response.Status_Code (R)));
      Text_IO.Put (" - ");
      Text_IO.Put_Line (Response.Data_Mode'Image (Response.Mode (R)));
      Text_IO.New_Line;
   end Display;

   CFG : Config.Object;

begin
   Config.Set.Server_Name (CFG, "Client Mode");
   Config.Set.Server_Port (CFG, 0);
   Config.Set.Protocol_Family (CFG, "Family_Inet");

   Server.Start (WS, CB'Unrestricted_Access, CFG);

   --  AWS.Net.Log.Start (Dump'Unrestricted_Access);

   declare
      URL : constant String := Server.Status.Local_URL (WS);
   begin
      Display (URL & "/redirect");
      Display (URL & "/file");
      Display (URL & "/message");
      Display (URL & "/ack");
      Display (URL & "/ack2");
   end;

   Server.Shutdown (WS);
   Text_IO.Put_Line ("shutdown");
end Client_Mode;
