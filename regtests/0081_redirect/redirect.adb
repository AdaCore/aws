------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2003-2018, AdaCore                     --
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
with AWS.Messages;
with AWS.MIME;
with AWS.Response;
with AWS.Server.Status;
with AWS.Status;
with AWS.Utils;

procedure Redirect is

   use Ada;
   use AWS;
   use type AWS.Messages.Status_Code;

   WS : Server.HTTP;

   --------
   -- CB --
   --------

   function CB (Request : Status.Data) return Response.Data is
      URI : constant String := Status.URI (Request);
   begin
      if URI = "/first" then
         return Response.URL ("/second");

      elsif URI = "/move" then
         return Response.Moved ("/second", "page moved", AWS.MIME.Text_Plain);

      elsif URI = "/second" then
         return Response.Build (MIME.Text_HTML, "That's good!");

      else
         return Response.Build (MIME.Text_HTML, "URI not supported");
      end if;
   end CB;

   -------------
   -- Call_It --
   -------------

   procedure Call_It is
      R : Response.Data;
   begin
      R := Client.Get (Server.Status.Local_URL (WS) & "/first");

      if Response.Status_Code (R) = Messages.S302 then
         Text_IO.Put_Line ("OK, status is good");

      else
         Text_IO.Put_Line
           ("NOK, wrong status "
            & Messages.Image (Response.Status_Code (R)));
      end if;

      Text_IO.Put_Line (Response.Location (R));

      R := Client.Get
             (Server.Status.Local_URL (WS) & "/first",
              Follow_Redirection => True);

      Text_IO.Put_Line
           (Response.Status_Code (R)'Img & ' ' & Response.Message_Body (R));

      R := Client.Get
             (Server.Status.Local_URL (WS) & "/move",
              Follow_Redirection => True);

      Text_IO.Put_Line
        (Response.Status_Code (R)'Img & ' ' & Response.Message_Body (R));
   end Call_It;

   --------------------
   -- Test_Connected --
   --------------------

   procedure Test_Connected is
      Connect : Client.HTTP_Connection;
      R       : Response.Data;
   begin
      Client.Create (Connect, Server.Status.Local_URL (WS));
      Client.Get (Connect, R, "/first");

      Text_IO.Put_Line
        (Response.Status_Code (R)'Img & ' ' & Response.Location (R) & ' '
         & Response.Message_Body (R));

      Client.Get (Connect, R, "/move");

      Text_IO.Put_Line
        (Response.Status_Code (R)'Img & ' ' & Response.Location (R) & ' '
         & Response.Message_Body (R));
   end Test_Connected;


begin
   Server.Start
     (WS, "file", CB'Unrestricted_Access, Port => 0, Max_Connection => 5);
   Text_IO.Put_Line ("started"); Ada.Text_IO.Flush;

   Call_It;
   Test_Connected;

   Server.Shutdown (WS);
   Text_IO.Put_Line ("shutdown");
end Redirect;
