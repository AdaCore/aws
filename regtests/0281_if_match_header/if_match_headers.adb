------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                       Copyright (C) 2016, AdaCore                        --
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
with AWS.Headers;
with AWS.Messages;
with AWS.MIME;
with AWS.Parameters;
with AWS.Response.Set;
with AWS.Server.Status;
with AWS.Status;

procedure If_Match_Headers is

   use Ada;
   use AWS;

   WS : Server.HTTP;

   ETag : constant array (Positive range <>) of String (1 .. 5) :=
            ("12345", "12346");

   --------
   -- CB --
   --------

   function CB (Request : Status.Data) return Response.Data is

      H   : constant Headers.List := Status.Header (Request);
      URI : constant String := Status.URI (Request);
      E   : constant Positive :=
              Positive'Value (URI (URI'First + 1 .. URI'Last));
      R   : Response.Data := Response.Build (MIME.Text_HTML, "ok: " & URI);

   begin
      Response.Set.Add_Header
        (R, Messages.ETag_Token, String (Messages.Create_ETag (Etag (E))));
      return R;
   end CB;

   R    : Response.Data;
   H    : Headers.List;
   CFG  : Config.Object;
   HTTP : AWS.Client.HTTP_Connection;

   -------------
   -- Print_R --
   -------------

   procedure Print_R is
      use Ada.Text_IO;
      use AWS.Messages;
   begin
      Put (AWS.Response.Message_Body (R));
      if AWS.Response.Status_Code (R) /= S200 then
         Put_Line (' ' & Image (AWS.Response.Status_Code (R)));
      else
         New_Line;
      end if;
   end Print_R;

begin
   Config.Set.Server_Name (CFG, "If Match Headers");
   Config.Set.Server_Port (CFG, 0);

   Server.Start (WS, CB'Unrestricted_Access, CFG);

   AWS.Client.Create (HTTP, Server.Status.Local_URL (WS));

   AWS.Client.Get (HTTP, R, "/1", Headers => H);
   Print_R;

   --  If-None-Match

   H.Add
     (Messages.If_None_Match_Token,
      String (Messages.Create_ETag (ETag (1))));
   AWS.Client.Get (HTTP, R, "/1", Headers => H);
   Print_R;

   H.Reset;
   H.Add (Messages.If_None_Match_Token,
          String (Messages.Create_ETag (ETag (2))));
   AWS.Client.Get (HTTP, R, "/1", Headers => H);
   Print_R;

   --  If-Match

   H.Add
     (Messages.If_Match_Token,
      String (Messages.Create_ETag (ETag (1))));
   AWS.Client.Get (HTTP, R, "/2", Headers => H);
   Print_R;

   H.Reset;
   H.Add (Messages.If_Match_Token,
          String (Messages.Create_ETag (ETag (2))));
   AWS.Client.Get (HTTP, R, "/2", Headers => H);
   Print_R;

   Server.Shutdown (WS);
   Text_IO.Put_Line ("shutdown");
end If_Match_Headers;
