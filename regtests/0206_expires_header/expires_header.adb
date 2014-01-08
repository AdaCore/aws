------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2010-2014, AdaCore                     --
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

with Ada.Calendar;
with Ada.Text_IO;

with AWS.Client;
with AWS.Config.Set;
with AWS.Messages;
with AWS.MIME;
with AWS.Response.Set;
with AWS.Server.Status;
with AWS.Status;
with AWS.Utils;

procedure Expires_Header is

   use Ada;
   use AWS;

   WS   : Server.HTTP;
   CNF  : Config.Object;

   Date : constant String := "Tue, 15 Nov 1994 08:12:31 GMT";
   D    : constant Calendar.Time := Messages.To_Time  (Date);

   --------
   -- CB --
   --------

   function CB (Request : Status.Data) return Response.Data is
      R : Response.Data;
   begin
      Text_IO.Put_Line (">>>>> " & Status.URI (Request));

      R := Response.Build (MIME.Text_HTML, "ok");
      Response.Set.Expires (R, D);
      return R;
   end CB;

   R  : Response.Data;
   WC : Client.HTTP_Connection;

begin
   Config.Set.Server_Name (CNF, "Expires Header");
   Config.Set.Server_Host (CNF, "localhost");
   Config.Set.Server_Port (CNF, 0);

   Server.Start (WS, CB'Unrestricted_Access, CNF);

   Client.Create (WC, Server.Status.Local_URL (WS));

   Client.Get (WC, R, URI => "/get");

   Text_IO.Put_Line
     ("Expires response : '"
      & Messages.To_HTTP_Date (Response.Expires (R)) & ''');

   Client.Head (WC, R, URI => "/head");

   Text_IO.Put_Line
     ("Expires response : '"
      & Messages.To_HTTP_Date (Response.Expires (R)) & ''');

   Client.Post (WC, R, URI => "/post", Data => "");

   Text_IO.Put_Line
     ("Expires response : '"
      & Messages.To_HTTP_Date (Response.Expires (R)) & ''');

   Text_IO.Put_Line ("Expirers Header : '" & Messages.Expires (D) & ''');

   Client.Close (WC);

   Server.Shutdown (WS);
   Text_IO.Put_Line ("shutdown");
end Expires_Header;
