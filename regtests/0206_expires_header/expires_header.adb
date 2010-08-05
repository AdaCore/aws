------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                       Copyright (C) 2010, AdaCore                        --
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

with Ada.Calendar;
with Ada.Text_IO;

with AWS.Client;
with AWS.Messages;
with AWS.MIME;
with AWS.Response.Set;
with AWS.Server;
with AWS.Status;
with AWS.Utils;

with Get_Free_Port;

procedure Expires_Header is

   use Ada;
   use AWS;

   WS   : Server.HTTP;
   Port : Positive := 8275;

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

   R : Response.Data;

begin
   Get_Free_Port (Port);

   Server.Start
     (WS, "Expires Header", CB'Unrestricted_Access, Port => Port);

   R := AWS.Client.Get
     (URL => "http://localhost:" & Utils.Image (Port) & "/get");

   Text_IO.Put_Line
     ("Expires response : '"
      & Messages.To_HTTP_Date (Response.Expires (R)) & ''');

   R := AWS.Client.Head
     (URL => "http://localhost:" & Utils.Image (Port) & "/head");

   Text_IO.Put_Line
     ("Expires response : '"
      & Messages.To_HTTP_Date (Response.Expires (R)) & ''');

   R := AWS.Client.Post
     (URL => "http://localhost:" & Utils.Image (Port) & "/post", Data => "");

   Text_IO.Put_Line
     ("Expires response : '"
      & Messages.To_HTTP_Date (Response.Expires (R)) & ''');

   Text_IO.Put_Line ("Expirers Header : '" & Messages.Expires (D) & ''');

   Server.Shutdown (WS);
   Text_IO.Put_Line ("shutdown");
end Expires_Header;
