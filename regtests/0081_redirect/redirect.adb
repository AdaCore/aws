------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2003-2008, AdaCore                     --
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

with Ada.Text_IO;

with AWS.Client;
with AWS.Messages;
with AWS.MIME;
with AWS.Response;
with AWS.Server;
with AWS.Status;
with AWS.Utils;

with Get_Free_Port;

procedure Redirect is

   use Ada;
   use AWS;
   use type AWS.Messages.Status_Code;

   WS   : Server.HTTP;
   Port : Natural := 1239;

   --------
   -- CB --
   --------

   function CB (Request : in Status.Data) return Response.Data is
      URI : constant String := Status.URI (Request);
   begin
      if URI = "/first" then
         return Response.URL ("/second");

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
      R := Client.Get ("http://localhost:" & Utils.Image (Port) & "/first");

      if Response.Status_Code (R) = Messages.S302 then
         Text_IO.Put_Line ("OK, status is good");

      else
         Text_IO.Put_Line
           ("NOK, wrong status "
            & Messages.Image (Response.Status_Code (R)));
      end if;

      Text_IO.Put_Line (Response.Location (R));
   end Call_It;

begin
   Get_Free_Port (Port);

   Server.Start
     (WS, "file", CB'Unrestricted_Access, Port => Port, Max_Connection => 5);
   Text_IO.Put_Line ("started"); Ada.Text_IO.Flush;

   Call_It;

   Server.Shutdown (WS);
   Text_IO.Put_Line ("shutdown");
end Redirect;
