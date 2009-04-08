------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2003-2009, AdaCore                     --
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

procedure Once is

   use Ada;
   use AWS;

   WS   : Server.HTTP;
   Port : Natural := 1269;

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
      R := Client.Get ("http://localhost:" & Utils.Image (Port) & "/once");

      if Response.Status_Code (R) = Messages.S404 then
         Text_IO.Put_Line ("404 not found");
      else
         Text_IO.Put_Line
           ("Found: " & Response.Message_Body (R));
      end if;
   end Call_It;

   File : Text_IO.File_Type;

begin
   Get_Free_Port (Port);

   Server.Start
     (WS, "once", CB'Unrestricted_Access, Port => Port, Max_Connection => 5);
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
