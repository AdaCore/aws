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

with Ada.Calendar;
with Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Text_IO;

with AWS.Client;
with AWS.MIME;
with AWS.Response;
with AWS.Server;
with AWS.Session;
with AWS.Status;
with AWS.Utils;

with Get_Free_Port;

procedure Sessions3 is

   use Ada;
   use AWS;

   WS   : Server.HTTP;
   Port : Natural := 1260;

   C : Client.HTTP_Connection;
   R : Response.Data;

   task Server is
      entry Started;
      entry Stop;
   end Server;

   --------
   -- CB --
   --------

   function CB (Request : Status.Data) return Response.Data is
      SID : constant Session.ID := Status.Session (Request);
      N   : Natural := 0;
   begin
      if Session.Exist (SID, "toto") then
         N := Session.Get (SID, "toto");
         N := N + 1;
      end if;

      Session.Set (SID, "toto", N);

      return Response.Build
        (MIME.Text_HTML, "Ok, this is call " & Natural'Image (N));
   end CB;

   -----------
   -- Image --
   -----------

   function Image (Str : String) return String is
      Last : constant Natural := Strings.Fixed.Index (Str, "activated.");
   begin
      --  Remove stack traceback information
      return Str (Str'First .. Last + 10) & Str (Str'Last - 13 .. Str'Last);
   end Image;

   ------------
   -- Server --
   ------------

   task body Server is
   begin
      Get_Free_Port (Port);

      AWS.Server.Start
        (WS, "session",
         CB'Unrestricted_Access,
         Port           => Port,
         Max_Connection => 5,
         Session        => False);

      Ada.Text_IO.Put_Line ("started");

      accept Started;

      accept Stop;

      Ada.Text_IO.Put_Line ("Ready to stop");
   exception
      when E : others =>
         Text_IO.Put_Line (Exceptions.Exception_Information (E));
   end Server;

begin
   Server.Started;

   Client.Create (C, "http://localhost:" & Utils.Image (Port));

   Client.Get (C, R, "/");
   Ada.Text_IO.Put_Line ("Response : " & Image (Response.Message_Body (R)));

   Client.Get (C, R, "/");
   Ada.Text_IO.Put_Line ("Response : " & Image (Response.Message_Body (R)));

   Server.Stop;

   AWS.Server.Shutdown (WS);

   Client.Close (C);

   Session.Clear;
   Ada.Text_IO.Put_Line ("shutdown");

exception
   when E : others =>
      Text_IO.Put_Line (Exceptions.Exception_Information (E));
end Sessions3;
