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

with Ada.Calendar;
with Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Text_IO;

with AWS.Client;
with AWS.MIME;
with AWS.Response;
with AWS.Server.Status;
with AWS.Session;
with AWS.Status;
with AWS.Utils;

procedure Sessions3 is

   use Ada;
   use AWS;

   WS : Server.HTTP;

   C : Client.HTTP_Connection;
   R : Response.Data;

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

begin
   Server.Start
     (WS, "session", CB'Unrestricted_Access,
      Port           => 0,
      Max_Connection => 5,
      Session        => False);

   Text_IO.Put_Line ("started");

   Client.Create (C, AWS.Server.Status.Local_URL (WS));

   Client.Get (C, R, "/");
   Text_IO.Put_Line ("Response : " & Image (Response.Message_Body (R)));

   Client.Get (C, R, "/");
   Text_IO.Put_Line ("Response : " & Image (Response.Message_Body (R)));

   Text_IO.Put_Line ("Ready to stop");

   Server.Shutdown (WS);

   Client.Close (C);

   Session.Clear;
   Text_IO.Put_Line ("shutdown");

exception
   when E : others =>
      Text_IO.Put_Line (Exceptions.Exception_Information (E));
      Server.Shutdown (WS);
end Sessions3;
