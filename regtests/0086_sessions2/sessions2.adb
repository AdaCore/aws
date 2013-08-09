------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2003-2013, AdaCore                     --
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
with Ada.Text_IO;

with AWS.Client;
with AWS.Config.Set;
with AWS.MIME;
with AWS.Parameters;
with AWS.Response;
with AWS.Server.Status;
with AWS.Session;
with AWS.Status;
with AWS.Utils;

with Stack_Size;

procedure Sessions2 is

   use Ada;
   use AWS;

   WS : Server.HTTP;

   task type T_Client is
      pragma Storage_Size (Stack_Size.Value);
      entry Start (N : Positive);
      entry Stopped;
   end T_Client;

   Clients : array (1 .. 5) of T_Client;

   --------
   -- CB --
   --------

   function CB (Request : Status.Data) return Response.Data is
      SID : constant Session.ID      := Status.Session (Request);
      Key : constant String := "key";
      N   : Natural := 0;
   begin
      if Session.Exist (SID, Key) then
         N := Session.Get (SID, Key);
         N := N + 1;
      end if;

      Session.Set (SID, Key, N);

      return Response.Build
        (MIME.Text_HTML, "Ok, this is call " & Natural'Image (N));
   end CB;

   --------------
   -- T_Client --
   --------------

   task body T_Client is
      R : Response.Data;
      C : Client.HTTP_Connection;
      N : Positive;
   begin
      accept Start (N : Positive) do
         T_Client.N := N;
      end Start;

      Client.Create (C, AWS.Server.Status.Local_URL (WS));

      for K in 1 .. 10 loop
         Client.Get (C, R, "/");
         delay 0.1;
      end loop;

      accept Stopped;

      Client.Close (C);

   exception
      when E : others =>
         Text_IO.Put_Line (Exceptions.Exception_Information (E));
   end T_Client;

   ----------------
   -- Delete_SID --
   ----------------

   procedure Delete_SID (SID : Session.ID) is

      procedure Display_Session_Data
        (N          : Positive;
         Key, Value : String;
         Kind       : Session.Value_Kind;
         Quit       : in out Boolean) is
      begin
         Text_IO.Put_Line ("   " & Key & " = " & Value);
      end Display_Session_Data;

      procedure Display_Data is
         new Session.For_Every_Session_Data (Display_Session_Data);

   begin
      Text_IO.Put_Line ("New SID");
      Display_Data (SID);
      Text_IO.Flush;
   end Delete_SID;

begin
   Config.Set.Session_Cleanup_Interval (3.0);
   Config.Set.Session_Lifetime (2.0);

   Server.Start
     (WS, "session", CB'Unrestricted_Access,
      Port           => 0,
      Max_Connection => 5,
      Session        => True);

   Text_IO.Put_Line ("started");

   Session.Set_Callback (Delete_SID'Unrestricted_Access);

   for K in Clients'Range loop
      Clients (K).Start (K);
   end loop;

   delay 1.0;

   for K in Clients'Range loop
      Clients (K).Stopped;
   end loop;

   delay 5.0;

   Session.Set_Callback (null);

   delay 1.0;

   Text_IO.Put_Line ("Ready to stop");

   Server.Shutdown (WS);

   Session.Clear;
   Text_IO.Put_Line ("shutdown");
end Sessions2;
