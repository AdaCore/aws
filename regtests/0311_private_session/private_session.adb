------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                       Copyright (C) 2017, AdaCore                        --
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

with GNAT.SHA256;

with AWS.Client;
with AWS.Config.Set;
with AWS.Headers.Set;
with AWS.Messages;
with AWS.MIME;
with AWS.Response;
with AWS.Server.Status;
with AWS.Session;
with AWS.Status;

procedure Private_Session is

   use Ada;
   use GNAT;

   use AWS;

   WS   : Server.HTTP;
   Conf : Config.Object;

   SID   : Session.Id := Session.No_Session;
   P_SID : String (1 .. SHA256.Message_Digest'Length);
   --  The compromise session

   function CB (Request : Status.Data) return Response.Data;

   task A is
      entry Start;
      entry Done;
      entry Stop;
   end A;

   task B is
      entry Stop;
   end B;

   --------
   -- CB --
   --------

   function CB (Request : Status.Data) return Response.Data is
      use type Session.Id;

      SID : constant Session.Id := Status.Session (Request);
      Key : constant String := "key";
      N   : Natural := 0;
   begin
      if Private_Session.SID = Session.No_Session then
         Private_Session.SID := SID;
         P_SID := Status.Session_Private (Request);
      end if;

      if Session.Exist (SID, Key) then
         N := Session.Get (SID, Key);
         N := N + 1;
      end if;

      Session.Set (SID, Key, N);

      return Response.Build (MIME.Text_HTML, "N =" & Natural'Image (N));
   end CB;

   -------
   -- A --
   -------

   task body A is
      R : Response.Data;
      H : Headers.List;
      C : Client.HTTP_Connection;
   begin
      accept Start;

      Client.Create (C, AWS.Server.Status.Local_URL (WS));

      Client.Get (C, R, "/a");

      --  This works, but is not needed, the proper session's keys are
      --  automaticly handled by AWS. We specify them here to ensure that
      --  only that is working compared to what is done in B where only SID
      --  has been compromized.

      Headers.Add
        (H, Messages.Cookie_Token,
         "ASN=" & Session.Image (SID) & ", AWS_Private=" & P_SID);

      accept Done;

      for K in 1 .. 10 loop
         Client.Get (C, R, "/a", Headers => H);
      end loop;

      --  Print result

      Text_IO.Put_Line ("A: " & Response.Message_Body (R));

      accept Stop;
   end A;

   -------
   -- B --
   -------

   task body B is
      R : Response.Data;
      H : Headers.List;
      C : Client.HTTP_Connection;
   begin
      A.Done;

      Client.Create (C, AWS.Server.Status.Local_URL (WS));

      --  Simulate a compromised session, lets use A's ASN session key

      Headers.Add (H, Messages.Cookie_Token, "ASN=" & Session.Image (SID));

      for K in 1 .. 10 loop
         Client.Get (C, R, "/b", Headers => H);
      end loop;

      A.Stop;

      Text_IO.Put_Line ("B: " & Response.Message_Body (R));

      accept Stop;
   end B;

begin
   Ada.Text_IO.Put_Line ("start");

   Config.Set.Server_Name    (Conf, "Session_Hack");
   Config.Set.Server_Port    (Conf, 0);
   Config.Set.Max_Connection (Conf, 5);
   Config.Set.Session        (Conf, True);
   Config.Set.Session_Name   (Conf, "ASN");

   Server.Start (WS, CB'Unrestricted_Access, Conf);

   A.Start;

   B.Stop;

   Server.Shutdown (WS);

   Ada.Text_IO.Put_Line ("shutdown");
end Private_Session;
