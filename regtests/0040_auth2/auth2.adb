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

with Ada.Text_IO;
with Ada.Exceptions;

with AWS.Server.Status;
with AWS.Client;
with AWS.Config.Set;
with AWS.Status;
with AWS.MIME;
with AWS.Response;
with AWS.Messages;
with AWS.Utils;

procedure Auth2 is

   use Ada;
   use Ada.Text_IO;
   use AWS;

   function CB (Request : Status.Data) return Response.Data;

   HTTP : AWS.Server.HTTP;
   CNF  : Config.Object;
   R    : Response.Data;

   --------
   -- CB --
   --------

   function CB (Request : Status.Data) return Response.Data is
      Username : constant String
        := AWS.Status.Authorization_Name (Request);
      Password : constant String
        := AWS.Status.Authorization_Password (Request);
   begin
      return AWS.Response.Build
        ("text/plain", Username & " - " & Password);
   end CB;

begin
   Config.Set.Server_Name    (CNF, "Test authentication.");
   Config.Set.Server_Host    (CNF, "localhost");
   Config.Set.Server_Port    (CNF, 0);
   Config.Set.Max_Connection (CNF, 3);

   AWS.Server.Start (HTTP, CB'Unrestricted_Access, CNF);

   R := Client.Get (AWS.Server.Status.Local_URL (HTTP), "toto", "toto_pwd");
   Text_IO.Put_Line (Response.Message_Body (R));

   R := Client.Get (AWS.Server.Status.Local_URL (HTTP), "xyz", "_123_");
   Text_IO.Put_Line (Response.Message_Body (R));

   AWS.Server.Shutdown (HTTP);

exception
   when E : others =>
      Put_Line ("Main Error " & Exceptions.Exception_Information (E));
end Auth2;
