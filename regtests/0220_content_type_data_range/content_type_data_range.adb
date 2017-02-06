------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2011-2017, AdaCore                     --
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
with AWS.Containers.Tables;
with AWS.Headers;
with AWS.MIME;
with AWS.Response;
with AWS.Server.Status;
with AWS.Status;
with AWS.Utils;

procedure Content_Type_Data_Range is

   use Ada;
   use Ada.Text_IO;
   use AWS;

   function CB (Request : AWS.Status.Data) return AWS.Response.Data is
      Data : Response.Data;
      URI  : constant String := Status.URI (Request);
   begin
      Text_IO.Put_Line ("Server/ uri " & URI);

      return Response.File
        (Content_Type => MIME.Content_Type (URI),
         Filename     => URI (URI'First + 1 .. URI'Last));
   end CB;

   WS            : Server.HTTP;
   Conf          : Config.Object;
   Client_Data   : Response.Data;
   Client_Header : Headers.List;
   CHeader       : Containers.Tables.Table_Type
                     renames Containers.Tables.Table_Type (Client_Header);

   ------------------
   -- Send_Request --
   ------------------

   procedure Send_Request is
   begin
      Client_Data := Client.Get
                       (AWS.Server.Status.Local_URL (WS) & "/test.txt",
                        Headers => Client_Header);
   exception
      when others =>
         --  An exception is raised because the client API does not support
         --  data ranges.
         null;
   end Send_Request;

begin
   Config.Set.Server_Host (Conf, "localhost");
   Config.Set.Server_Port (Conf, 0);

   Headers.Debug (True);

   Server.Start
     (Web_Server => WS,
      Callback   => CB'Unrestricted_Access,
      Config     => Conf);

   CHeader.Add (Name => "Accept",        Value => "*/*");
   CHeader.Add (Name => "User-Agent",    Value => "Fake bzr/2.3.4 (urllib)");
   CHeader.Add (Name => "Pragma",        Value => "no-cache");
   CHeader.Add (Name => "Cache-Control", Value => "max-age=0");
   CHeader.Add (Name => "Range",         Value => "bytes=0-41,46-80");
   CHeader.Add (Name => "Host",
                Value => "localhost:" & Utils.Image (Server.Status.Port (WS)));
   Send_Request;

   CHeader.Update (Name => "Range", Value => "bytes=220-888");
   Send_Request;

   Server.Shutdown (WS);
end Content_Type_Data_Range;
