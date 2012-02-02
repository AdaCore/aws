------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2011-2012, AdaCore                     --
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
with AWS.Containers.Tables.Set;
with AWS.Headers.Set;
with AWS.MIME;
with AWS.Response;
with AWS.Server.Status;
with AWS.Status;
with AWS.Utils;

with Get_Free_Port;

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
   Port          : Natural := 2745;
   Conf          : Config.Object;
   Client_Data   : Response.Data;
   Client_Header : Headers.List;
   CHeader       : Containers.Tables.Table_Type
                     renames Containers.Tables.Table_Type (Client_Header);
begin
   Get_Free_Port (Port);

   Config.Set.Server_Port (Conf, Port);

   Headers.Set.Debug (True);

   Server.Start
     (Web_Server => WS,
      Callback   => CB'Unrestricted_Access,
      Config     => Conf);

   Containers.Tables.Set.Add
     (CHeader,
      Name  => "Accept",
      Value => "*/*");

   Containers.Tables.Set.Add
     (CHeader,
      Name  => "User-Agent",
      Value => "Fake bzr/2.3.4 (urllib)");
   Containers.Tables.Set.Add
     (CHeader,
      Name  => "Host",
      Value => "localhost:" & Utils.Image (Port));
   Containers.Tables.Set.Add
     (CHeader,
      Name  => "Pragma",
      Value => "no-cache");
   Containers.Tables.Set.Add
     (CHeader,
      Name  => "Cache-Control",
      Value => "max-age=0");
   Containers.Tables.Set.Add
     (CHeader,
      Name  => "Range",
      Value => "bytes=0-41,46-80");

   Client_Data := Client.Get
     ("http://" & AWS.Server.Status.Host (WS) & ':' & Utils.Image (Port)
        & "/test.txt",
      Headers => Client_Header);

exception
   when others =>
      --  An exception is raised because the client API does not support
      --  data ranges.
      Server.Shutdown (WS);
end Content_Type_Data_Range;
