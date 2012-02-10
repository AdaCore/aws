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
with AWS.MIME;
with AWS.Resources.Streams.Disk;
with AWS.Response;
with AWS.Server.Status;
with AWS.Status;
with AWS.Utils;
with AWS.Messages;

procedure Stream_Not_Found is

   use Ada;
   use AWS;

   WS  : Server.HTTP;
   CNF : Config.Object;

   function CB (Request : Status.Data) return Response.Data is
      URI  : constant String := Status.URI (Request);
      Strm : Resources.Streams.Stream_Access;
   begin
      if URI = "/data" then
         Strm := new Resources.Streams.Disk.Stream_Type;

         Resources.Streams.Disk.Open
           (Resources.Streams.Disk.Stream_Type (Strm.all),
            "data2.txt");

         return Response.Stream (MIME.Text_Plain, Strm);

      elsif URI = "/zdata" then
         Strm := new Resources.Streams.Disk.Stream_Type;

         Resources.Streams.Disk.Open
           (Resources.Streams.Disk.Stream_Type (Strm.all),
            "data2.txt.gz");

         return Response.Stream
           (MIME.Text_Plain, Strm, Encoding => Messages.GZip);
      else
         return Response.Build (MIME.Text_HTML, "URI not supported");
      end if;
   end CB;

   procedure Call_It (URL : String) is
      use type Response.Content_Length_Type;
      R : Response.Data;
   begin
      R := Client.Get (Server.Status.Local_URL (WS) & URL);

      Text_IO.Put_Line
        ("RC " & URL & " : "
           & Messages.Status_Code'Image (Response.Status_Code (R)));

      if Response.Content_Length (R) = -1 then
         Text_IO.Put_Line ("OK length is -1");
      else
         Text_IO.Put_Line
           ("NOK length is " & Utils.Image (Response.Content_Length (R)));
         Text_IO.Put_Line
           ("=> " & Response.Message_Body (R));
      end if;
   end Call_It;

begin
   Config.Set.Server_Name    (CNF, "stream_not_found");
   Config.Set.Server_Host    (CNF, "localhost");
   Config.Set.Server_Port    (CNF, 0);
   Config.Set.Max_Connection (CNF, 5);

   Server.Start (WS, CB'Unrestricted_Access, CNF);
   Text_IO.Put_Line ("started"); Ada.Text_IO.Flush;

   delay 1.0;

   Call_It ("/data");
   Call_It ("/zdata");

   Server.Shutdown (WS);
   Text_IO.Put_Line ("shutdown");
end Stream_Not_Found;
