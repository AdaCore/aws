------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                        Copyright (C) 2011, AdaCore                       --
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
with AWS.MIME;
with AWS.Resources.Streams.Disk;
with AWS.Response;
with AWS.Server;
with AWS.Status;
with AWS.Utils;
with AWS.Messages;

with Get_Free_Port;

procedure Stream_Not_Found is

   use Ada;
   use AWS;

   WS   : Server.HTTP;
   Port : Positive := 8567;

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
      R := Client.Get ("http://localhost:" & Utils.Image (Port) & URL);

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
   Get_Free_Port (Port);

   Server.Start
     (WS, "stream_not_found",
      CB'Unrestricted_Access,
      Port           => Port,
      Max_Connection => 5);
   Text_IO.Put_Line ("started"); Ada.Text_IO.Flush;

   delay 1.0;

   Call_It ("/data");
   Call_It ("/zdata");

   Server.Shutdown (WS);
   Text_IO.Put_Line ("shutdown");
end Stream_Not_Found;
