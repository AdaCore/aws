------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                       Copyright (C) 2018, AdaCore                        --
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

with AWS.Net.Buffered;
with AWS.Net.Std;
with AWS.Response;
with AWS.Server.Status;
with AWS.Status;
with AWS.Status;

procedure Chunked_Form is

   use Ada;
   use AWS;

   Done : Boolean := False;

   -------------
   -- Service --
   -------------

   function Service (Request : Status.Data) return Response.Data is
      URI  : constant String := Status.URI (Request);
      Foo  : constant String := Status.Parameter (Request, "foo");
      Test : constant String := Status.Parameter (Request, "test");
   begin
      Text_IO.Put_Line (URI);
      Text_IO.Put_Line ("foo=" & Foo);
      Text_IO.Put_Line ("test=" & Test);
      Done := True;
      return Response.Build (Content_Type => "text/html",
                             Message_Body => "Hello, world!");
   end;

   WS : Server.HTTP;
   S  : Net.Socket_Type'Class := Net.Socket (False);

begin
   Server.Start
     (Web_Server => WS,
      Name       => "test",
      Callback   => Service'Unrestricted_Access,
      Port       => 0);

   S.Connect
     (Net.Localhost (Server.Status.Is_IPv6 (WS)), Server.Status.Port (WS));

   Net.Buffered.Put_Line (S, "POST / HTTP/1.1");
   Net.Buffered.Put_Line (S, "Host: 127.0.0.1:8383");
   Net.Buffered.Put_Line (S, "User-Agent: curl/7.61.0");
   Net.Buffered.Put_Line (S, "Accept: */*");
   Net.Buffered.Put_Line (S, "Transfer-Encoding: chunked");
   Net.Buffered.Put_Line
     (S, "Content-Type: application/x-www-form-urlencoded");
   Net.Buffered.New_Line (S);
   Net.Buffered.Put_Line (S, "15");
   Net.Buffered.Put_Line (S, "foo=bar&test=testdata");
   Net.Buffered.Put_Line (S, "0");
   Net.Buffered.New_Line (S);
   Net.Buffered.Flush (S);

   while not Done loop
      delay 1.0;
   end loop;

   Net.Shutdown (S);
   Server.Shutdown (WS);
end Chunked_Form;
