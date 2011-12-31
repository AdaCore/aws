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
with AWS.MIME;
with AWS.Resources;
with AWS.Response.Set;
with AWS.Server;
with AWS.Status;
with AWS.Utils;
with AWS.Messages;

with Get_Free_Port;

procedure Stream_Response is

   use Ada;
   use AWS;

   WS    : Server.HTTP;
   Port  : Positive := 8167;
   Index : Natural := 0;
   Cnx   : Client.HTTP_Connection;

   function CB (Request : Status.Data) return Response.Data is
   begin
      Index := Index + 1;
      return Response.Build
        (MIME.Text_HTML,
         "This is the response number" & Positive'Image (Index));
   end CB;

   procedure Call_It is
      use type Response.Content_Length_Type;
      R : Response.Data;
   begin
      --  Standard

      R := Client.Get ("http://localhost:" & Utils.Image (Port));

      Text_IO.Put_Line
        ("RC " & " : "
           & Messages.Status_Code'Image (Response.Status_Code (R)));
      Text_IO.Put_Line
        ("=> " & Response.Message_Body (R));

      declare
         S      : Resources.File_Type;
         Buffer : String (1 .. 1_024);
         Last   : Natural;
      begin
         Response.Message_Body (R, S);
         Resources.Get_Line (S, Buffer, Last);
         Text_IO.Put_Line ("+> " & Buffer (1 .. Last));
      end;

      --  Persistent

      Response.Set.Clear (R);
      Client.Get (Cnx, R);

      Text_IO.Put_Line
        ("RC " & " : "
           & Messages.Status_Code'Image (Response.Status_Code (R)));
      Text_IO.Put_Line
        ("=> " & Response.Message_Body (R));

      declare
         S      : Resources.File_Type;
         Buffer : String (1 .. 1_024);
         Last   : Natural;
      begin
         Response.Message_Body (R, S);
         Resources.Get_Line (S, Buffer, Last);
         Text_IO.Put_Line ("+> " & Buffer (1 .. Last));
         Resources.Close (S);
      end;
   end Call_It;

begin
   Get_Free_Port (Port);

   Server.Start
     (WS, "stream_response",
      CB'Unrestricted_Access,
      Port           => Port,
      Max_Connection => 5);
   Text_IO.Put_Line ("started"); Ada.Text_IO.Flush;

   delay 1.0;

   Client.Create (Cnx, "http://localhost:" & Utils.Image (Port));

   for K in 1 .. 5 loop
      Call_It;
   end loop;

   Server.Shutdown (WS);
   Text_IO.Put_Line ("shutdown");
end Stream_Response;
