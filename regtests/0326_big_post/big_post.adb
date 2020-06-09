------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                       Copyright (C) 2020, AdaCore                        --
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

with Ada.Streams;
with Ada.Text_IO;

with AWS.Client;
with AWS.Messages;
with AWS.MIME;
with AWS.Net.Log;
with AWS.Parameters;
with AWS.Response;
with AWS.Server.Status;
with AWS.Status;
with AWS.Translator;
with AWS.Utils;

procedure Big_Post is

   use Ada;
   use Ada.Streams;
   use AWS;

   type String_Access is access String;

   --------
   -- CB --
   --------

   function CB (Request : Status.Data) return Response.Data is
   begin
      if not Status.Is_Body_Uploaded (Request) then
         Server.Get_Message_Body;
      end if;

      Text_IO.Put_Line
        ("Length payload: " & Status.Content_Length (Request)'Img);
      return Response.Build (MIME.Text_HTML, "ok");
   end CB;

   WS     : Server.HTTP;
   M_Body : String_Access;
   R      : Response.Data;

begin
   Server.Start (WS, "Big Post", CB'Unrestricted_Access, Port => 0);

   Text_IO.Put_Line ("started");
   Text_IO.Flush;

   M_Body := new String'(1 .. 16_000_000 => 'a');

   R := Client.Post
          (Server.Status.Local_URL (WS) & "/big_post",
           M_Body.all,
           Content_Type => MIME.Text_HTML);

   Text_IO.Put_Line (Response.Message_Body (R));

   Server.Shutdown (WS);
   Text_IO.Put_Line ("shutdown");
end Big_Post;
