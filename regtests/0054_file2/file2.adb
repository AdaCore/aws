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

with AWS.Client;
with AWS.MIME;
with AWS.Net.Log.Callbacks;
with AWS.Response;
with AWS.Server.Log;
with AWS.Server.Status;
with AWS.Status;
with AWS.Translator;
with AWS.URL;
with AWS.Utils;

with zresres;

with ZLib;

procedure File2 is

   use Ada;
   use AWS;

   WS : Server.HTTP;

   procedure Call_It;

   function CB (Request : Status.Data) return Response.Data;

   -------------
   -- Call_It --
   -------------

   procedure Call_It is
      R       : Response.Data;
      Connect : Client.HTTP_Connection;
      Ptr     : Utils.Stream_Element_Array_Access;

   begin
      Client.Create (Connect, Server.Status.Local_URL (WS));

      Client.Get (Connect, R, "file1.txt");

      Text_IO.Put_Line ("-- file1.txt --");
      Text_IO.Put_Line (Response.Message_Body (R));

      Client.Get (Connect, R, "file2.txt");

      --  Current implementation does not send Content-Encoding: gzip
      --  so the client side is not decoding it.

      Text_IO.Put_Line ("-- file2.txt --");
      Text_IO.Put_Line (Response.Message_Body (R));

      Client.Get (Connect, R, "file2.txt.gz");

      Client.Close (Connect);

      Ptr := Translator.Decompress
               (Response.Message_Body (R), Header => ZLib.GZip);

      Text_IO.Put_Line ("-- file2.txt.gz unzipped --");
      Text_IO.Put_Line (Translator.To_String (Ptr.all));

      Utils.Unchecked_Free (Ptr);
   end Call_It;

   --------
   -- CB --
   --------

   function CB (Request : Status.Data) return Response.Data is
      Filename : constant String := URL.File (Status.URI (Request));
   begin
      return Response.File (MIME.Text_HTML, Filename);
   end CB;

begin
   Net.Log.Callbacks.Initialize
     ("file2.netlog", Net.Log.Callbacks.Binary'Access);

   Server.Start
     (WS, "file", CB'Unrestricted_Access, Port => 0, Max_Connection => 5);

   Server.Log.Start_Error (WS);

   Text_IO.Put_Line ("started"); Ada.Text_IO.Flush;

   delay 1.0;

   Call_It;

   Server.Shutdown (WS);

   Net.Log.Callbacks.Finalize;

   Text_IO.Put_Line ("shutdown");
end File2;
