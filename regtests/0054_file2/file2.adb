------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2003-2008, AdaCore                     --
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
with AWS.Net.Log.Callbacks;
with AWS.Response;
with AWS.Server.Log;
with AWS.Status;
with AWS.Translator;
with AWS.URL;
with AWS.Utils;

with zresres;

with ZLib;
with Get_Free_Port;

procedure File2 is

   use Ada;
   use AWS;

   WS   : Server.HTTP;
   Port : Natural := 4569;

   procedure Call_It;

   function CB (Request : in Status.Data) return Response.Data;

   -------------
   -- Call_It --
   -------------

   procedure Call_It is
      R       : Response.Data;
      Connect : Client.HTTP_Connection;
      Ptr     : Utils.Stream_Element_Array_Access;

   begin
      Client.Create (Connect, "http://localhost:" & Utils.Image (Port));

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

      Utils.Free (Ptr);
   end Call_It;

   --------
   -- CB --
   --------

   function CB (Request : in Status.Data) return Response.Data is
      Filename : constant String := URL.File (Status.URI (Request));
   begin
      return Response.File (MIME.Text_HTML, Filename);
   end CB;

begin
   Get_Free_Port (Port);

   Net.Log.Callbacks.Initialize
     ("file2.netlog", Net.Log.Callbacks.Binary'Access);

   Server.Start
     (WS, "file", CB'Unrestricted_Access, Port => Port, Max_Connection => 5);

   Server.Log.Start_Error (WS);

   Text_IO.Put_Line ("started"); Ada.Text_IO.Flush;

   delay 1.0;

   Call_It;

   Server.Shutdown (WS);

   Net.Log.Callbacks.Finalize;

   Text_IO.Put_Line ("shutdown");
end File2;
