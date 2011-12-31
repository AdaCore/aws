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

--  Compile this application, then run it with:
--
--  $ ./download_manager [<Mb>]
--
--  <Mb> is the size in Mb of the file to download, default to 100Mb
--

with Ada.Command_Line;
with Ada.Numerics.Discrete_Random;
with Ada.Text_IO;
with Ada.Streams.Stream_IO;

with GNAT.MD5;

with AWS.Config.Set;
with AWS.Messages;
with AWS.MIME;
with AWS.Resources.Streams.Disk;
with AWS.Response;
with AWS.Server;
with AWS.Services.Download;
with AWS.Services.Dispatchers.Linker;
with AWS.Services.Dispatchers.URI;
with AWS.Status;
with AWS.Translator;

procedure Download_Manager is

   use Ada;
   use Ada.Streams;
   use Ada.Text_IO;
   use AWS;

   function CB (Request : Status.Data) return Response.Data;

   Filename : constant String := "dm_file.data";
   Size     : Positive := 100;

   procedure Create_Filename;
   --  Generate file content and print the MD5 signature, this can be used to
   --  check if the file has properly been downloaded.

   --------
   -- CB --
   --------

   function CB (Request : Status.Data) return Response.Data is
      URI    : constant String := Status.URI (Request);
      Stream : Resources.Streams.Stream_Access;
   begin
      if URI = "/" then
         return Response.File (MIME.Text_HTML, "main.html");

      elsif URI = "/download_file" then
         Stream := new Resources.Streams.Disk.Stream_Type;
         Resources.Streams.Disk.Open
           (Resources.Streams.Disk.Stream_Type (Stream.all), Filename);
         return Services.Download.Build (Request, Filename, Stream);

      else
         return Response.Acknowledge (Messages.S404, "Not found");
      end if;
   end CB;

   ---------------------
   -- Create_Filename --
   ---------------------

   procedure Create_Filename is
      use GNAT;
      subtype Buffer is String (1 .. 1_024);

      package Char_Random is new Numerics.Discrete_Random (Character);
      use Char_Random;

      Rand_Generator : Char_Random.Generator;

      Ctx  : MD5.Context;
      File : Stream_IO.File_Type;
      Str  : Buffer;
   begin
      Reset (Rand_Generator);

      Stream_IO.Create (File, Stream_IO.Out_File, Filename);

      for K in 1 .. Size * 1_000 loop
         for K in Buffer'Range loop
            Str (K) := Random (Rand_Generator);
         end loop;
         MD5.Update (Ctx, Str);
         Stream_IO.Write (File, Translator.To_Stream_Element_Array (Str));
      end loop;

      Stream_IO.Close (File);

      Text_IO.Put_Line ("File MD5 is " & MD5.Digest (Ctx));
   end Create_Filename;

   URI     : Services.Dispatchers.URI.Handler;
   Handler : Services.Dispatchers.Linker.Handler;

   Conf : Config.Object := Config.Get_Current;
   WS   : Server.HTTP;

begin
   if Command_Line.Argument_Count = 1 then
      Size := Positive'Value (Command_Line.Argument (1));
   end if;

   Create_Filename;

   Config.Set.Server_Port (Conf, 8080);
   Config.Set.Reuse_Address (Conf, True);

   Services.Dispatchers.URI.Register
     (URI, "/", CB'Unrestricted_Access);
   Services.Dispatchers.URI.Register
     (URI, "/download_file", CB'Unrestricted_Access);

   Text_IO.Put_Line ("Start download server...");

   Services.Download.Start (URI, Handler, 1);

   Text_IO.Put_Line ("Start main server...");

   Server.Start (WS, Handler, Conf);

   Text_IO.Put_Line ("Press Q to quit...");
   Server.Wait (Server.Q_Key_Pressed);

   Server.Shutdown (WS);
   Services.Download.Stop;
end Download_Manager;
