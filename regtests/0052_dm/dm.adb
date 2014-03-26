------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2005-2014, AdaCore                     --
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

with Ada.Numerics.Discrete_Random;
with Ada.Streams.Stream_IO;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with GNAT.MD5;

with AWS.Client;
with AWS.Config.Set;
with AWS.Messages;
with AWS.MIME;
with AWS.Resources.Streams.Disk;
with AWS.Response;
with AWS.Server.Status;
with AWS.Services.Dispatchers.Linker;
with AWS.Services.Dispatchers.URI;
with AWS.Services.Download;
with AWS.Status;
with AWS.Translator;
with AWS.Utils;

procedure DM is

   use Ada;
   use Ada.Streams;
   use Ada.Strings;
   use Ada.Strings.Unbounded;
   use Ada.Text_IO;

   use AWS;
   use GNAT;

   Debug : constant Boolean := False;

   Nb_Client : constant := 5;

   WS : Server.HTTP;

   type Download_Info is record
      Size      : Positive;
      Signature : MD5.Message_Digest;
   end record;

   function CB (Request : Status.Data) return Response.Data;

   task type Client is
      entry Start (N : Positive);
      entry Stop (Info : out Download_Info);
   end Client;

   Filename : constant String := "dm_file.data";

   procedure Put_Line (Str : String);
   --  Output Str if in Debug mode

   function Create_Filename return MD5.Message_Digest;
   --  Generate file content and return the MD5 signature

   function MD5_Signature
     (Content : Unbounded_String) return MD5.Message_Digest;
   --  Returns the MD5 signature for Content

   Some_Waiting : Boolean := False;
   Starting     : Natural := 0;
   Downloads    : Natural := 0;

   Signature    : MD5.Message_Digest;

   -----------------
   -- Char_Random --
   -----------------

   package Char_Random is new Numerics.Discrete_Random (Character);
   use Char_Random;

   Rand_Generator : Char_Random.Generator;

   --------
   -- CB --
   --------

   function CB (Request : Status.Data) return Response.Data is
      URI    : constant String := Status.URI (Request);
      Stream : Resources.Streams.Stream_Access;
   begin
      if URI = "/welcome" then
         Text_IO.Put_Line ("/welcome");
         return Response.Build (MIME.Text_HTML, "welcome!");

      elsif URI = "/download_file" then
         Stream := new Resources.Streams.Disk.Stream_Type;
         Resources.Streams.Disk.Open
           (Resources.Streams.Disk.Stream_Type (Stream.all), Filename);
         return Services.Download.Build (Request, Filename, Stream);

      else
         return Response.Acknowledge (Messages.S404, "Not found");
      end if;
   end CB;

   ------------
   -- Client --
   ------------

   task body Client is
      use type Messages.Status_Code;
      URI  : Unbounded_String;
      R    : Response.Data;
      Code : Messages.Status_Code;
      N    : Positive;
      HTTP : AWS.Client.HTTP_Connection;

      procedure Get_R (URI : String);
      --  Get response for the specified URI, store the URI

      procedure Reload_R;
      --  Reload the previous URI

      ---------
      -- Get --
      ---------

      procedure Get_R (URI : String) is
      begin
         Client.URI := To_Unbounded_String (URI);
         AWS.Client.Get (HTTP, R, URI);
      end Get_R;

      ------------
      -- Reload --
      ------------

      procedure Reload_R is
      begin
         AWS.Client.Get (HTTP, R, To_String (URI));
      end Reload_R;

   begin
      accept Start (N : Positive) do
         Client.N := N;
      end Start;

      AWS.Client.Create (HTTP, Server.Status.Local_URL (WS));

      Get_R ("/download_file");

      loop
         Code := Response.Status_Code (R);

         declare
            Message : constant String := Response.Message_Body (R);
         begin
            if Code = Messages.S302 then
               Put_Line
                 ("Client " & Utils.Image (N)
                  & " " & Messages.Status_Code'Image (Code) & Message);
               Get_R (Response.Location (R));

            elsif Fixed.Index (Message, "Download manager") /= 0 then

               if Fixed.Index (Message, "waiting") /= 0 then
                  Some_Waiting := True;
               else
                  Starting := Starting + 1;
               end if;

               --  A download page, we need to reload
               Put_Line
                 ("Client " & Utils.Image (N)
                  & " " & Messages.Status_Code'Image (Code) & Message);
               delay 1.0;
               Reload_R;

            elsif Code = Messages.S200 then
               Downloads := Downloads + 1;
               Put_Line
                 ("Client " & Utils.Image (N)
                  & " " & Messages.Status_Code'Image (Code));
               exit;

            else
               Text_IO.Put_Line
                 ("Error code " & Messages.Status_Code'Image (Code)
                    & Message);
               exit;
            end if;
         end;
      end loop;

      accept Stop (Info : out Download_Info) do
         Info.Size      := Length (Response.Message_Body (R));
         Info.Signature := MD5_Signature (Response.Message_Body (R));
      end Stop;

   exception
      when others =>
         Put_Line ("Client " & Utils.Image (N) & " error!");
   end Client;

   ---------------------
   -- Create_Filename --
   ---------------------

   function Create_Filename return MD5.Message_Digest is
      subtype Buffer is String (1 .. 20);
      Ctx  : MD5.Context;
      File : Stream_IO.File_Type;
      Str  : Buffer;
   begin
      Stream_IO.Create (File, Stream_IO.Out_File, Filename);

      for K in 1 .. 10_000 loop
         for K in Buffer'Range loop
            Str (K) := Random (Rand_Generator);
         end loop;
         MD5.Update (Ctx, Str);
         Stream_IO.Write (File, Translator.To_Stream_Element_Array (Str));
      end loop;

      Stream_IO.Close (File);

      return MD5.Digest (Ctx);
   end Create_Filename;

   -------------------
   -- MD5_Signature --
   -------------------

   function MD5_Signature
     (Content : Unbounded_String) return MD5.Message_Digest
   is
      Chunk_Size  : constant := 4 * 1_024;
      Len         : constant Positive := Length (Content);
      Ctx         : MD5.Context;
      First, Last : Positive;
   begin
      First := 1;
      loop
         Last := Positive'Min (First + Chunk_Size - 1, Len);
         MD5.Update (Ctx, Slice (Content, First, Last));
         exit when Last = Len;
         First := Last + 1;
      end loop;

      return MD5.Digest (Ctx);
   end MD5_Signature;

   --------------
   -- Put_Line --
   --------------

   procedure Put_Line (Str : String) is
   begin
      if Debug then
         Text_IO.Put_Line (Str);
      end if;
   end Put_Line;

   U    : Services.Dispatchers.URI.Handler;
   D    : Services.Dispatchers.Linker.Handler;
   R    : Response.Data;

   Conf : Config.Object := Config.Get_Current;

   Clients : array (1 .. Nb_Client) of Client;

   Results : array (1 .. Nb_Client) of Download_Info;

   Size    : Positive;
   Size_Ok : Boolean := True;
   Sig_Ok  : Boolean := True;

begin
   Reset (Rand_Generator);

   Signature := Create_Filename;

   Config.Set.Server_Host (Conf, "localhost");
   Config.Set.Server_Port (Conf, 0);

   Services.Dispatchers.URI.Register
     (U, "/welcome", CB'Unrestricted_Access);
   Services.Dispatchers.URI.Register
     (U, "/download_file", CB'Unrestricted_Access);

   Text_IO.Put_Line ("Start download server...");

   Services.Download.Start (U, D, 1);

   Text_IO.Put_Line ("Start main server...");

   Server.Start (WS, D, Conf);

   R := AWS.Client.Get (Server.Status.Local_URL (WS) & "/welcome");

   --  Start clients

   Text_IO.Put_Line ("Start clients...");

   for K in Clients'Range loop
      Clients (K).Start (K);
   end loop;

   --  Wait for client to stop

   for K in Clients'Range loop
      Clients (K).Stop (Results (K));
   end loop;

   Text_IO.Put_Line ("Clients stopped...");

   R := AWS.Client.Get (Server.Status.Local_URL (WS) & "/welcome");

   --  Get the real size

   Size := Positive (Utils.File_Size (Filename));

   --  Check the size of each download

   for K in Results'Range loop
      if Results (K).Size /= Size then
         Size_Ok := False;
      end if;
   end loop;

   --  Check the signature

   for K in Results'Range loop
      if Results (K).Signature /= Signature then
         Sig_Ok := False;
      end if;
   end loop;

   if Some_Waiting then
      Text_IO.Put_Line ("OK: some have been waiting");
   else
      Text_IO.Put_Line ("ERROR: nobody in the waiting queue");
   end if;

   Text_IO.Put_Line ("Started   " & Utils.Image (Starting));
   Text_IO.Put_Line ("Donwloads " & Utils.Image (Downloads));

   if Size_Ok then
      Text_IO.Put_Line ("OK: All downloads have the correct size");
   else
      Text_IO.Put_Line ("ERROR: some download are not correct");
   end if;

   if Sig_Ok then
      Text_IO.Put_Line ("OK: All downloads have the correct signature");
   else
      Text_IO.Put_Line ("ERROR: some download have not a correct signature");
   end if;

   Text_IO.Put_Line ("Stop servers...");
   Server.Shutdown (WS);
   Services.Download.Stop;
end DM;
