------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2005-2012, AdaCore                     --
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
with Ada.Exceptions;
with Ada.Streams.Stream_IO;

with GNAT.MD5;

with AWS.Client;
with AWS.Digest;
with AWS.Server.Status;
with AWS.Status;
with AWS.MIME;
with AWS.Response;
with AWS.Messages;
with AWS.Utils;

procedure Partial is

   use Ada;
   use Ada.Streams;
   use Ada.Text_IO;
   use AWS;
   use GNAT;

   Filename : constant String := "partial.txt";

   function CB (Request : Status.Data) return Response.Data;

   function File_MD5 (Filename : String) return MD5.Message_Digest;
   --  Compute the MD5 signature for Filename

   procedure Dump_Response
     (Id          : String;
      R           : Response.Data;
      Status_Only : Boolean := False);
   --  Dump information about the response

   HTTP : Server.HTTP;

   Connect : Client.HTTP_Connection;

   R     : Response.Data;
   P_MD5 : MD5.Message_Digest;
   D_MD5 : MD5.Message_Digest;

   --------
   -- CB --
   --------

   function CB (Request : Status.Data) return Response.Data is
      URI      : constant String := Status.URI (Request);
      Filename : constant String := URI (URI'First + 1 .. URI'Last);
   begin
      return Response.File (MIME.Content_Type (Filename), Filename);
   end CB;

   -------------------
   -- Dump_Response --
   -------------------

   procedure Dump_Response
     (Id          : String;
      R           : Response.Data;
      Status_Only : Boolean := False) is
   begin
      Text_IO.Put
        (Id & " code="
         & Messages.Status_Code'Image (Response.Status_Code (R)));

      if Status_Only then
         Text_IO.New_Line;
      else
         Text_IO.Put_Line
           (" length="
            & Response.Content_Length_Type'Image
              (Response.Content_Length (R)));
      end if;
   end Dump_Response;

   --------------
   -- File_MD5 --
   --------------

   function File_MD5 (Filename : String) return MD5.Message_Digest is
      File   : Stream_IO.File_Type;
      Buffer : Stream_Element_Array (1 .. 4 * 1_024);
      Last   : Stream_Element_Offset;
      Ctx    : MD5.Context;
   begin
      Stream_IO.Open (File, Stream_IO.In_File, Filename);

      loop
         Stream_IO.Read (File, Buffer, Last);
         MD5.Update (Ctx, Buffer (1 .. Last));
         exit when Last < Buffer'Last;
      end loop;

      Stream_IO.Close (File);

      return MD5.Digest (Ctx);
   end File_MD5;

begin
   Server.Start
     (HTTP, "Test Partial Download.", CB'Unrestricted_Access, Port => 0,
      Max_Connection => 2);

   --  Compute MD5

   P_MD5 := File_MD5 (Filename);

   Client.Create
     (Connection => Connect, Host => AWS.Server.Status.Local_URL (HTTP));

   --  Get file by chunck

   declare
      use type Client.Content_Bound;
      URI        : constant String := "/" & Filename;
      Size       : constant Utils.File_Size_Type := Utils.File_Size (Filename);
      R1, R2, R3 : Response.Data;
      File       : Stream_IO.File_Type;
   begin
      Client.Get (Connect, R1, URI, (0, 1_023));
      Dump_Response ("R1", R1);

      Client.Get (Connect, R3, URI, (Client.Undefined, 1_024));
      Dump_Response ("R3", R3);

      Client.Get (Connect, R2, URI,
                  (1_024, Client.Content_Bound (Size) - 1_024 - 1));
      Dump_Response ("R2", R2, Status_Only => True);

      Stream_IO.Create (File, Stream_IO.Out_File, Filename & ".downloaded");
      Stream_IO.Write (File, Response.Message_Body (R1));
      Stream_IO.Write (File, Response.Message_Body (R2));
      Stream_IO.Write (File, Response.Message_Body (R3));
      Stream_IO.Close (File);
   end;

   Client.Close (Connect);
   Server.Shutdown (HTTP);

   --  Compute MD5 of downloaded file, and check with original file

   D_MD5 := File_MD5 (Filename & ".downloaded");

   if P_MD5 = D_MD5 then
      Text_IO.Put_Line ("OK partial download");
   else
      Text_IO.Put_Line ("NOK partial download");
   end if;

exception
   when E : others =>
      Put_Line ("Main Error " & Exceptions.Exception_Information (E));
      Server.Shutdown (HTTP);
end Partial;
