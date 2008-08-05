------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2005-2008, AdaCore                     --
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
with Ada.Exceptions;
with Ada.Streams.Stream_IO;

with GNAT.MD5;

with AWS.Client;
with AWS.Digest;
with AWS.Server;
with AWS.Status;
with AWS.MIME;
with AWS.Response;
with AWS.Messages;
with AWS.Utils;

with Get_Free_Port;

procedure Partial is

   use Ada;
   use Ada.Streams;
   use Ada.Text_IO;
   use AWS;
   use GNAT;

   Filename : constant String := "partial.adb";

   function CB (Request : in Status.Data) return Response.Data;

   function File_MD5 (Filename : in String) return MD5.Message_Digest;
   --  Compute the MD5 signature for Filename

   procedure Dump_Response
     (Id          : in String;
      R           : in Response.Data;
      Status_Only : in Boolean := False);
   --  Dump information about the response

   task Server is
      entry Wait_Start;
      entry Stop;
   end Server;

   HTTP : AWS.Server.HTTP;

   Connect : Client.HTTP_Connection;

   R     : Response.Data;
   Port  : Natural := 1286;
   P_MD5 : MD5.Message_Digest;
   D_MD5 : MD5.Message_Digest;

   --------
   -- CB --
   --------

   function CB (Request : in Status.Data) return Response.Data is
      URI      : constant String := Status.URI (Request);
      Filename : constant String := URI (URI'First + 1 .. URI'Last);
   begin
      return Response.File (MIME.Content_Type (Filename), Filename);
   end CB;

   -------------------
   -- Dump_Response --
   -------------------

   procedure Dump_Response
     (Id          : in String;
      R           : in Response.Data;
      Status_Only : in Boolean := False) is
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

   function File_MD5 (Filename : in String) return MD5.Message_Digest is
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

   ------------
   -- Server --
   ------------

   task body Server is
   begin
      Get_Free_Port (Port);

      AWS.Server.Start
        (HTTP, "Test Partial Download.",
         CB'Unrestricted_Access, Port => Port, Max_Connection => 2);

      accept Wait_Start;
      accept Stop;

   exception
      when E : others =>
         Put_Line ("Server Error " & Exceptions.Exception_Information (E));
   end Server;

begin
   Server.Wait_Start;

   --  Compute MD5

   P_MD5 := File_MD5 (Filename);

   Client.Create
     (Connection => Connect,
      Host       => "http://localhost:" & Utils.Image (Port));

   --  Get file by chunck

   declare
      use type Client.Content_Bound;
      URI        : constant String := "/" & Filename;
      Size       : constant Stream_Element_Offset :=
                     Utils.File_Size (Filename);
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
   Server.Stop;

   --  Compute MD5 of downloaded file, and check with original file

   D_MD5 := File_MD5 (Filename & ".downloaded");

   if P_MD5 = D_MD5 then
      Text_IO.Put_Line ("OK partial download");
   else
      Text_IO.Put_Line ("NOK partial download");
   end if;

exception
   when E : others =>
      Server.Stop;
      Put_Line ("Main Error " & Exceptions.Exception_Information (E));
end Partial;
