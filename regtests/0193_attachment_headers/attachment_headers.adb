------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2008-2015, AdaCore                     --
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
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with AWS.Attachments;
with AWS.Client;
with AWS.Containers.Tables;
with AWS.Headers;
with AWS.MIME;
with AWS.Net.Log;
with AWS.Response;
with AWS.Server.Status;
with AWS.Status;
with AWS.Translator;
with AWS.Utils;

procedure Attachment_Headers is

   use Ada;
   use Ada.Streams;
   use Ada.Strings.Unbounded;
   use AWS;

   function "-" (S : Unbounded_String) return String renames To_String;

   ----------
   -- Dump --
   ----------

   procedure Dump
     (Direction : Net.Log.Data_Direction;
      Socket    : Net.Socket_Type'Class;
      Data      : Stream_Element_Array;
      Last      : Stream_Element_Offset)
   is
      use type Net.Log.Data_Direction;
   begin
      if Direction = Net.Log.Sent then
         Text_IO.Put_Line
           ("********** " & Net.Log.Data_Direction'Image (Direction));
         Text_IO.Put_Line
           (Translator.To_String (Data (Data'First .. Last)));
         Text_IO.New_Line;
      end if;
   end Dump;

   ------------
   -- Upload --
   ------------

   function Upload (Request : Status.Data) return Response.Data is

      procedure Process_Attachment
        (Attachment : AWS.Attachments.Element;
         Index      : Positive;
         Quit       : in out Boolean);
      --  Process each attachment

      ------------------------
      -- Process_Attachment --
      ------------------------

      procedure Process_Attachment
        (Attachment : AWS.Attachments.Element;
         Index      : Positive;
         Quit       : in out Boolean)
      is
         Filename     : constant String :=
                          AWS.Attachments.Local_Filename (Attachment);
         Headers      : constant AWS.Headers.List :=
                          AWS.Attachments.Headers (Attachment);
         Header_Names : constant Containers.Tables.VString_Array :=
                          Headers.Get_Names;
      begin
         Text_IO.Put_Line ("Attachment headers:");
         for Index in Header_Names'Range loop
            Text_IO.Put_Line (-Header_Names (Index));
         end loop;
      end Process_Attachment;

      procedure Process_Attachments is
         new AWS.Attachments.For_Every_Attachment (Process_Attachment);

      Attachments  : constant AWS.Attachments.List :=
                       Status.Attachments (Request);
      Headers      : constant AWS.Headers.List := Status.Header (Request);
      Header_Names : constant Containers.Tables.VString_Array :=
                       Headers.Get_Names;

   begin
      Text_IO.Put_Line ("Post headers:");

      for Index in Header_Names'Range loop
         Text_IO.Put_Line (-Header_Names (Index));
      end loop;

      Process_Attachments (Attachments);

      return Response.Build (MIME.Text_HTML, "OK");
   end Upload;

   Attachments : AWS.Attachments.List;
   Headers     : AWS.Headers.List;
   Result      : AWS.Response.Data;
   Srv         : Server.HTTP;

begin
   Text_IO.Put_Line ("Start...");

   Server.Start
     (Srv, "attachment_headers",
      Upload'Unrestricted_Access,
      Upload_Directory => ".",
      Port             => 0);

   Headers.Add (Name => "X-Message-Seconds", Value => "64");
   Headers.Add (Name => "Custom-Header", Value => "A Value");
   Headers.Add (Name => "Content-Custom-Header", Value => "Something else");

   Text_IO.Put_Line ("Insert attachment...");

   AWS.Attachments.Add
     (Attachments => Attachments,
      Filename => "test.txt", Headers => Headers);

   Text_IO.Put_Line ("Call Post...");

   --  AWS.Net.Log.Start (Dump'Unrestricted_Access);

   Result := Client.Post
     (URL          => Server.Status.Local_URL (Srv) & "/upload",
      Data         => "ID=1",
      Content_Type => MIME.Application_Form_Data,
      Attachments  => Attachments,
      Headers      => Headers);

   Server.Shutdown (Srv);
end Attachment_Headers;
