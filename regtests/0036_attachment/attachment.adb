------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2004-2016, AdaCore                     --
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

with Ada.Streams.Stream_IO;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with AWS.Attachments;
with AWS.Client;
with AWS.Config.Set;
with AWS.MIME;
with AWS.Net.Log;
with AWS.Response;
with AWS.Server.Status;
with AWS.Status;
with AWS.Translator;
with AWS.Utils;

procedure Attachment is

   use Ada;
   use Ada.Streams;
   use Ada.Strings;
   use AWS;

   Att      : Attachments.List;
   Response : AWS.Response.Data;

   WS       : Server.HTTP;
   CNF      : Config.Object;

   procedure Output (Filename, Local_Filename, Content_Type : String);
   --  Output content of filename. Output is base64 encoded if content type is
   --  not textual data.

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
         Text_IO.Put_Line (Translator.To_String (Data (Data'First .. Last)));
         Text_IO.New_Line;
      end if;
   end Dump;

   ------------
   -- Output --
   ------------

   procedure Output (Filename, Local_Filename, Content_Type : String) is
      use Ada.Streams;
      use Ada.Strings.Unbounded;

      L_Filename : Unbounded_String := To_Unbounded_String (Local_Filename);
      C          : constant Natural :=
                     Strings.Unbounded.Index (L_Filename, "-");

      File   : Stream_IO.File_Type;
      Buffer : Stream_Element_Array (1 .. 4_048);
      Last   : Stream_Element_Offset;
   begin
      --  Convert slashes

      Strings.Unbounded.Translate
        (L_Filename, Strings.Maps.To_Mapping ("\", "/"));

      --  Now remove the pid

      Strings.Unbounded.Replace_Slice (L_Filename, 3, C, "XpidX-");

      Text_IO.Put_Line
        ("File : " & To_String (L_Filename)
         & ", " & Filename & ", " & Content_Type);

      Stream_IO.Open (File, Stream_IO.In_File, Local_Filename);
      Stream_IO.Read (File, Buffer, Last);

      if MIME.Is_Text (Content_Type) then
         Text_IO.Put_Line (Translator.To_String (Buffer (1 .. Last)));
      else
         Text_IO.Put_Line (Translator.Base64_Encode (Buffer (1 .. Last)));
      end if;

      Text_IO.New_Line;

      Stream_IO.Close (File);
   end Output;

   -----------
   -- HW_CB --
   -----------

   function HW_CB (Request : AWS.Status.Data) return AWS.Response.Data is
      Att_List : constant AWS.Attachments.List :=
                   AWS.Status.Attachments (Request);
      Atts     : constant Integer := AWS.Attachments.Count (Att_List);
      Att      : AWS.Attachments.Element;
   begin
      for J in 1 .. Atts loop
         Att := AWS.Attachments.Get (Att_List, J);

         Output (AWS.Attachments.Filename (Att),
                 AWS.Attachments.Local_Filename (Att),
                 AWS.Attachments.Content_Type (Att));
      end loop;

      return AWS.Response.Build
        (MIME.Text_Plain, "Got" & Integer'Image (Atts) & " attachments");
   end HW_CB;

begin
   AWS.Attachments.Add
     (Attachments => Att,
      Filename    => "attachment1.txt",
      Content_Id  => "My-Txt-Attachment");

   AWS.Attachments.Add
     (Attachments => Att,
      Filename    => "attachment2.txt",
      Content_Id  => "My-Second-Txt-Attachment");

   AWS.Attachments.Add
     (Attachments => Att,
      Filename    => "aws_logo.png",
      Content_Id  => "My-Png-Attachment");

   Config.Set.Server_Name      (CNF, "Attachment Server");
   Config.Set.Server_Host      (CNF, "localhost");
   Config.Set.Server_Port      (CNF, 0);
   Config.Set.Upload_Directory (CNF, ".");

   Server.Start (WS, HW_CB'Unrestricted_Access, CNF);

   --  AWS.Net.Log.Start (Dump'Unrestricted_Access);

   Response := AWS.Client.Post
     (URL         => AWS.Server.Status.Local_URL (WS) & "/any_URI",
      Data        => "Dummy message",
      Attachments => Att);

   Text_IO.Put_Line ("Response=" & AWS.Response.Message_Body (Response));

   Server.Shutdown (WS);
end Attachment;
