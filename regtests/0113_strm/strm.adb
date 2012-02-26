------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2000-2012, AdaCore                     --
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

--  Test for user defined stream

with Ada.Text_IO;
with Ada.Exceptions;
with Ada.Streams;

with AWS.Client;
with AWS.Exceptions;
with AWS.Log;
with AWS.Messages;
with AWS.Resources.Streams;
with AWS.Response;
with AWS.Server.Log;
with AWS.Server.Status;
with AWS.Status;
with AWS.Utils;

with User_Strm;

procedure Strm is

   use Ada;
   use Ada.Text_IO;
   use AWS;

   function CB (Request : Status.Data) return Response.Data;

   procedure UEH
     (E      : Ada.Exceptions.Exception_Occurrence;
      Log    : in out AWS.Log.Object;
      Error  : AWS.Exceptions.Data;
      Answer : in out Response.Data);

   procedure Compare_Message;

   HTTP    : AWS.Server.HTTP;
   Connect : Client.HTTP_Connection;
   R       : Response.Data;

   File_Size : constant := 98_100;
   --  !!! Do not change the file size.
   --  It is just for control error when only CRC in the last chunk
   --  of the deflate compressed data.

   Base_URL : access String;

   Length_Defined_URI   : constant String := "/length_defined";
   Length_Undefined_URI : constant String := "/length_undefined";
   Deflated_URI         : constant String := "/deflate-it";
   GZip_URI             : constant String := "/gzip-it";

   --------
   -- CB --
   --------

   function CB (Request : Status.Data) return Response.Data is
      File : AWS.Resources.Streams.Stream_Access :=
               new User_Strm.File_Tagged;
   begin
      User_Strm.Create
        (Resource       => File.all,
         Undefined_Size => Status.URI (Request) /= Length_Defined_URI,
         Size           => File_Size);

      if Status.URI (Request) = Deflated_URI then
         return AWS.Response.Stream
           ("text/plain",
            File,
            Encoding => AWS.Messages.Deflate);

      elsif Status.URI (Request) = GZip_URI then
         return AWS.Response.Stream
           ("text/plain",
            File,
            Encoding => AWS.Messages.GZip);

      else
         return AWS.Response.Stream
           ("text/plain",
            File);
      end if;
   end CB;

   ---------------------
   -- Compare_Message --
   ---------------------

   procedure Compare_Message is
      use Ada.Streams;

      Message      : constant Stream_Element_Array :=
                       Response.Message_Body (R);
      Same_Message : Stream_Element_Array (Message'Range);

      File : User_Strm.File_Tagged;
      Last : Stream_Element_Offset;
   begin
      User_Strm.Create (File, File_Size, False);
      User_Strm.Read (File, Same_Message, Last);

      if Message = Same_Message
        and then Last = Message'Last
        and then User_Strm.End_Of_File (File)
      then
         Put_Line ("Ok.");
      else
         Put_Line
           (Boolean'Image (Message = Same_Message)
            & ' ' & Boolean'Image (Last = Message'Last)
            & ' ' & Boolean'Image (User_Strm.End_Of_File (File))
            & Integer'Image (Message'Length));

         Put_Line ("Error.");
      end if;
   end Compare_Message;

   procedure UEH
     (E      : Ada.Exceptions.Exception_Occurrence;
      Log    : in out AWS.Log.Object;
      Error  : AWS.Exceptions.Data;
      Answer : in out Response.Data)
   is
      pragma Unreferenced (Log, Error, Answer);
   begin
      Put_Line (Ada.Exceptions.Exception_Information (E));
   end UEH;

begin
   Server.Set_Unexpected_Exception_Handler (HTTP, UEH'Unrestricted_Access);

   Server.Start
     (HTTP, "Testing user defined stream.", CB'Unrestricted_Access, Port => 0,
      Max_Connection => 3);

   Server.Log.Start (HTTP);

   Base_URL := new String'(AWS.Server.Status.Local_URL (HTTP));

   --  Keep-alive test

   Client.Create
     (Connection => Connect,
      Host       => Base_URL.all,
      Timeouts   => Client.Timeouts
        (Connect => 1.0, Send => 5.0, Receive => 5.0, Response => 5.0));

   Client.Get (Connect, R, Length_Defined_URI);
   Compare_Message;

   Client.Get (Connect, R, Length_Undefined_URI);
   Compare_Message;

   Client.Get (Connect, R, Deflated_URI);
   Compare_Message;

   Client.Get (Connect, R, GZip_URI);
   Compare_Message;

   --  Test for header answer with undefined responce length

   Client.Head (Connect, R, Length_Undefined_URI);
   Put_Line
     (Messages.Status_Code'Image (Response.Status_Code (R))
      & ' ' & Response.Content_Type (R)
      & ' ' & Utils.Image (Response.Content_Length (R)));

   Client.Close (Connect);

   --  Non keep-alive test

   R := Client.Get (Base_URL.all & Length_Defined_URI);
   Compare_Message;

   R := Client.Get (Base_URL.all & Length_Undefined_URI);
   Compare_Message;

   R := Client.Get (Base_URL.all & Deflated_URI);
   Compare_Message;

   R := Client.Get (Base_URL.all & GZip_URI);
   Compare_Message;

   Server.Shutdown (HTTP);

exception
   when E : others =>
      Put_Line ("Main Error " & Ada.Exceptions.Exception_Information (E));
      Server.Shutdown (HTTP);
end Strm;
