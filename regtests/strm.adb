------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2000-2002                          --
--                               ACT-Europe                                 --
--                                                                          --
--  Authors: Dmitriy Anisimokv - Pascal Obry                                --
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

--  $Id$

--  Test for user defined stream.

with Ada.Text_IO;
with Ada.Exceptions;
with Ada.Streams;

with AWS.Server;
with AWS.Client;
with AWS.Status;
with AWS.MIME;
with AWS.Response;
with AWS.Messages;

with AWS.Resources.Streams;

with User_Strm;

procedure Strm is

   use Ada;
   use Ada.Text_IO;
   use AWS;

   function CB (Request : in Status.Data) return Response.Data;

   task Server is
      entry Wait_Start;
      entry Stop;
   end Server;

   HTTP    : AWS.Server.HTTP;
   Connect : Client.HTTP_Connection;
   R       : Response.Data;

   File_Size : constant := 100_000;

   Length_Defined_URI : constant String := "/length_defined";

   --------
   -- CB --
   --------

   function CB (Request : in Status.Data) return Response.Data is
      File : AWS.Resources.Streams.Stream_Access
        := new User_Strm.File_Tagged;
   begin
      User_Strm.Create
        (Resource => File.all,
         Size     => File_Size);

      if Status.URI (Request) = Length_Defined_URI then
         return AWS.Response.Stream
           ("text/plain",
            File,
            File_Size);
      else
         return AWS.Response.Stream
           ("text/plain",
            File,
            AWS.Response.Undefined_Length);
      end if;
   end CB;

   ------------
   -- Server --
   ------------

   task body Server is
   begin
      AWS.Server.Start
        (HTTP, "Testing user defined stream.",
         CB'Unrestricted_Access, Port => 7645, Max_Connection => 3);

      AWS.Server.Start_Log (HTTP);

      accept Wait_Start;
      accept Stop;

   exception
      when E : others =>
         Put_Line ("Server Error " & Exceptions.Exception_Information (E));
   end Server;

   ---------------------
   -- Compare_Message --
   ---------------------

   procedure Compare_Message is
      use Ada.Streams;

      Message      : constant Stream_Element_Array
        := Response.Message_Body (R);

      Same_Message : Stream_Element_Array (Message'Range);

      File : User_Strm.File_Tagged;
      Last : Stream_Element_Offset;
   begin
      User_Strm.Create (File, File_Size);
      User_Strm.Read (File, Same_Message, Last);

      if Message = Same_Message
        and then Last = Message'Last
        and then User_Strm.End_Of_File (File)
      then
         Put_Line ("Ok.");
      else
         Put_Line ("Error.");
      end if;
   end Compare_Message;

begin
   Server.Wait_Start;

   Client.Create
     (Connection => Connect,
      Host       => "http://localhost:7645",
      Timeouts   => (5, 5));

   Client.Get (Connect, R, Length_Defined_URI);
   Compare_Message;

   Client.Get (Connect, R, "/length_undefined");
   Compare_Message;

   Client.Close (Connect);

   Server.Stop;

exception
   when E : others =>
      Server.Stop;
      Put_Line ("Main Error " & Exceptions.Exception_Information (E));
end Strm;
