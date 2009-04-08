------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2008-2009, AdaCore                     --
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

with Ada.Streams;
with Ada.Strings.Fixed;
with Ada.Text_IO;
with Ada.Exceptions;

with AWS.Log;
with AWS.Client;
with AWS.Config.Set;
with AWS.Exceptions;
with AWS.MIME;
with AWS.Messages;
with AWS.Net;
with AWS.Parameters;
with AWS.Response;
with AWS.Server.Status;
with AWS.Status;
with AWS.Translator;
with AWS.Utils;

procedure Uplimit is

   use Ada;
   use Ada.Streams;
   use Ada.Text_IO;
   use AWS;

   function CB (Request : Status.Data) return Response.Data;

   task Server is
      entry Start;
      entry Started (Port : out Positive);
      entry Stopped;
   end Server;

   HTTP : AWS.Server.HTTP;

   procedure Problem
     (E      : Ada.Exceptions.Exception_Occurrence;
      Log    : in out AWS.Log.Object;
      Error  : AWS.Exceptions.Data;
      Answer : in out Response.Data);
   --  Web exception handler

   --------
   -- CB --
   --------

   function CB (Request : Status.Data) return Response.Data is
      URI    : constant String := Status.URI (Request);
      Buffer : Stream_Element_Array (1 .. 10);
      Prev   : Stream_Element_Array (1 .. 10) := (others => 0);
      Last   : Stream_Element_Offset;
      Is_Up  : constant Boolean := Status.Is_Body_Uploaded (Request);
   begin
      if URI = "/" or else URI = "/noup" then
         if URI = "/" then
            AWS.Server.Get_Message_Body;

         elsif not Status.Is_Body_Uploaded (Request)
           and then Status.Binary_Size (Request) = 0
         then
            return Response.Build
                     (MIME.Text_Plain,
                      Integer'Image (Status.Content_Length (Request))
                      & " bytes of message body ignored.",
                      Messages.S413);
         end if;

         loop
            Status.Read_Body (Request, Buffer, Last);
            if Buffer (1) = 0
              or else (Prev (1) /= 0 and then Buffer /= Prev)
            then
               return Response.Build
                        (MIME.Text_Plain, "Data trasfer error", Messages.S500);
            end if;

            exit when Last < Buffer'Last;

            Prev := Buffer;
         end loop;

         return Response.Build
                  (MIME.Text_Plain,
                   URI & " ok server uploaded: " & Boolean'Image (Is_Up)
                   & Stream_Element_Offset'Image (Status.Binary_Size (Request))
                   & Integer'Image (Status.Content_Length (Request))
                   & ' ' & AWS.Translator.To_String (Prev));

      else
         Put_Line ("Unknown URI " & URI);
         return Response.Build
           (MIME.Text_HTML, URI & " not found", Messages.S404);
      end if;
   end CB;

   -------------
   -- Problem --
   -------------

   procedure Problem
     (E      : Ada.Exceptions.Exception_Occurrence;
      Log    : in out AWS.Log.Object;
      Error  : AWS.Exceptions.Data;
      Answer : in out Response.Data) is
   begin
      Put_Line ("!ERROR " & Ada.Exceptions.Exception_Information (E));
      Answer := Response.Build
        (MIME.Text_HTML,
         "Server side exception " & Ada.Exceptions.Exception_Message (E));
   end Problem;

   ------------
   -- Server --
   ------------

   task body Server is
      Web_Config : Config.Object;
   begin
      Config.Set.Server_Port (Web_Config, 0);
      Config.Set.Upload_Directory (Web_Config, "./");
      Config.Set.Upload_Size_Limit (Web_Config, 11_000);

      AWS.Server.Set_Unexpected_Exception_Handler
        (HTTP, Problem'Unrestricted_Access);

      accept Start;

      AWS.Server.Start (HTTP, CB'Unrestricted_Access, Web_Config);

      Put_Line ("Server started");
      New_Line;

      accept Started (Port : out Positive) do
         Port := AWS.Server.Status.Port (HTTP);
      end Started;

      select
         accept Stopped;
      or
         delay 20.0;
         Put_Line ("Too much time to do the job !");
      end select;

      AWS.Server.Shutdown (HTTP);

      Put_Line ("Server stopped");
   exception
      when E : others =>
         Put_Line ("Server Error " & Ada.Exceptions.Exception_Information (E));
   end Server;

   -------------
   -- Request --
   -------------

   Port   : Positive;
   Client : AWS.Client.HTTP_Connection;
   R      : Response.Data;

   procedure Print is
   begin
      Put_Line
        ("=> " & Messages.Status_Line (Response.Status_Code (R))
         & ' ' & Response.Message_Body (R));
   end Print;

   use Ada.Strings.Fixed;

begin
   Put_Line ("Start main, wait for server to start...");

   Server.Start;
   Server.Started (Port);

   AWS.Client.Create (Client, "http://localhost:" & Utils.Image (Port));

   AWS.Client.Post (Client, R, 1000 * "0123456789", URI => "/noup"); Print;
   AWS.Client.Post (Client, R, 1100 * "abcdefghij", URI => "/noup"); Print;
   AWS.Client.Post (Client, R, 1200 * "ABCDEFGHIJ", URI => "/noup"); Print;
   AWS.Client.Post (Client, R, 1000 * "9876543210"); Print;
   AWS.Client.Post (Client, R, 1100 * "qwertyuiop"); Print;
   AWS.Client.Post (Client, R, 1200 * "QWERTYUIOP"); Print;

   Server.Stopped;

exception
   when E : others =>
      Put_Line ("Main Error " & Ada.Exceptions.Exception_Information (E));
end Uplimit;
