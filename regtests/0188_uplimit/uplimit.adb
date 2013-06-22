------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2008-2013, AdaCore                     --
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

with Ada.Exceptions;
with Ada.Streams;
with Ada.Strings.Fixed;
with Ada.Text_IO;

with AWS.Client;
with AWS.Config.Set;
with AWS.Exceptions;
with AWS.Log;
with AWS.Messages;
with AWS.MIME;
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

   HTTP : Server.HTTP;

   procedure Start_Server;

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

   procedure Start_Server is
      Web_Config : Config.Object;
   begin
      Config.Set.Server_Port (Web_Config, 0);
      Config.Set.Upload_Directory (Web_Config, "./");
      Config.Set.Upload_Size_Limit (Web_Config, 11_000);
      Config.Set.Max_Connection (Web_Config, 10);

      Server.Set_Unexpected_Exception_Handler
        (HTTP, Problem'Unrestricted_Access);

      Server.Start (HTTP, CB'Unrestricted_Access, Web_Config);

      Put_Line ("Server started");
      New_Line;
   end Start_Server;

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

   Start_Server;

   AWS.Client.Create (Client, Server.Status.Local_URL (HTTP));

   AWS.Client.Post (Client, R, 1000 * "0123456789", URI => "/noup"); Print;
   AWS.Client.Post (Client, R, 1100 * "abcdefghij", URI => "/noup"); Print;
   AWS.Client.Post (Client, R, 1200 * "ABCDEFGHIJ", URI => "/noup"); Print;
   AWS.Client.Post (Client, R, 1000 * "9876543210"); Print;
   AWS.Client.Post (Client, R, 1100 * "qwertyuiop"); Print;
   AWS.Client.Post (Client, R, 1200 * "QWERTYUIOP"); Print;

   Server.Shutdown (HTTP);
   Put_Line ("Server stopped");

exception
   when E : others =>
      Server.Shutdown (HTTP);
      Put_Line ("Main Error " & Ada.Exceptions.Exception_Information (E));
end Uplimit;
