------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2000-2001                          --
--                                ACT-Europe                                --
--                                                                          --
--  Authors: Dmitriy Anisimkov - Pascal Obry                                --
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

with AWS.OS_Lib;
with AWS.Config;
with AWS.Messages;
with AWS.MIME;
with AWS.Services.Directory;
with AWS.Server.Push;
with AWS.Parameters;

with GNAT.Calendar.Time_IO;
with Ada.Calendar;
with Ada.Strings.Unbounded;
with Ada.Integer_Text_IO;
with Ada.Exceptions;

package body WS_CB is

   use AWS;
   use Ada.Strings.Unbounded;
   use Ada.Calendar;

   WWW_Root : String renames AWS.Config.WWW_Root (Server.Config (WS));

   type Client_Env is record
      Start   : Time;
      Picture : Unbounded_String;
   end record;

   --  Simple ID generator.

   protected New_Client_ID is
      procedure Get (New_ID : out String);
   private
      ID : Natural := 0;
   end New_Client_ID;

   task Server_Push_Task;
   --  The push data are generated here.

   function Image
     (Time : in Ada.Calendar.Time;
      Env  : in Client_Env)
     return String;

   package Time_Push is new AWS.Server.Push
     (Client_Output_Type => Ada.Calendar.Time,
      Stream_Output_Type => String,
      Client_Environment => Client_Env,
      To_Stream_Output   => Image);

   SP : Time_Push.Object;

   ---------
   -- Get --
   ---------

   function Get (Request : in AWS.Status.Data) return AWS.Response.Data is
      URI      : constant String := AWS.Status.URI (Request);
      Filename : constant String := WWW_Root & URI (2 .. URI'Last);

   begin
      if URI = "/ref" then
         return AWS.Response.Moved
           (Location => "http://localhost:1234/demos/page1.html");

      elsif URI = "/server_push" then

         declare
            use GNAT.Calendar.Time_IO;
            use Ada.Calendar;

            P_List : constant AWS.Parameters.List
              := AWS.Status.Parameters (Request);

            Picture : Unbounded_String
              := To_Unbounded_String (AWS.Parameters.Get_Value (P_List));

            Client_ID : String (1 .. 32);
         begin
            New_Client_ID.Get (Client_ID);

            if Picture = Null_Unbounded_String then
               Picture := To_Unbounded_String ("%D - %T");
            end if;

            Time_Push.Register
              (Server      => SP,
               Client_ID   => Client_ID,
               Socket      => AWS.Status.Socket (Request),
               Environment => (Clock, Picture),
               Kind        => Time_Push.Multipart);

            Time_Push.Send_To
              (SP, Client_ID, Ada.Calendar.Clock, "text/html");
         end;

         return AWS.Response.Socket_Taken;

      elsif OS_Lib.Is_Regular_File (Filename) then
         return AWS.Response.File
           (Content_Type => AWS.MIME.Content_Type (Filename),
            Filename     => Filename);

      elsif OS_Lib.Is_Directory (Filename) then
         return AWS.Response.Build
           (Content_Type => "text/html",
            Message_Body =>
              AWS.Services.Directory.Browse
              (Filename, "aws_directory.thtml", Request));

      else
         return AWS.Response.Acknowledge
           (Messages.S404,
            "<p>Page '" & URI & "' Not found.");
      end if;
   end Get;

   -----------
   -- Image --
   -----------

   function Image
     (Time : in Ada.Calendar.Time;
      Env  : in Client_Env)
     return String
   is
      use GNAT.Calendar.Time_IO;
   begin
      return Image (Time, Picture_String (To_String (Env.Picture)))
        & ASCII.CR & ASCII.LF
        & Duration'Image (Time - Env.Start);
   end Image;

   ----------------------
   -- Server_Push_Task --
   ----------------------

   task body Server_Push_Task is
   begin
      loop
         delay 1.0;
         Time_Push.Send (SP, Ada.Calendar.Clock, "text/plain");
      end loop;
   end Server_Push_Task;

   -------------------
   -- New_Client_ID --
   -------------------

   protected body New_Client_ID is

      procedure Get (New_ID : out String) is
      begin
         ID := ID + 1;
         Ada.Integer_Text_IO.Put (New_ID, ID);
      end Get;

   end New_Client_ID;

   ---------
   -- Put --
   ---------

   function Put (Request : in AWS.Status.Data) return AWS.Response.Data is
   begin
      return AWS.Response.Acknowledge (Status_Code => AWS.Messages.S200);
   end Put;

   -------------
   -- Service --
   -------------

   function Service (Request : in AWS.Status.Data) return AWS.Response.Data is
      use type AWS.Status.Request_Method;
   begin
      if AWS.Status.Method (Request) = AWS.Status.GET
        or else AWS.Status.Method (Request) = AWS.Status.POST
        or else AWS.Status.Method (Request) = AWS.Status.HEAD
      then
         return Get (Request);

      elsif AWS.Status.Method (Request) = AWS.Status.PUT then
         return Put (Request);

      else
         return AWS.Response.Acknowledge (Status_Code => Messages.S405);
      end if;

   exception
      when E : others =>
         return AWS.Response.Build
           (Content_Type => "text/plain",
            Status_Code => AWS.Messages.S500,
            Message_Body => Ada.Exceptions.Exception_Information (E));
   end Service;

   ----------------------
   -- Stop_Push_Server --
   ----------------------

   procedure Stop_Push_Server is
   begin
      abort Server_Push_Task;
   end Stop_Push_Server;

end WS_CB;
