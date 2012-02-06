------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2011-2012, AdaCore                     --
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

with Ada.Calendar;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with GNAT.AWK;
with GNAT.Calendar.Time_IO;

with AWS.Client;
with AWS.Headers.Set;
with AWS.Messages;
with AWS.MIME;
with AWS.Response;
with AWS.Server.Log;
with AWS.Server.Status;
with AWS.Status;
with AWS.Utils;

with Get_Free_Port;

procedure Log_Check is

   use Ada;
   use Ada.Strings.Unbounded;
   use GNAT;
   use AWS;

   WS   : Server.HTTP;
   Port : Natural := 3241;
   MS   : Unbounded_String;

   --------
   -- CB --
   --------

   function CB (Request : Status.Data) return Response.Data is
      URI  : constant String := Status.URI (Request);
      File : constant String := URI (URI'First + 1 .. URI'Last);
   begin
      if File = "file.txt" or else File = "does-not-exist" then
         return Response.File (MIME.Text_Plain, File);
      elsif File = "data" then
         return Response.Build
           (MIME.Text_Plain,
            Message_Body => "not found",
            Status_Code  => Messages.S404);
      else
         return Response.Build
           (MIME.Text_Plain,
            Message_Body => "xx",
            Status_Code  => Messages.S200);
      end if;
   end CB;

   ---------
   -- Get --
   ---------

   procedure Get (Filename : String) is
      H : Client.Header_List;
      R : Response.Data;
   begin
      if Filename = "file.txt" and then MS /= Null_Unbounded_String then
         Headers.Set.Add
           (H,
            Messages.If_Modified_Since_Token,
            To_String (MS));
      end if;

      R := Client.Get
        ("http://" & Server.Status.Host (WS) & ':' & Utils.Image (Port)
           & '/' & Filename,
         Headers => H);
      Text_IO.Put_Line ("R : " & Response.Message_Body (R));
      Text_IO.Put_Line ("S : " & Messages.Image (Response.Status_Code (R)));

      if Filename = "file.txt" and then MS = Null_Unbounded_String then
         MS := To_Unbounded_String
           (Response.Header (R, Messages.Last_Modified_Token));
      end if;
   end Get;

   ----------------
   -- Parse_Logs --
   ----------------

   procedure Parse_Logs is
      Filename : constant := 6;
      Status   : constant := 8;
   begin
      AWK.Add_File ("log_check.log");

      AWK.Open;

      while not AWK.End_Of_Data loop
         AWK.Get_Line;

         if AWK.Field (2) = "Stop" and AWK.Field (3) = "logging." then
            null;
         else
            Text_IO.Put_Line
              (AWK.Field (Filename) & " - " & AWK.Field (Status));
         end if;
      end loop;

      AWK.Close (AWK.Current_Session.all);
   end Parse_Logs;

begin
   Get_Free_Port (Port);

   Server.Start
     (WS, "log_check",
      CB'Unrestricted_Access, Port => Port, Max_Connection => 1);
   Text_IO.Put_Line ("started"); Ada.Text_IO.Flush;

   Server.Log.Start (WS, Filename_Prefix => "log_check");

   Get ("file.txt");
   Get ("file.txt");
   Get ("does-not-exist");

   Get ("data");
   Get ("data2");

   Server.Shutdown (WS);
   Text_IO.Put_Line ("shutdown");

   Parse_Logs;
end Log_Check;
