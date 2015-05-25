------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2006-2015, AdaCore                     --
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
with Ada.Characters.Handling;
with Ada.Exceptions;
with Ada.Streams.Stream_IO;
with Ada.Text_IO;

with GNAT.AWK;
with GNAT.Calendar.Time_IO;
with GNAT.Directory_Operations.Iteration;
with GNAT.OS_Lib;

with AWS.Client;
with AWS.Config.Set;
with AWS.Exceptions;
with AWS.Log;
with AWS.Messages;
with AWS.MIME;
with AWS.Net.Buffered;
with AWS.Response;
with AWS.Server.Log;
with AWS.Server.Status;
with AWS.Status;
with AWS.Translator;
with AWS.Utils;

procedure TLog_Proc (Extended_Fields : String) is

   use Ada;
   use GNAT;
   use AWS;

   Filename : constant String := "file.tmp";
   Skip_Log : constant String := "/skip-log-record";
   Today    : constant String :=
                GNAT.Calendar.Time_IO.Image (Ada.Calendar.Clock, "%Y-%m-%d");
   Success  : Boolean;

   WS : Server.HTTP;

   X_Counter : Natural := 0;

   Config : AWS.Config.Object;

   -----------------
   -- Create_File --
   -----------------

   procedure Create_File is
      File : Streams.Stream_IO.File_Type;
      L1   : constant Streams.Stream_Element_Array :=
               Translator.To_Stream_Element_Array ("9");
   begin
      Streams.Stream_IO.Create (File, Streams.Stream_IO.Out_File, Filename);

      for K in 1 .. 2_897 loop
         Streams.Stream_IO.Write (File, L1);
      end loop;

      Streams.Stream_IO.Close (File);
   end Create_File;

   ---------
   -- UEH --
   ---------

   procedure UEH
     (E      : Ada.Exceptions.Exception_Occurrence;
      Log    : in out AWS.Log.Object;
      Error  : AWS.Exceptions.Data;
      Answer : in out Response.Data) is
   begin
      Answer := Response.Build
        (MIME.Text_HTML,
         "012345678901234567890123456789012345678901234567890123456789"
           & "0123456",
        Messages.S500);
   end UEH;

   ---------
   -- Del --
   ---------

   procedure Del
     (Name  : String;
      Index : Positive;
      Quit  : in out Boolean)
   is
      Success : Boolean;
   begin
      OS_Lib.Delete_File (Name, Success);
   end Del;

   -----------------
   -- Delete_Logs --
   -----------------

   procedure Delete_Logs is
      procedure Del_Logs is new Directory_Operations.Iteration.Find (Del);
   begin
      Del_Logs (".", ".*\.log");
   end Delete_Logs;

   ---------------
   -- Hide_Date --
   ---------------

   function Hide_Date (Filename : String) return String is
      R : String := Filename;
   begin
      for K in R'Range loop
         if Characters.Handling.Is_Digit (R (K)) then
            R (K) := 'x';
         end if;
      end loop;
      return R;
   end Hide_Date;

   ----------------
   -- Parse_Logs --
   ----------------

   procedure Parse_Logs is
      use type AWK.Count;
      I       : AWK.Count;
      Ext_Log : Boolean;
      Parser  : AWK.Session_Type;
   begin
      AWK.Add_File ("tlog-" & Today & ".log", Parser);
      AWK.Add_File ("tlog_error-" & Today & ".log", Parser);

      AWK.Open (Session => Parser);

      AWK.Set_Current (Parser);

      Text_IO.Put_Line ("> File : " & Hide_Date (AWK.File));

      while not AWK.End_Of_Data loop

         if AWK.End_Of_File then
            AWK.Get_Line;
            Text_IO.Put_Line ("> File : " & Hide_Date (AWK.File));
         else
            AWK.Get_Line;
         end if;

         Ext_Log := Integer (AWK.NF)
                    = AWS.Config.Log_Extended_Fields_Length (Config);

         if AWK.Field (1) (1) = '#' then
            I := 0;
         elsif AWK.Field (2) = "Stop" and AWK.Field (3) = "logging." then
            I := 0;
         elsif Ext_Log then
            I := 5;
         else
            I := 4;
         end if;

         if I > 0 then
            Text_IO.Put (AWK.Field (1));
            Text_IO.Put (" | ");

            Text_IO.Put (AWK.Field (2));
            Text_IO.Put (" | ");

            Text_IO.Put (AWK.Field (3));
            Text_IO.Put (" | ");

            if not Ext_Log then
               declare
                  V : constant String := AWK.Field (I);
               begin
                  Text_IO.Put (V (V'First));
                  Text_IO.Put ("<date>");
                  Text_IO.Put (V (V'Last));
                  Text_IO.Put (" | ");
               end;
            end if;

            for K in I + 1 .. AWK.Count'Min (9 + I, AWK.NF) loop
               Text_IO.Put (AWK.Field (K));
               Text_IO.Put (" | ");
            end loop;

            Text_IO.New_Line;
         end if;
      end loop;

      AWK.Close (Parser);
   end Parse_Logs;

   --------
   -- CB --
   --------

   function CB (Request : Status.Data) return Response.Data is
      URI : constant String := Status.URI (Request);
   begin
      if URI = Skip_Log then
         Server.Skip_Log_Record;

         return Response.Build (MIME.Text_Plain, "not-logging");
      end if;

      X_Counter := X_Counter + 1;

      Server.Set_Field ("x-counter", Utils.Image (X_Counter));

      if URI = "/one" then
         return Response.Build (MIME.Text_HTML, "one");

      elsif URI = "/file" then
         return Response.File (MIME.Text_Plain, Filename);

      elsif URI = "/error" then
         raise Constraint_Error with "CB at " & Utils.Image (X_Counter);

      else
         return Response.Build
           (MIME.Text_HTML, "This is an error", Messages.S404);
      end if;
   end CB;

   Connect : Client.HTTP_Connection;
   R       : Response.Data;

begin
   if Extended_Fields = "" then
      Delete_Logs;
      Create_File;
   end if;

   AWS.Server.Set_Unexpected_Exception_Handler (WS, UEH'Unrestricted_Access);

   AWS.Config.Set.Server_Port (Config, 0);
   AWS.Config.Set.Server_Name (Config, "tlog");
   AWS.Config.Set.Protocol_Family (Config, "Family_Inet");
   AWS.Config.Set.Max_Connection (Config, 1);
   AWS.Config.Set.Log_Extended_Fields (Config, Extended_Fields);

   Server.Start (WS, CB'Unrestricted_Access, Config => Config);

   Server.Log.Start (WS, Log.Daily, Filename_Prefix => "tlog");
   Server.Log.Start_Error (WS, Log.Daily, Filename_Prefix => "tlog_error");

   Ada.Text_IO.Put_Line ("started");

   declare
      URL : constant String := AWS.Server.Status.Local_URL (WS);
   begin
      R := Client.Get (URL & "/one");
      R := Client.Get (URL & "/azerty");
      R := Client.Get (URL & "/file");
      R := Client.Get (URL & "/one");
      R := Client.Get (URL & "/file");
      R := Client.Get (URL & Skip_Log);
      R := Client.Get (URL & "/error");
      R := Client.Get (URL & "/azerty");
      R := Client.Get (URL & "/error");
      R := Client.Get (URL & "/one");
      R := Client.Get (URL & "/azerty");
      R := Client.Get (URL & "/file");

      Client.Create (Connection => Connect, Host => URL, Retry => 0);
   end;

   --  Test for basic authentication

   Client.Set_WWW_Authentication (Connect, "login", "pwd", Client.Basic);

   Client.Get (Connect, R, "/azerty");
   Client.Get (Connect, R, "/one");
   Client.Get (Connect, R, Skip_Log);
   Client.Get (Connect, R, "/file");
   Client.Get (Connect, R, "/error");
   Client.Get (Connect, R, "/one");
   Client.Get (Connect, R, "/file");

   Client.Close (Connect);

   --  test for invalid header line logging

   declare
      use AWS.Net;
      Sock : Socket_Type'Class := Socket (False);
   begin
      Net.Connect (Sock, "127.0.0.1", Server.Status.Port (WS));
      Buffered.Put_Line (Sock, "GET /header/line/error HTTP/1.1");
      Buffered.Put_Line (Sock, "header line error");
      Buffered.New_Line (Sock);

      loop
         declare
            Data : constant String := Buffered.Get_Line (Sock);
         begin
            null;
         end;
      end loop;
   exception
      when Socket_Error => Net.Shutdown (Sock);
   end;

   Server.Shutdown (WS);

   Ada.Text_IO.Put_Line ("shutdown");

   if Extended_Fields /= "" then
      Parse_Logs;
      OS_Lib.Delete_File (Filename, Success);
   end if;

exception
   when E : others =>
      Text_IO.Put_Line
        ("Main error: " & Ada.Exceptions.Exception_Information (E));
end TLog_Proc;
