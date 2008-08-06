------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2006-2008, AdaCore                     --
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
with AWS.Status;
with AWS.Server.Log;
with AWS.Translator;
with AWS.Utils;

with Get_Free_Port;

procedure TLog_Proc (Extended_Fields : in String) is

   use Ada;
   use GNAT;
   use AWS;

   Filename : constant String := "file.tmp";
   Today    : constant String
     := GNAT.Calendar.Time_IO.Image (Ada.Calendar.Clock, "%Y-%m-%d");
   Success  : Boolean;

   WS   : Server.HTTP;
   Port : Natural := 8251;

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
     (E      : in     Ada.Exceptions.Exception_Occurrence;
      Log    : in out AWS.Log.Object;
      Error  : in     AWS.Exceptions.Data;
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
     (Name  : in     String;
      Index : in     Positive;
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

   function Hide_Date (Filename : in String) return String is
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
      I    : AWK.Count;
      Ext_Log : Boolean;
   begin
      AWK.Add_File ("tlog-" & Today & ".log");
      AWK.Add_File ("tlog_error-" & Today & ".log");

      AWK.Open;

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

            for K in I + 1 .. AWK.Count'Min (10 + I, AWK.NF) loop
               Text_IO.Put (AWK.Field (K));
               Text_IO.Put (" | ");
            end loop;

            Text_IO.New_Line;
         end if;
      end loop;

      AWK.Close (AWK.Current_Session);
   end Parse_Logs;

   --------
   -- CB --
   --------

   function CB (Request : in Status.Data) return Response.Data is
      URI : constant String := Status.URI (Request);
   begin
      X_Counter := X_Counter + 1;

      AWS.Server.Set_Field ("x-counter", AWS.Utils.Image (X_Counter));

      if URI = "/one" then
         return Response.Build (MIME.Text_HTML, "one");

      elsif URI = "/file" then
         return Response.File (MIME.Text_Plain, Filename);

      elsif URI = "/error" then
         raise Constraint_Error;
         return Response.File (MIME.Text_Plain, "tlog.adb");

      else
         return Response.Build
           (MIME.Text_HTML, "This is an error", Messages.S404);
      end if;
   end CB;

   R : Response.Data;

   Connect : Client.HTTP_Connection;

begin
   if Extended_Fields = "" then
      Delete_Logs;
      Create_File;
   end if;

   AWS.Server.Set_Unexpected_Exception_Handler (WS, UEH'Unrestricted_Access);

   Get_Free_Port (Port);

   AWS.Config.Set.Server_Port (Config, Port);
   AWS.Config.Set.Server_Name (Config, "tlog");
   AWS.Config.Set.Max_Connection (Config, 1);
   AWS.Config.Set.Log_Extended_Fields (Config, Extended_Fields);

   Server.Start (WS, CB'Unrestricted_Access, Config => Config);

   Server.Log.Start (WS);
   Server.Log.Start_Error (WS);

   Ada.Text_IO.Put_Line ("started");

   R := Client.Get ("http://localhost:" & Utils.Image (Port) & "/one");
   R := Client.Get ("http://localhost:" & Utils.Image (Port) & "/azerty");
   R := Client.Get ("http://localhost:" & Utils.Image (Port) & "/file");
   R := Client.Get ("http://localhost:" & Utils.Image (Port) & "/one");
   R := Client.Get ("http://localhost:" & Utils.Image (Port) & "/file");
   R := Client.Get ("http://localhost:" & Utils.Image (Port) & "/error");
   R := Client.Get ("http://localhost:" & Utils.Image (Port) & "/azerty");
   R := Client.Get ("http://localhost:" & Utils.Image (Port) & "/error");
   R := Client.Get ("http://localhost:" & Utils.Image (Port) & "/one");
   R := Client.Get ("http://localhost:" & Utils.Image (Port) & "/azerty");
   R := Client.Get ("http://localhost:" & Utils.Image (Port) & "/file");

   Client.Create
     (Connection => Connect,
      Host       => "http://localhost:" & Utils.Image (Port),
      Retry      => 0);

   --  Test for basic authentication.

   Client.Set_WWW_Authentication (Connect, "login", "pwd", Client.Basic);

   Client.Get (Connect, R, "/azerty");
   Client.Get (Connect, R, "/one");
   Client.Get (Connect, R, "/file");
   Client.Get (Connect, R, "/error");
   Client.Get (Connect, R, "/one");
   Client.Get (Connect, R, "/file");

   Client.Close (Connect);

   --  test for invalid header line logging.

   declare
      use AWS.Net;
      Sock : Socket_Type'Class := Socket (False);
   begin
      Net.Connect (Sock, "localhost", Port);
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
