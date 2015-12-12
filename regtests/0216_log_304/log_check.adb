------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2011-2015, AdaCore                     --
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
with AWS.Config.Set;
with AWS.Headers;
with AWS.Messages;
with AWS.MIME;
with AWS.Response;
with AWS.Server.Log;
with AWS.Server.Status;
with AWS.Status;
with AWS.Utils;

procedure Log_Check is

   use Ada;
   use Ada.Strings.Unbounded;
   use GNAT;
   use AWS;

   WS  : Server.HTTP;
   CNF : Config.Object;
   MS  : Unbounded_String;

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
         H.Add (Messages.If_Modified_Since_Token, To_String (MS));
      end if;

      R := Client.Get (Server.Status.Local_URL (WS) & '/' & Filename,
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
      Parser   : AWK.Session_Type;
   begin
      AWK.Open (Filename => "log_check.log", Session => Parser);

      while not AWK.End_Of_Data (Parser) loop
         AWK.Get_Line (Session => Parser);

         if AWK.Field (2, Parser) = "Stop"
           and AWK.Field (3, Parser) = "logging."
         then
            null;
         else
            Text_IO.Put_Line
              (AWK.Field (Filename, Parser) & " - "
               & AWK.Field (Status, Parser));
         end if;
      end loop;

      AWK.Close (Parser);
   end Parse_Logs;

begin
   Config.Set.Server_Name    (CNF, "log_check");
   Config.Set.Server_Host    (CNF, "localhost");
   Config.Set.Server_Port    (CNF, 0);
   Config.Set.Max_Connection (CNF, 1);

   Server.Start (WS, CB'Unrestricted_Access, CNF);
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
