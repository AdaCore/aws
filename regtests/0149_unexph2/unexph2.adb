------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2003-2012, AdaCore                     --
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
with Ada.Strings.Fixed;
with Ada.Text_IO;

with GNAT.OS_Lib;

with AWS.Client;
with AWS.Exceptions;
with AWS.MIME;
with AWS.Response;
with AWS.Server.Status;
with AWS.Status;
with AWS.Log;
with AWS.Utils;

procedure Unexph2 is

   use Ada;
   use Ada.Text_IO;
   use AWS;
   use GNAT;

   HTTP : Server.HTTP;
   R    : Response.Data;

   ----------------------
   -- Create_500_Tmplt --
   ----------------------

   procedure Create_500_Tmplt is
      File : Text_IO.File_Type;
   begin
      Text_IO.Create (File, Text_IO.Out_File, "500.tmplt");
      Text_IO.Put_Line (File, "This is the 500.tmplt file content.");
      Text_IO.Close (File);
   end Create_500_Tmplt;

   ----------------------
   -- Delete_500_Tmplt --
   ----------------------

   procedure Delete_500_Tmplt is
      Success : Boolean;
   begin
      OS_Lib.Delete_File ("500.tmplt", Success);
   end Delete_500_Tmplt;

   ------------------
   -- No_Traceback --
   ------------------

   function No_Traceback (Str : String) return String is
      K1 : constant Natural := Strings.Fixed.Index (Str, "locations:");
      K2 : constant Natural := Strings.Fixed.Index (Str, "</pre>");
   begin
      if K1 = 0 or else K2 = 0 then
         return Str;
      else
         return Str (Str'First .. K1 + 9) & " .. " & Str (K2 .. Str'Last);
      end if;
   end No_Traceback;

   --------
   -- CB --
   --------

   function CB (Request : Status.Data) return Response.Data is
   begin
      raise Constraint_Error;
      return Response.Build (MIME.Text_HTML, "be happy.");
   end CB;

begin
   Server.Start
     (HTTP, "Test default unexpected exception handler",
      CB'Unrestricted_Access, Port => 0, Max_Connection => 3);

   R := Client.Get
     (AWS.Server.Status.Local_URL (HTTP) & "/test",
      Timeouts => Client.Timeouts
        (Connect => 1.0, Send => 2.0, Receive => 2.0, Response => 20.0));

   Text_IO.Put_Line ("----------------------");
   Text_IO.Put_Line (No_Traceback (Response.Message_Body (R)));

   Create_500_Tmplt;

   R := Client.Get
     (Server.Status.Local_URL (HTTP) & "/test",
      Timeouts => Client.Timeouts
        (Connect => 1.0, Send => 2.0, Receive => 2.0, Response => 20.0));

   Text_IO.Put_Line ("----------------------");
   Text_IO.Put_Line (Response.Message_Body (R));
   Text_IO.Put_Line ("----------------------");

   Delete_500_Tmplt;

   Server.Shutdown (HTTP);

exception
   when E : others =>
      Put_Line ("Main Error " & Ada.Exceptions.Exception_Information (E));
      Server.Shutdown (HTTP);
end Unexph2;
