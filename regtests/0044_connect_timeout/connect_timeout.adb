------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2008-2012, AdaCore                     --
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
with Ada.Text_IO;
with Ada.Calendar;

with AWS.Client;
with AWS.Net;
with AWS.Response;

procedure Connect_Timeout is

   use Ada;
   use Ada.Exceptions;
   use AWS;

   procedure Start_Time;
   --  Record starting time

   procedure Stop_Time;
   --  Record ending time

   procedure Check_Duration (D : Duration; Message : String);
   --  Check that duration is about D seconds

   Resp        : Response.Data;
   Start, Stop : Calendar.Time;

   --------------------
   -- Check_Duration --
   --------------------

   procedure Check_Duration (D : Duration; Message : String) is
      use type Calendar.Time;
      Elaps : constant Duration := Stop - Start - D;
   begin
      if abs (Elaps) < 1.0 then
         Text_IO.Put_Line ("OK:" & Message);
      else
         Text_IO.Put_Line ("NOK:" & Message & ", " & Duration'Image (Elaps));
      end if;
   end Check_Duration;

   ----------------
   -- Start_Time --
   ----------------

   procedure Start_Time is
   begin
      Start := Calendar.Clock;
   end Start_Time;

   ---------------
   -- Stop_Time --
   ---------------

   procedure Stop_Time is
   begin
      Stop := Calendar.Clock;
   end Stop_Time;

begin
   begin
      Start_Time;
      Resp := Client.Get
        ("http://www.google.com:9264",
         Timeouts => Client.Timeouts
           (Connect => 5.0, Send => 2.0, Receive => 4.0));
      Stop_Time;
      Check_Duration (5.0, "GET");
      Text_IO.Put_Line (Response.Message_Body (Resp));
   exception
      when E : others =>
         Text_IO.Put_Line (Exception_Message (E));
   end;

   begin
      Start_Time;
      Resp := Client.Post
        ("http://www.google.com:9264", "toto", "text/plain",
         Timeouts => Client.Timeouts
           (Connect => 2.0, Send => 2.0, Receive => 2.0));
      Stop_Time;
      Check_Duration (2.0, "POST");
      Text_IO.Put_Line (Response.Message_Body (Resp));
   exception
      when E : others =>
         Text_IO.Put_Line (Exception_Message (E));
   end;
end Connect_Timeout;
