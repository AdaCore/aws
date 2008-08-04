------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                        Copyright (C) 2008, AdaCore                       --
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

   procedure Check_Duration (D : in Duration; Message : in String);
   --  Check that duration is about D seconds

   Resp        : Response.Data;
   Start, Stop : Calendar.Time;

   --------------------
   -- Check_Duration --
   --------------------

   procedure Check_Duration (D : in Duration; Message : in String) is
      use type Calendar.Time;
      Elaps : constant Duration := Stop - Start - D;
   begin
      if abs (Elaps) < 0.2 then
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
         Timeouts => (5.0, 2.0, 4.0));
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
         Timeouts => (2.0, 2.0, 2.0));
      Stop_Time;
      Check_Duration (2.0, "POST");
      Text_IO.Put_Line (Response.Message_Body (Resp));
   exception
      when E : others =>
         Text_IO.Put_Line (Exception_Message (E));
   end;
end Connect_Timeout;
