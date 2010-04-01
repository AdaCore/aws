------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2008-2010, AdaCore                     --
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
with Ada.Exceptions;
with Ada.Text_IO;

with AWS.Client;

with R_Hello_Timeout_Demo.Client;
with R_Hello_Timeout_Demo.Server;

procedure Test_SOAP_Timeout is

   use Ada;
   use Ada.Exceptions;
   use AWS;

   procedure Start_Time;
   --  Record starting time

   procedure Stop_Time;
   --  Record ending time

   procedure Check_Duration (D : Duration; Message : String);
   --  Check that duration is about D seconds

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
   Start_Time;
   declare
      R : constant String := R_Hello_Timeout_Demo.Client.sayHello
        ("pascal",
         Endpoint => "http://www.google.com:9856",
         Timeouts => Client.Timeouts
           (Connect => 4.0, Send => 2.0, Receive => 1.0));
   begin
      Text_IO.Put_Line ("Result: '" & R & ''');
   end;
exception
   when E : others =>
      Stop_Time;
      Check_Duration (4.0, "SOAP");
      Text_IO.Put_Line ("Exception: " & Exception_Name (E));
      Text_IO.Put_Line ("Exception: " & Exception_Message (E));
end Test_SOAP_Timeout;
