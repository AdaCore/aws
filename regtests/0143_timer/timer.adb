------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2003-2009, AdaCore                     --
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

with Ada.Calendar.Formatting;
with Ada.Text_IO;

with AWS.Client;
with AWS.Config.Set;
with AWS.Dispatchers.Callback;
with AWS.MIME;
with AWS.Response;
with AWS.Server;
with AWS.Services.Dispatchers.Timer;
with AWS.Status;
with AWS.Utils;

with Get_Free_Port;

procedure Timer is

   use Ada;
   use AWS;
   use type Ada.Calendar.Time;

   Now  : Calendar.Time := Calendar.Clock;

   Year       : Calendar.Year_Number;
   Month      : Calendar.Month_Number;
   Day        : Calendar.Day_Number;
   Hour       : Calendar.Formatting.Hour_Number;
   Minute     : Calendar.Formatting.Minute_Number;
   Second     : Calendar.Formatting.Second_Number;
   Sub_Second : Calendar.Formatting.Second_Duration;

   WS   : Server.HTTP;
   Port : Natural := 1272;
   Disp : Services.Dispatchers.Timer.Handler;
   Conf : Config.Object;

   P1   : Services.Dispatchers.Timer.Period;

   ---------
   -- DCB --
   ---------

   function DCB (Request : Status.Data) return Response.Data is
   begin
      return Response.Build (MIME.Text_HTML, "This is the default dispatcher");
   end DCB;

   ---------
   -- CB1 --
   ---------

   function CB1 (Request : Status.Data) return Response.Data is
   begin
      return Response.Build (MIME.Text_HTML, "This is CB1 dispatcher");
   end CB1;

   R : Response.Data;

begin
   loop
      Calendar.Formatting.Split
        (Now, Year, Month, Day, Hour, Minute, Second, Sub_Second);
      exit when Second < 50;

      delay 1.0;
      Now := Calendar.Clock;
   end loop;

   Get_Free_Port (Port);

   --  Config

   Config.Set.Server_Port (Conf, Port);

   --  Dispatcher

   P1 := Services.Dispatchers.Timer.Minutely
     (From_Second => Second, To_Second => Second + 2);

   Services.Dispatchers.Timer.Register
     (Disp, "T1", P1, Dispatchers.Callback.Create (CB1'Unrestricted_Access));

   Services.Dispatchers.Timer.Register_Default_Callback
     (Disp, Dispatchers.Callback.Create (DCB'Unrestricted_Access));

   Server.Start (WS, Disp, Conf);

   Text_IO.Put_Line ("started"); Ada.Text_IO.Flush;

   R := Client.Get ("http://localhost:" & Utils.Image (Port));
   Text_IO.Put_Line ("> " & Response.Message_Body (R));

   delay 2.0;

   R := Client.Get ("http://localhost:" & Utils.Image (Port));
   Text_IO.Put_Line ("> " & Response.Message_Body (R));

   Server.Shutdown (WS);

   Text_IO.Put_Line ("shutdown");
end Timer;
