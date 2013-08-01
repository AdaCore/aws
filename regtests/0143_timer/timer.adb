------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2003-2013, AdaCore                     --
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

with Ada.Calendar.Formatting;
with Ada.Text_IO;

with AWS.Client;
with AWS.Config.Set;
with AWS.Dispatchers.Callback;
with AWS.MIME;
with AWS.Response;
with AWS.Server.Status;
with AWS.Services.Dispatchers.Timer;
with AWS.Status;
with AWS.Utils;

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
   Services.Dispatchers.Timer.Register_Default_Callback
     (Disp, Dispatchers.Callback.Create (DCB'Unrestricted_Access));

   loop
      Calendar.Formatting.Split
        (Now, Year, Month, Day, Hour, Minute, Second, Sub_Second);
      exit when Second < 45;

      delay 1.0;
      Now := Calendar.Clock;
   end loop;

   --  Config

   Config.Set.Server_Port (Conf, 0);

   --  Dispatcher

   P1 := Services.Dispatchers.Timer.Minutely
     (From_Second => Second, To_Second => Second + 3);

   Services.Dispatchers.Timer.Register
     (Disp, "T1", P1, Dispatchers.Callback.Create (CB1'Unrestricted_Access));

   Server.Start (WS, Disp, Conf);

   Text_IO.Put_Line ("started"); Ada.Text_IO.Flush;

   R := Client.Get (Server.Status.Local_URL (WS));
   Text_IO.Put_Line ("> " & Response.Message_Body (R));

   delay 3.0;

   R := Client.Get (Server.Status.Local_URL (WS));
   Text_IO.Put_Line ("> " & Response.Message_Body (R));

   Server.Shutdown (WS);

   Text_IO.Put_Line ("shutdown");
end Timer;
