------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                            Copyright (C) 2004                            --
--                                ACT-Europe                                --
--                                                                          --
--  Authors: Dmitriy Anisimkov - Pascal Obry                                --
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

--  $Id$

--  Test for heavy loading.

with Ada.Calendar;
with Ada.Exceptions;
with Ada.Text_IO;
with Ada.Strings.Unbounded;

with AWS.Server;
with AWS.Response;
with AWS.Status;
with AWS.MIME;
with AWS.Client;
with AWS.Parameters;
with AWS.Utils;

with Get_Free_Port;

package body HLoad_Pack is

   use Ada;
   use Ada.Strings.Unbounded;

   use AWS;

   Max_Client   : constant := 18;
   Max_Line     : constant := 16;
   Client_Count : constant := 300;

   function CB (Request : in Status.Data) return Response.Data;

   subtype Count is Long_Integer;

   protected Interval_Timer is
      procedure Reset;
      procedure Stamp;
      function Statistic_Image (Timed : Boolean) return String;

   private
      Last : Ada.Calendar.Time;
      Start : Ada.Calendar.Time;

      Counter   : Count;
      Max_Index : Count;
      Min_Index : Count;

      Max : Duration;
      Min : Duration;
   end Interval_Timer;

   --------
   -- CB --
   --------

   function CB (Request : in Status.Data) return Response.Data is
      P : constant AWS.Parameters.List := AWS.Status.Parameters (Request);
   begin
      Interval_Timer.Stamp;

      return Response.Build
        (MIME.Text_HTML, "Data:" & AWS.Parameters.Get (P, "PARAM"));
   end CB;

   --------------------
   -- Interval_Timer --
   --------------------

   protected body Interval_Timer is

      -----------
      -- Reset --
      -----------

      procedure Reset is
      begin
         Counter   := 0;
         Max_Index := 0;
         Min_Index := 0;
         Max       := 0.0;
         Min       := Duration'Last;
      end Reset;

      -----------
      -- Stamp --
      -----------

      procedure Stamp is
         use type Ada.Calendar.Time;

         Now : constant Ada.Calendar.Time := Ada.Calendar.Clock;
         Interval : Duration;
      begin
         if Counter = 0 then
            Last  := Now;
            Start := Now;
         else
            Interval := Now - Last;

            if Interval > Max then
               Max       := Interval;
               Max_Index := Counter;

            elsif Interval < Min then
               Min       := Interval;
               Min_Index := Counter;
            end if;
         end if;

         Last    := Now;
         Counter := Counter + 1;
      end Stamp;

      ---------------------
      -- Statistic_Image --
      ---------------------

      function Statistic_Image (Timed : Boolean) return String is
         use type Ada.Calendar.Time;
         Result : constant String := "Counter:" & Count'Image (Counter);
      begin
         if Timed then
            return Result & ASCII.LF
              & "Min: index" & Count'Image (Min_Index)
                & "; interval:" & Duration'Image (Min) & " sec." & ASCII.LF
              & "Max: index" & Count'Image (Max_Index)
                & "; interval:" & Duration'Image (Max) & " sec." & ASCII.LF
              & "Average:"
                & Duration'Image (Duration (Counter) / (Last - Start))
                & " requests in second.";

         else
            return Result;

         end if;
      end Statistic_Image;

   end Interval_Timer;

   ---------
   -- Run --
   ---------

   procedure Run
     (Protocol : in String;
      Port     : in Positive;
      Timed    : in Boolean := False) is

      task type Client is
         entry Start (Name : in String);
         entry Stop;
      end Client;

      WS : Server.HTTP;

      Free_Port : Positive := Port;

      Clients : array (1 .. Max_Client) of Client;

      ------------
      -- Client --
      ------------

      task body Client is
         Name : Unbounded_String;
         Connect : AWS.Client.HTTP_Connection;
      begin
         accept Start (Name : in String) do
            Client.Name := To_Unbounded_String (Name);
         end Start;

         AWS.Client.Create
           (Connect,
            Protocol & "://localhost:" & Utils.Image (Free_Port),
            Timeouts => (15.0, 15.0));

         for K in 1 .. Client_Count loop
            begin
               declare
                  K_Img : constant String
                    := Utils.Image (K) & '-' & To_String (Name);

                  R : AWS.Response.Data;
                  Expected : constant String := "Data:" & K_Img;
               begin
                  AWS.Client.Get (Connect, R, "/toto?PARAM=" & K_Img);

                  if Expected /= String'(AWS.Response.Message_Body (R)) then
                     Text_IO.Put_Line
                       ("nok " & K_Img
                        & " expected " & Expected
                        & " -> found "
                        & String'(AWS.Response.Message_Body (R)));
                  end if;
               end;
            exception
               when E : others =>
                  Text_IO.Put_Line
                    ("client " & To_String (Name)
                     & " request " & Utils.Image (K) & " aborted.");
                  Text_IO.Put_Line
                    (" => " & Exceptions.Exception_Information (E));
            end;
         end loop;

         AWS.Client.Close (Connect);

         accept Stop;
      end Client;

   begin
      Get_Free_Port (Free_Port);
      Interval_Timer.Reset;

      Server.Start
        (WS,
         "Heavy Loaded",
         CB'Access,
         Port           => Free_Port,
         Security       => Protocol = "https",
         Max_Connection => Max_Line,
         Session        => True);

      Ada.Text_IO.Put_Line ("server started."); Ada.Text_IO.Flush;

      delay 1.0;

      for K in Clients'Range loop
         Clients (K).Start ("client" & Utils.Image (K));
         Text_IO.Put_Line ("client " & Utils.Image (K) & " started.");
      end loop;

      for K in Clients'Range loop
         Clients (K).Stop;
         Text_IO.Put_Line ("client " & Utils.Image (K) & " stopped.");
      end loop;

      Server.Shutdown (WS);
      Ada.Text_IO.Put_Line ("server stopped.");

      Ada.Text_IO.Put_Line (Interval_Timer.Statistic_Image (Timed));

   exception
      when E : others =>
         Text_IO.Put_Line
           ("main task" & ASCII.LF & Exceptions.Exception_Information (E));
   end Run;

end HLoad_Pack;
