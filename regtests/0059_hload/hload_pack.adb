------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2004-2012, AdaCore                     --
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

--  Test for heavy loading

with Ada.Calendar;
with Ada.Exceptions;
with Ada.Text_IO;
with Ada.Strings.Unbounded;

with GNAT.OS_Lib;

with AWS.Client;
with AWS.Config.Set;
with AWS.MIME;
with AWS.Parameters;
with AWS.Response;
with AWS.Server.Status;
with AWS.Status;
with AWS.Utils;

package body HLoad_Pack is

   use Ada;
   use Ada.Strings.Unbounded;

   use AWS;

   Max_Client   : constant := 18;
   Max_Line     : constant := 16;
   Client_Count : constant := 300;

   function CB (Request : Status.Data) return Response.Data;

   subtype Count is Long_Integer;

   protected Interval_Timer is
      procedure Reset;
      procedure Stamp;
      function Statistic_Image (Timed : Boolean) return String;

   private
      Last  : Ada.Calendar.Time;
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

   function CB (Request : Status.Data) return Response.Data is
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

   procedure Run (Protocol : String; Timed : Boolean := False) is

      task type Client is
         entry Start (Name : String);
         entry Stop;
      end Client;

      WS  : Server.HTTP;
      CNF : Config.Object;

      Clients : array (1 .. Max_Client) of Client;

      ------------
      -- Client --
      ------------

      task body Client is
         Name    : Unbounded_String;
         Connect : AWS.Client.HTTP_Connection;
      begin
         accept Start (Name : String) do
            Client.Name := To_Unbounded_String (Name);
         end Start;

         AWS.Client.Create
           (Connect,
            AWS.Server.Status.Local_URL (WS),
            Timeouts => AWS.Client.Timeouts
              (Connect  => 15.0,
               Send     => 15.0,
               Receive  => 15.0,
               Response => 15.0));

         for K in 1 .. Client_Count loop
            begin
               declare
                  K_Img    : constant String :=
                               Utils.Image (K) & '-' & To_String (Name);
                  R        : AWS.Response.Data;
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

      exception
         when E : others =>
            Text_IO.Put_Line ("client " & To_String (Name) & " aborted.");
            Text_IO.Put_Line
              (" => " & Exceptions.Exception_Information (E));
            accept Stop;
      end Client;

   begin
      Interval_Timer.Reset;

      Config.Set.Server_Name    (CNF, "Heavy Loaded");
      Config.Set.Server_Host    (CNF, "localhost");
      Config.Set.Server_Port    (CNF, 0);
      Config.Set.Security       (CNF, Protocol = "https");
      Config.Set.Max_Connection (CNF, Max_Line);
      Config.Set.Session        (CNF, True);

      Server.Start (WS, CB'Access, CNF);

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
         GNAT.OS_Lib.OS_Exit (1);
   end Run;

end HLoad_Pack;
