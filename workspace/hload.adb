
--  $Id$

--  See Sock. This could be used as a regression test when bug showed by
--  sock.adb will be fixed.

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

procedure Hload is

   use Ada;
   use Ada.Strings.Unbounded;

   Max_Client   : constant := 18;
   Client_Count : constant := 300;

   function Image (K : in Positive) return String;

   function Image (K : in Positive) return String is
      K_Img : constant String := Positive'Image (K);
   begin
      return K_Img (K_Img'First + 1 .. K_Img'Last);
   end Image;

   task type Client is
      entry Start (Name : in String);
      entry Stop;
   end Client;

   subtype Count is Long_Integer;

   protected Interval_Timer is
      procedure Stamp;
      procedure Print_Statistic;
   private
      Last : Ada.Calendar.Time;
      Start : Ada.Calendar.Time;

      Counter   : Count := 0;
      Max_Index : Count := 0;
      Min_Index : Count := 0;

      Max : Duration := 0.0;
      Min : Duration := Duration'Last;
   end Interval_Timer;

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
        (Connect, "http://localhost:1234", Timeouts => (15, 15));

      for K in 1 .. Client_Count loop
         begin
            declare
               K_Img : constant String := Image (K) & '-' & To_String (Name);
               R : AWS.Response.Data;
               Expected : constant String := "Data:" & K_Img;
            begin
               AWS.Client.Get (Connect, R, "/toto?PARAM=" & K_Img);

               if Expected /= String'(AWS.Response.Message_Body (R)) then
                  Text_IO.Put_Line
                    ("nok " & K_Img
                     & " expected " & Expected
                     & " -> found " & String'(AWS.Response.Message_Body (R)));
               end if;
            end;
         exception
            when E : others =>
               Text_IO.Put_Line
                 ("client " & To_String (Name)
                  & " request " & Image (K) & " aborted.");
               Text_IO.Put_Line
                 (" => " & Exceptions.Exception_Information (E));
         end;
      end loop;

      accept Stop;

      Text_IO.Put_Line ("client " & To_String (Name) & " stopped.");

   end Client;

   --------------------
   -- Interval_Timer --
   --------------------

   protected body Interval_Timer is

      ---------------------
      -- Print_Statistic --
      ---------------------

      procedure Print_Statistic is
         use type Ada.Calendar.Time;
      begin
         Ada.Text_IO.Put_Line
           ("Counter:" & Count'Image (Counter) & ASCII.LF
            & "Min: index" & Count'Image (Min_Index)
              & "; interval:" & Duration'Image (Min) & " sec." & ASCII.LF
            & "Max: index" & Count'Image (Max_Index)
              & "; interval:" & Duration'Image (Max) & " sec." & ASCII.LF
            & "Average:" & Duration'Image (Duration (Counter) / (Last - Start))
              & " requests in second.");
      end Print_Statistic;

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

   end Interval_Timer;

   use AWS;

   WS : Server.HTTP;

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

   Clients : array (1 .. Max_Client) of Client;

begin
   Server.Start
     (WS,
      "Heavy Loaded",
      CB'Unrestricted_Access,
      Port           => 1234,
      Max_Connection => 16,
      Session        => True);

   Ada.Text_IO.Put_Line ("started"); Ada.Text_IO.Flush;

   delay 1.0;

   for K in Clients'Range loop
      Clients (K).Start ("client" & Image (K));
   end loop;

   for K in Clients'Range loop
      Clients (K).Stop;
   end loop;

   Server.Shutdown (WS);
   Ada.Text_IO.Put_Line ("shutdown");

   Interval_Timer.Print_Statistic;

exception
   when E : others =>
      Text_IO.Put_Line
        ("main task" & ASCII.LF & Exceptions.Exception_Information (E));
end Hload;
