------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2004-2014, AdaCore                     --
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
with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Ada.Exceptions;
with Ada.Strings.Unbounded;
with Ada.Strings.Hash;
with Ada.Task_Attributes;
with Ada.Task_Identification;
with Ada.Text_IO;

with AWS.Config.Set;
with AWS.Client;
with AWS.MIME;
with AWS.Net.Log;
with AWS.Net.SSL;
with AWS.Parameters;
with AWS.Response;
with AWS.Server.Status;
with AWS.Status;
with AWS.Utils;

package body S_HLoad_Pack is

   use Ada;
   use Ada.Exceptions;
   use Ada.Strings.Unbounded;

   use AWS;

   Max_Client   : constant := 18;
   Max_Line     : constant := 16;
   Client_Count : constant := 300;

   Client_Error : Boolean := False with Atomic;

   package String_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists (String);

   package Debug_Dumps is
     new Ada.Task_Attributes (String_Lists.List, String_Lists.Empty_List);

   procedure Debug_Output (Text : String);

   procedure Error_Callback
     (Socket : Net.Socket_Type'Class; Message : String);
   --  The callback procedure which is called for every socket error

   procedure Event_Callback
     (Action : Net.Log.Event_Type; Socket : Net.Socket_Type'Class);

   function CB (Request : Status.Data) return Response.Data;

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

   function CB (Request : Status.Data) return Response.Data is
      P : constant AWS.Parameters.List := AWS.Status.Parameters (Request);
   begin
      Interval_Timer.Stamp;

      return Response.Build
        (MIME.Text_HTML, "Data:" & AWS.Parameters.Get (P, "PARAM"));
   end CB;

   Counter : Utils.Counter (100_000);

   ------------------
   -- Debug_Output --
   ------------------

   procedure Debug_Output (Text : String) is
      Value : Natural;
   begin
      Counter.Increment (Value => Value);

      declare
         Cnt_Img : constant String := Natural'Image (Value);
      begin
         Debug_Dumps.Reference.Append
           (Cnt_Img (Cnt_Img'Last - 4 .. Cnt_Img'Last)  & ' ' & Text);
      end;
   end Debug_Output;

   --------------------
   -- Error_Callback --
   --------------------

   procedure Error_Callback
     (Socket : Net.Socket_Type'Class; Message : String)
   is
      FD : constant Integer := Socket.Get_FD;
   begin
      if FD = Net.No_Socket then
         Debug_Output (Message & ASCII.LF);
      else
         Debug_Output
           (Socket.Get_Port'Img & Socket.Peer_Port'Img & FD'Img & ' '
            & Message & ASCII.LF);
      end if;

   exception
      when E : others =>
         Debug_Output
           (Message & ' ' & Exception_Message (E) & ASCII.LF);
   end Error_Callback;

   --------------------
   -- Event_Callback --
   --------------------

   procedure Event_Callback
     (Action : Net.Log.Event_Type; Socket : Net.Socket_Type'Class)
   is
      FD : constant Integer := Socket.Get_FD;
   begin
      if FD = Net.No_Socket then
         Debug_Output (Action'Img & " closed socket" & ASCII.LF);
      else
         Debug_Output
           (Action'Img
            & (if Net.Log."=" (Action, Net.Log.Connect)
               then Socket.Peer_Port'Img & Socket.Get_Port'Img
               else Socket.Get_Port'Img & Socket.Peer_Port'Img)
            & FD'Img & ' ' & ASCII.LF);
      end if;

   exception
      when E : others =>
         Debug_Output
           (Action'Img & FD'Img & ' ' & Exception_Message (E) & ASCII.LF);
   end Event_Callback;

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

   procedure Run (Timed : Boolean := False; Debug : Boolean := False) is

      task type Client is
         entry Start (Name : String);
         entry Pause;
         entry Stop;
      end Client;

      WS  : Server.HTTP;
      Cfg : Config.Object;
      Sfg : Net.SSL.Config;

      Clients : array (1 .. Max_Client) of Client;
      Hang : array (Clients'Range) of Boolean := (others => False);

      Delay_Time : Duration := 60.0;

      procedure Dump_Task (Id : Task_Identification.Task_Id; Hang : Boolean);

      ------------
      -- Client --
      ------------

      task body Client is
         Name : Unbounded_String;
         Connect : AWS.Client.HTTP_Connection;
      begin
         accept Start (Name : String) do
            Client.Name := To_Unbounded_String (Name);
         end Start;

         AWS.Client.Create
           (Connect,
            AWS.Server.Status.Local_URL (WS),
            Timeouts => AWS.Client.Timeouts
              (Connect => 5.0,
               Send => 15.0, Receive => 15.0, Response => 15.0));

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

                     Client_Error := True;
                     exit;
                  end if;
               end;

            exception
               when E : others =>
                  Text_IO.Put_Line
                    ("client " & To_String (Name)
                     & " request " & Utils.Image (K) & " aborted.");
                  Text_IO.Put_Line
                    (" => " & Exceptions.Exception_Information (E));

                  Client_Error := True;
                  exit;
            end;
         end loop;

         AWS.Client.Close (Connect);

         accept Pause;

         accept Stop;
      end Client;

      ---------------
      -- Dump_Task --
      ---------------

      procedure Dump_Task (Id : Task_Identification.Task_Id; Hang : Boolean) is
         Dump : String_Lists.List;
         DFO  : Text_IO.File_Type;
      begin
         if not Debug then
            return;
         end if;

         Dump := Debug_Dumps.Value (Id);

         Text_IO.Create (DFO, Name => Task_Identification.Image (Id));

         if Hang then
            Text_IO.Put_Line (DFO, "HANG");
         end if;

         Text_IO.Put_Line (DFO, Net.SSL.Version);

         for Line of Dump loop
            Text_IO.Put (DFO, Line);
         end loop;

         Text_IO.Close (DFO);
      end Dump_Task;

   begin -- Run
      Interval_Timer.Reset;

      if Debug then
         Net.SSL.Set_Debug (7, Debug_Output'Access);

         Net.Log.Start
           (Write => null,
            Event => Event_Callback'Access,
            Error => Error_Callback'Access);
      end if;

      Config.Set.Server_Name        (Cfg, "Heavy Loaded");
      Config.Set.Server_Port        (Cfg, 0);
      Config.Set.Security           (Cfg, True);
      Config.Set.Max_Connection     (Cfg, Max_Line);
      Config.Set.Session            (Cfg, True);
      Config.Set.TLS_Ticket_Support (Cfg, False);

      --  Set SSL session size a bit less than number of clients, to provoke
      --  session extrusion.

      Config.Set.SSL_Session_Cache_Size (Cfg, Max_Client - 1);

      Server.Start (WS, CB'Access, Cfg);

      Ada.Text_IO.Put_Line ("server started."); Ada.Text_IO.Flush;

      delay 1.0;

      for K in Clients'Range loop
         Clients (K).Start ("client" & Utils.Image (K));
         Text_IO.Put_Line ("client " & Utils.Image (K) & " started.");
      end loop;

      for K in Clients'Range loop
         select Clients (K).Pause;
         or delay Delay_Time;
            Text_IO.Put_Line ("Client" & K'Img & " hangs.");
            Hang (K) := True;
            Client_Error := True;
            Delay_Time := Delay_Time / 3;
         end select;
      end loop;

      if Client_Error then
         --  Dump debug output of the all clients and server lines

         for K in Clients'Range loop
            Dump_Task (Clients (K)'Identity, Hang (K));
         end loop;

         declare
            Tasks : constant Server.Task_Id_Array := Server.Line_Tasks (WS);
         begin
            for K in Tasks'Range loop
               Dump_Task (Tasks (K), False);
            end loop;
         end;
      end if;

      for K in Clients'Range loop
         Text_IO.Put ("client " & Utils.Image (K));

         if Hang (K) then
            abort Clients (K);
            Text_IO.Put_Line (" aborted.");
         else
            Clients (K).Stop;
            Text_IO.Put_Line (" stopped.");
         end if;
      end loop;

      Server.Shutdown (WS);
      Ada.Text_IO.Put_Line ("server stopped.");

      Ada.Text_IO.Put_Line (Interval_Timer.Statistic_Image (Timed));

   exception
      when E : others =>
         Text_IO.Put_Line
           ("main task" & ASCII.LF & Exceptions.Exception_Information (E));
   end Run;

end S_HLoad_Pack;
