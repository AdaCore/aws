------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2003-2004                          --
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

--  $RCSfile$
--  $Revision$$ $Dat$ $Author$

with Ada.Unchecked_Deallocation;

with AWS.Dispatchers.Callback;
with AWS.MIME;

package body AWS.Services.Dispatchers.Timer is

   use AWS.Dispatchers;

   procedure Free is new Ada.Unchecked_Deallocation (Node, Node_Access);

   -----------
   -- Daily --
   -----------

   function Daily
     (From_Hour   : in Hour_Number;
      From_Minute : in Minute_Number;
      From_Second : in Second_Number;
      To_Hour     : in Hour_Number;
      To_Minute   : in Minute_Number;
      To_Second   : in Second_Number)
      return Period
   is
      P : Period;
   begin
      P.Mode        := Daily;
      P.From.Hour   := From_Hour;
      P.From.Minute := From_Minute;
      P.From.Second := From_Second;
      P.To.Hour     := To_Hour;
      P.To.Minute   := To_Minute;
      P.To.Second   := To_Second;
      return P;
   end Daily;

   --------------
   -- Dispatch --
   --------------

   function Dispatch
     (Dispatcher : in Handler;
      Request    : in Status.Data)
      return Response.Data
   is
      use type Calendar.Time;

      function Match_Once     (Item : in Node_Access) return Boolean;
      function Match_Yearly   (Item : in Node_Access) return Boolean;
      function Match_Monthly  (Item : in Node_Access) return Boolean;
      function Match_Weekly   (Item : in Node_Access) return Boolean;
      function Match_Daily    (Item : in Node_Access) return Boolean;
      function Match_Hourly   (Item : in Node_Access) return Boolean;
      function Match_Minutely (Item : in Node_Access) return Boolean;

      Now : constant Calendar.Time := Calendar.Clock;

      Year       : Year_Number;
      Month      : Month_Number;
      Day        : Day_Number;
      N_Day      : Day_Name;
      Hour       : Hour_Number;
      Minute     : Minute_Number;
      Second     : Second_Number;
      Sub_Second : GNAT.Calendar.Second_Duration;

      -----------------
      -- Match_Daily --
      -----------------

      function Match_Daily (Item : in Node_Access) return Boolean is
         F        : Date_Time renames Item.Period.From;
         T        : Date_Time renames Item.Period.To;
         From, To : Calendar.Time;
      begin
         From := GNAT.Calendar.Time_Of
           (Year, Month, Day, F.Hour, F.Minute, F.Second);
         To := GNAT.Calendar.Time_Of
           (Year, Month, Day, T.Hour, T.Minute, T.Second);

         return From <= Now and then Now <= To;
      end Match_Daily;

      ------------------
      -- Match_Hourly --
      ------------------

      function Match_Hourly (Item : in Node_Access) return Boolean is
         F        : Date_Time renames Item.Period.From;
         T        : Date_Time renames Item.Period.To;
         From, To : Calendar.Time;
      begin
         From := GNAT.Calendar.Time_Of
           (Year, Month, Day, Hour, F.Minute, F.Second);
         To := GNAT.Calendar.Time_Of
           (Year, Month, Day, Hour, T.Minute, T.Second);

         return From <= Now and then Now <= To;
      end Match_Hourly;

      --------------------
      -- Match_Minutely --
      --------------------

      function Match_Minutely (Item : in Node_Access) return Boolean is
         F        : Date_Time renames Item.Period.From;
         T        : Date_Time renames Item.Period.To;
         From, To : Calendar.Time;
      begin
         From := GNAT.Calendar.Time_Of
           (Year, Month, Day, Hour, Minute, F.Second);
         To := GNAT.Calendar.Time_Of
           (Year, Month, Day, Hour, Minute, T.Second);

         return From <= Now and then Now <= To;
      end Match_Minutely;

      -------------------
      -- Match_Monthly --
      -------------------

      function Match_Monthly (Item : in Node_Access) return Boolean is
         F        : Date_Time renames Item.Period.From;
         T        : Date_Time renames Item.Period.To;
         From, To : Calendar.Time;
      begin
         From := GNAT.Calendar.Time_Of
           (Year, Month, F.Day, F.Hour, F.Minute, F.Second);
         To := GNAT.Calendar.Time_Of
           (Year, Month, T.Day, T.Hour, T.Minute, T.Second);

         return From <= Now and then Now <= To;
      end Match_Monthly;

      ----------------
      -- Match_Once --
      ----------------

      function Match_Once (Item : in Node_Access) return Boolean is
         F        : Date_Time renames Item.Period.From;
         T        : Date_Time renames Item.Period.To;
         From, To : Calendar.Time;
      begin
         From := GNAT.Calendar.Time_Of
           (F.Year, F.Month, F.Day, F.Hour, F.Minute, F.Second);
         To := GNAT.Calendar.Time_Of
           (T.Year, T.Month, T.Day, T.Hour, T.Minute, T.Second);

         return From <= Now and then Now <= To;
      end Match_Once;

      ------------------
      -- Match_Weekly --
      ------------------

      function Match_Weekly (Item : in Node_Access) return Boolean is
         F        : Date_Time renames Item.Period.From;
         T        : Date_Time renames Item.Period.To;
         From, To : Calendar.Time;
      begin
         From := GNAT.Calendar.Time_Of
           (F.Year, F.Month,
            F.Day - (Day_Name'Pos (N_Day) - Day_Name'Pos (F.N_Day)),
            F.Hour, F.Minute, F.Second);
         To := GNAT.Calendar.Time_Of
           (T.Year, T.Month,
            T.Day + (Day_Name'Pos (T.N_Day) - Day_Name'Pos (N_Day)),
            T.Hour, T.Minute, T.Second);

         return From <= Now and then Now <= To;
      end Match_Weekly;

      ------------------
      -- Match_Yearly --
      ------------------

      function Match_Yearly (Item : in Node_Access) return Boolean is
         F        : Date_Time renames Item.Period.From;
         T        : Date_Time renames Item.Period.To;
         From, To : Calendar.Time;
      begin
         From := GNAT.Calendar.Time_Of
           (Year, F.Month, F.Day, F.Hour, F.Minute, F.Second);
         To := GNAT.Calendar.Time_Of
           (Year, T.Month, T.Day, T.Hour, T.Minute, T.Second);

         return From <= Now and then Now <= To;
      end Match_Yearly;

   begin
      GNAT.Calendar.Split
        (Now, Year, Month, Day, Hour, Minute, Second, Sub_Second);
      N_Day := GNAT.Calendar.Day_Of_Week (Now);

      for K in 1 .. Period_Table.Length (Dispatcher.Table) loop
         declare
            Item : constant Node_Access
              := Period_Table.Element (Dispatcher.Table, Natural (K));
         begin
            case Item.Period.Mode is

               when Once =>
                  if Match_Once (Item) then
                     return Dispatch (Item.Action.all, Request);
                  end if;

               when Yearly =>
                  if Match_Yearly (Item) then
                     return Dispatch (Item.Action.all, Request);
                  end if;

               when Monthly =>
                  if Match_Monthly (Item) then
                     return Dispatch (Item.Action.all, Request);
                  end if;

               when Weekly =>
                  if Match_Weekly (Item) then
                     return Dispatch (Item.Action.all, Request);
                  end if;

               when Daily =>
                  if Match_Daily (Item) then
                     return Dispatch (Item.Action.all, Request);
                  end if;

               when Hourly =>
                  if Match_Hourly (Item) then
                     return Dispatch (Item.Action.all, Request);
                  end if;

               when Minutely =>
                  if Match_Minutely (Item) then
                     return Dispatch (Item.Action.all, Request);
                  end if;
            end case;
         end;
      end loop;

      if Dispatcher.Action /= null then
         return Dispatch (Dispatcher.Action.all, Request);
      end if;

      return Response.Build
        (MIME.Text_HTML,
         "<p>AWS " & Version
           & "<p>No rule found for the time dispatcher and no "
           & "default dispatcher defined.");
   end Dispatch;

   --------------
   -- Finalize --
   --------------

   procedure Finalize   (Dispatcher : in out Handler) is
   begin
      Finalize (AWS.Dispatchers.Handler (Dispatcher));

      if Ref_Counter (Dispatcher) = 0 then
         for K in 1 .. Period_Table.Length (Dispatcher.Table) loop
            declare
               Item : Node_Access
                 := Period_Table.Element (Dispatcher.Table, Natural (K));
            begin
               Free (Item.Action);
               Free (Item);
            end;
         end loop;

         Period_Table.Clear (Dispatcher.Table);
      end if;
   end Finalize;

   ------------
   -- Hourly --
   ------------

   function Hourly
     (From_Minute : in Minute_Number;
      From_Second : in Second_Number;
      To_Minute   : in Minute_Number;
      To_Second   : in Second_Number)
      return Period
   is
      P : Period;
   begin
      P.Mode        := Hourly;
      P.From.Minute := From_Minute;
      P.From.Second := From_Second;
      P.To.Minute   := To_Minute;
      P.To.Second   := To_Second;
      return P;
   end Hourly;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Dispatcher : in out Handler) is
   begin
      Initialize (AWS.Dispatchers.Handler (Dispatcher));
   end Initialize;

   --------------
   -- Minutely --
   --------------

   function Minutely
     (From_Second : in Second_Number;
      To_Second   : in Second_Number)
      return Period
   is
      P : Period;
   begin
      P.Mode        := Minutely;
      P.From.Second := From_Second;
      P.To.Second   := To_Second;
      return P;
   end Minutely;

   -------------
   -- Monthly --
   -------------

   function Monthly
     (From_Day    : in Day_Number;
      From_Hour   : in Hour_Number;
      From_Minute : in Minute_Number;
      From_Second : in Second_Number;
      To_Day      : in Day_Number;
      To_Hour     : in Hour_Number;
      To_Minute   : in Minute_Number;
      To_Second   : in Second_Number)
      return Period
   is
      P : Period;
   begin
      P.Mode        := Monthly;
      P.From.Day    := From_Day;
      P.From.Hour   := From_Hour;
      P.From.Minute := From_Minute;
      P.From.Second := From_Second;
      P.To.Day      := To_Day;
      P.To.Hour     := To_Hour;
      P.To.Minute   := To_Minute;
      P.To.Second   := To_Second;
      return P;
   end Monthly;

   ----------
   -- Once --
   ----------

   function Once
     (From_Year   : in Year_Number;
      From_Month  : in Month_Number;
      From_Day    : in Day_Number;
      From_Hour   : in Hour_Number;
      From_Minute : in Minute_Number;
      From_Second : in Second_Number;
      To_Year     : in Year_Number;
      To_Month    : in Month_Number;
      To_Day      : in Day_Number;
      To_Hour     : in Hour_Number;
      To_Minute   : in Minute_Number;
      To_Second   : in Second_Number)
      return Period
   is
      P : Period;
   begin
      P.Mode        := Once;
      P.From.Year   := From_Year;
      P.From.Month  := From_Month;
      P.From.Day    := From_Day;
      P.From.Hour   := From_Hour;
      P.From.Minute := From_Minute;
      P.From.Second := From_Second;
      P.To.Year     := To_Year;
      P.To.Month    := To_Month;
      P.To.Day      := To_Day;
      P.To.Hour     := To_Hour;
      P.To.Minute   := To_Minute;
      P.To.Second   := To_Second;
      return P;
   end Once;

   --------------
   -- Register --
   --------------

   procedure Register
     (Dispatcher : in out Handler;
      Name       : in     String;
      Period     : in     Timer.Period;
      Action     : in     AWS.Dispatchers.Handler'Class)
   is
      Value : constant Node_Access
        := new Node'(To_Unbounded_String (Name),
                     Period,
                     new AWS.Dispatchers.Handler'Class'(Action));
   begin
      Period_Table.Append (Dispatcher.Table, Value);
   end Register;

   procedure Register
     (Dispatcher : in out Handler;
      Name       : in     String;
      Period     : in     Timer.Period;
      Action     : in     Response.Callback) is
   begin
      Register
        (Dispatcher, Name, Period, AWS.Dispatchers.Callback.Create (Action));
   end Register;

   -------------------------------
   -- Register_Default_Callback --
   -------------------------------

   procedure Register_Default_Callback
     (Dispatcher : in out Handler;
      Action     : in     AWS.Dispatchers.Handler'Class) is
   begin
      if Dispatcher.Action /= null then
         Free (Dispatcher.Action);
      end if;

      Dispatcher.Action := new AWS.Dispatchers.Handler'Class'(Action);
   end Register_Default_Callback;

   ----------------
   -- Unregister --
   ----------------

   procedure Unregister
     (Dispatcher : in out Handler;
      Name       : in     String) is
   begin
      for K in 1 .. Natural (Period_Table.Length (Dispatcher.Table)) loop
         declare
            Item : Node_Access
              := Period_Table.Element (Dispatcher.Table, K);
         begin
            if To_String (Item.Name) = Name then
               Free (Item);
               Period_Table.Delete (Dispatcher.Table, K);
               exit;
            end if;
         end;
      end loop;
   end Unregister;

   ------------
   -- Weekly --
   ------------

   function Weekly
     (From_Day    : in Day_Name;
      From_Hour   : in Hour_Number;
      From_Minute : in Minute_Number;
      From_Second : in Second_Number;
      To_Day      : in Day_Name;
      To_Hour     : in Hour_Number;
      To_Minute   : in Minute_Number;
      To_Second   : in Second_Number)
      return Period
   is
      P : Period;
   begin
      P.Mode        := Weekly;
      P.From.N_Day  := From_Day;
      P.From.Hour   := From_Hour;
      P.From.Minute := From_Minute;
      P.From.Second := From_Second;
      P.To.N_Day    := To_Day;
      P.To.Hour     := To_Hour;
      P.To.Minute   := To_Minute;
      P.To.Second   := To_Second;
      return P;
   end Weekly;

   ------------
   -- Yearly --
   ------------

   function Yearly
     (From_Month  : in Month_Number;
      From_Day    : in Day_Number;
      From_Hour   : in Hour_Number;
      From_Minute : in Minute_Number;
      From_Second : in Second_Number;
      To_Month    : in Month_Number;
      To_Day      : in Day_Number;
      To_Hour     : in Hour_Number;
      To_Minute   : in Minute_Number;
      To_Second   : in Second_Number)
      return Period
   is
      P : Period;
   begin
      P.Mode        := Yearly;
      P.From.Month  := From_Month;
      P.From.Day    := From_Day;
      P.From.Hour   := From_Hour;
      P.From.Minute := From_Minute;
      P.From.Second := From_Second;
      P.To.Month    := To_Month;
      P.To.Day      := To_Day;
      P.To.Hour     := To_Hour;
      P.To.Minute   := To_Minute;
      P.To.Second   := To_Second;
      return P;
   end Yearly;

end AWS.Services.Dispatchers.Timer;
