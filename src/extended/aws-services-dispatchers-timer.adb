------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2003-2014, AdaCore                     --
--                                                                          --
--  This library is free software;  you can redistribute it and/or modify   --
--  it under terms of the  GNU General Public License  as published by the  --
--  Free Software  Foundation;  either version 3,  or (at your  option) any --
--  later version. This library is distributed in the hope that it will be  --
--  useful, but WITHOUT ANY WARRANTY;  without even the implied warranty of --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                    --
--                                                                          --
--  As a special exception under Section 7 of GPL version 3, you are        --
--  granted additional permissions described in the GCC Runtime Library     --
--  Exception, version 3.1, as published by the Free Software Foundation.   --
--                                                                          --
--  You should have received a copy of the GNU General Public License and   --
--  a copy of the GCC Runtime Library Exception along with this program;    --
--  see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see   --
--  <http://www.gnu.org/licenses/>.                                         --
--                                                                          --
--  As a special exception, if other files instantiate generics from this   --
--  unit, or you link this unit with other files to produce an executable,  --
--  this  unit  does not  by itself cause  the resulting executable to be   --
--  covered by the GNU General Public License. This exception does not      --
--  however invalidate any other reasons why the executable file  might be  --
--  covered by the  GNU Public License.                                     --
------------------------------------------------------------------------------

pragma Ada_2012;

with Ada.Unchecked_Deallocation;

with AWS.Dispatchers.Callback;
with AWS.Messages;

package body AWS.Services.Dispatchers.Timer is

   use AWS.Dispatchers;

   procedure Unchecked_Free is
     new Ada.Unchecked_Deallocation (Node, Node_Access);

   -----------
   -- Clone --
   -----------

   overriding function Clone (Dispatcher : Handler) return Handler is
      New_Dispatcher : Handler;
   begin
      if Dispatcher.Action /= null then
         New_Dispatcher.Action :=
           new AWS.Dispatchers.Handler'Class'
             (AWS.Dispatchers.Handler'Class (Dispatcher.Action.Clone));
      end if;

      for K in 1 .. Period_Table.Length (Dispatcher.Table) loop
         declare
            Item     : constant Node_Access :=
                         Period_Table.Element (Dispatcher.Table, Natural (K));
            New_Item : constant Node_Access := new Node'(Item.all);
         begin
            if Item.Action /= null then
               New_Item.Action :=
                 new AWS.Dispatchers.Handler'Class'
                   (AWS.Dispatchers.Handler'Class (Item.Action.Clone));
            end if;

            Period_Table.Insert
              (Container => New_Dispatcher.Table,
               Before    => Natural (K),
               New_Item  => New_Item);
         end;
      end loop;

      return New_Dispatcher;
   end Clone;

   -----------
   -- Daily --
   -----------

   function Daily
     (From_Hour   : Hour_Number;
      From_Minute : Minute_Number;
      From_Second : Second_Number;
      To_Hour     : Hour_Number;
      To_Minute   : Minute_Number;
      To_Second   : Second_Number) return Period is
   begin
      return Period'(Mode => Daily,
                     From => (Hour   => From_Hour,
                              Minute => From_Minute,
                              Second => From_Second,
                              others => <>),
                     To   => (Hour   => To_Hour,
                              Minute => To_Minute,
                              Second => To_Second,
                              others => <>));
   end Daily;

   --------------
   -- Dispatch --
   --------------

   overriding function Dispatch
     (Dispatcher : Handler;
      Request    : Status.Data) return Response.Data
   is
      use type Calendar.Time;

      function Match_Once     (Item : Node_Access) return Boolean;
      function Match_Yearly   (Item : Node_Access) return Boolean;
      function Match_Monthly  (Item : Node_Access) return Boolean;
      function Match_Weekly   (Item : Node_Access) return Boolean;
      function Match_Daily    (Item : Node_Access) return Boolean;
      function Match_Hourly   (Item : Node_Access) return Boolean;
      function Match_Minutely (Item : Node_Access) return Boolean;

      Now : constant Calendar.Time := Calendar.Clock;

      Year       : Year_Number;
      Month      : Month_Number;
      Day        : Day_Number;
      N_Day      : Day_Name;
      Hour       : Hour_Number;
      Minute     : Minute_Number;
      Second     : Second_Number;
      Sub_Second : Calendar.Formatting.Second_Duration;

      -----------------
      -- Match_Daily --
      -----------------

      function Match_Daily (Item : Node_Access) return Boolean is
         F        : Date_Time renames Item.Period.From;
         T        : Date_Time renames Item.Period.To;
         From, To : Calendar.Time;
      begin
         From := Calendar.Formatting.Time_Of
           (Year, Month, Day, F.Hour, F.Minute, F.Second);
         To := Calendar.Formatting.Time_Of
           (Year, Month, Day, T.Hour, T.Minute, T.Second);

         return From <= Now and then Now <= To;
      end Match_Daily;

      ------------------
      -- Match_Hourly --
      ------------------

      function Match_Hourly (Item : Node_Access) return Boolean is
         F        : Date_Time renames Item.Period.From;
         T        : Date_Time renames Item.Period.To;
         From, To : Calendar.Time;
      begin
         From := Calendar.Formatting.Time_Of
           (Year, Month, Day, Hour, F.Minute, F.Second);
         To := Calendar.Formatting.Time_Of
           (Year, Month, Day, Hour, T.Minute, T.Second);

         return From <= Now and then Now <= To;
      end Match_Hourly;

      --------------------
      -- Match_Minutely --
      --------------------

      function Match_Minutely (Item : Node_Access) return Boolean is
         F        : Date_Time renames Item.Period.From;
         T        : Date_Time renames Item.Period.To;
         From, To : Calendar.Time;
      begin
         From := Calendar.Formatting.Time_Of
           (Year, Month, Day, Hour, Minute, F.Second);
         To := Calendar.Formatting.Time_Of
           (Year, Month, Day, Hour, Minute, T.Second);

         return From <= Now and then Now <= To;
      end Match_Minutely;

      -------------------
      -- Match_Monthly --
      -------------------

      function Match_Monthly (Item : Node_Access) return Boolean is
         F        : Date_Time renames Item.Period.From;
         T        : Date_Time renames Item.Period.To;
         From, To : Calendar.Time;
      begin
         From := Calendar.Formatting.Time_Of
           (Year, Month, F.Day, F.Hour, F.Minute, F.Second);
         To := Calendar.Formatting.Time_Of
           (Year, Month, T.Day, T.Hour, T.Minute, T.Second);

         return From <= Now and then Now <= To;
      end Match_Monthly;

      ----------------
      -- Match_Once --
      ----------------

      function Match_Once (Item : Node_Access) return Boolean is
         F        : Date_Time renames Item.Period.From;
         T        : Date_Time renames Item.Period.To;
         From, To : Calendar.Time;
      begin
         From := Calendar.Formatting.Time_Of
           (F.Year, F.Month, F.Day, F.Hour, F.Minute, F.Second);
         To := Calendar.Formatting.Time_Of
           (T.Year, T.Month, T.Day, T.Hour, T.Minute, T.Second);

         return From <= Now and then Now <= To;
      end Match_Once;

      ------------------
      -- Match_Weekly --
      ------------------

      function Match_Weekly (Item : Node_Access) return Boolean is
         F        : Date_Time renames Item.Period.From;
         T        : Date_Time renames Item.Period.To;
         From, To : Calendar.Time;
      begin
         From := Calendar.Formatting.Time_Of
           (F.Year, F.Month,
            F.Day - (Day_Name'Pos (N_Day) - Day_Name'Pos (F.N_Day)),
            F.Hour, F.Minute, F.Second);
         To := Calendar.Formatting.Time_Of
           (T.Year, T.Month,
            T.Day + (Day_Name'Pos (T.N_Day) - Day_Name'Pos (N_Day)),
            T.Hour, T.Minute, T.Second);

         return From <= Now and then Now <= To;
      end Match_Weekly;

      ------------------
      -- Match_Yearly --
      ------------------

      function Match_Yearly (Item : Node_Access) return Boolean is
         F        : Date_Time renames Item.Period.From;
         T        : Date_Time renames Item.Period.To;
         From, To : Calendar.Time;
      begin
         From := Calendar.Formatting.Time_Of
           (Year, F.Month, F.Day, F.Hour, F.Minute, F.Second);
         To := Calendar.Formatting.Time_Of
           (Year, T.Month, T.Day, T.Hour, T.Minute, T.Second);

         return From <= Now and then Now <= To;
      end Match_Yearly;

   begin
      Calendar.Formatting.Split
        (Now, Year, Month, Day, Hour, Minute, Second, Sub_Second);
      N_Day := Calendar.Formatting.Day_Of_Week (Now);

      for Item of Dispatcher.Table loop
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
      end loop;

      if Dispatcher.Action /= null then
         return Dispatch (Dispatcher.Action.all, Request);
      end if;

      return Response.Acknowledge
        (Messages.S404,
         "<p>AWS " & Version
           & "<p>No rule found for the time dispatcher and no "
           & "default dispatcher defined.");
   end Dispatch;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Dispatcher : in out Handler) is
      Ref_Counter : constant Natural := Dispatcher.Ref_Counter;
   begin
      Finalize (AWS.Dispatchers.Handler (Dispatcher));

      if Ref_Counter = 1 then
         for Item of Dispatcher.Table loop
            Free (Item.Action);
            Unchecked_Free (Item);
         end loop;

         Period_Table.Clear (Dispatcher.Table);
      end if;
   end Finalize;

   ------------
   -- Hourly --
   ------------

   function Hourly
     (From_Minute : Minute_Number;
      From_Second : Second_Number;
      To_Minute   : Minute_Number;
      To_Second   : Second_Number) return Period is
   begin
      return Period'(Mode => Hourly,
                     From => (Minute => From_Minute,
                              Second => From_Second,
                              others => <>),
                     To   => (Minute => To_Minute,
                              Second => To_Second,
                              others => <>));
   end Hourly;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (Dispatcher : in out Handler) is
   begin
      Initialize (AWS.Dispatchers.Handler (Dispatcher));
   end Initialize;

   --------------
   -- Minutely --
   --------------

   function Minutely
     (From_Second : Second_Number;
      To_Second   : Second_Number) return Period is
   begin
      return Period'(Mode => Minutely,
                     From => (Second => From_Second,
                              others => <>),
                     To   => (Second => To_Second,
                              others => <>));
   end Minutely;

   -------------
   -- Monthly --
   -------------

   function Monthly
     (From_Day    : Day_Number;
      From_Hour   : Hour_Number;
      From_Minute : Minute_Number;
      From_Second : Second_Number;
      To_Day      : Day_Number;
      To_Hour     : Hour_Number;
      To_Minute   : Minute_Number;
      To_Second   : Second_Number) return Period is
   begin
      return Period'(Mode => Monthly,
                     From => (Day    => From_Day,
                              Hour   => From_Hour,
                              Minute => From_Minute,
                              Second => From_Second,
                              others => <>),
                     To   => (Day    => To_Day,
                              Hour   => To_Hour,
                              Minute => To_Minute,
                              Second => To_Second,
                              others => <>));
   end Monthly;

   ----------
   -- Once --
   ----------

   function Once
     (From_Year   : Year_Number;
      From_Month  : Month_Number;
      From_Day    : Day_Number;
      From_Hour   : Hour_Number;
      From_Minute : Minute_Number;
      From_Second : Second_Number;
      To_Year     : Year_Number;
      To_Month    : Month_Number;
      To_Day      : Day_Number;
      To_Hour     : Hour_Number;
      To_Minute   : Minute_Number;
      To_Second   : Second_Number) return Period is
   begin
      return Period'(Mode => Once,
                     From => (Year   => From_Year,
                              Month  => From_Month,
                              Day    => From_Day,
                              Hour   => From_Hour,
                              Minute => From_Minute,
                              Second => From_Second,
                              others => <>),
                     To   => (Year   => To_Year,
                              Month  => To_Month,
                              Day    => To_Day,
                              Hour   => To_Hour,
                              Minute => To_Minute,
                              Second => To_Second,
                              others => <>));
   end Once;

   --------------
   -- Register --
   --------------

   procedure Register
     (Dispatcher : in out Handler;
      Name       : String;
      Period     : Timer.Period;
      Action     : AWS.Dispatchers.Handler'Class)
   is
      Value : constant Node_Access :=
                new Node'(To_Unbounded_String (Name),
                          Period,
                          new AWS.Dispatchers.Handler'Class'(Action));
   begin
      Period_Table.Append (Dispatcher.Table, Value);
   end Register;

   procedure Register
     (Dispatcher : in out Handler;
      Name       : String;
      Period     : Timer.Period;
      Action     : Response.Callback) is
   begin
      Register
        (Dispatcher, Name, Period, AWS.Dispatchers.Callback.Create (Action));
   end Register;

   -------------------------------
   -- Register_Default_Callback --
   -------------------------------

   procedure Register_Default_Callback
     (Dispatcher : in out Handler;
      Action     : AWS.Dispatchers.Handler'Class) is
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
      Name       : String)
   is
      use type Period_Table.Cursor;

      Pos : Period_Table.Cursor := Period_Table.No_Element;
   begin
      for Cursor in Dispatcher.Table.Iterate loop
         declare
            Item : Node_Access := Period_Table.Element (Cursor);
         begin
            if To_String (Item.Name) = Name then
               Unchecked_Free (Item);
               Pos := Cursor;
               exit;
            end if;
         end;
      end loop;

      if Pos = Period_Table.No_Element then
         raise Constraint_Error
           with "Timer distpatcher " & Name & " not found";
      else
         Period_Table.Delete (Dispatcher.Table, Pos);
      end if;
   end Unregister;

   ------------
   -- Weekly --
   ------------

   function Weekly
     (From_Day    : Day_Name;
      From_Hour   : Hour_Number;
      From_Minute : Minute_Number;
      From_Second : Second_Number;
      To_Day      : Day_Name;
      To_Hour     : Hour_Number;
      To_Minute   : Minute_Number;
      To_Second   : Second_Number) return Period
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
     (From_Month  : Month_Number;
      From_Day    : Day_Number;
      From_Hour   : Hour_Number;
      From_Minute : Minute_Number;
      From_Second : Second_Number;
      To_Month    : Month_Number;
      To_Day      : Day_Number;
      To_Hour     : Hour_Number;
      To_Minute   : Minute_Number;
      To_Second   : Second_Number) return Period
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
