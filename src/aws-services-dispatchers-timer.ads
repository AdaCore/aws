------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2003-2006                          --
--                                 AdaCore                                  --
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

--  Dispatch a specific request to a callback depending on current time

with Ada.Calendar;
with Ada.Strings.Unbounded;

with AI302.Containers.Vectors;
with GNAT.Calendar;

with AWS.Dispatchers;
with AWS.Response;
with AWS.Status;

package AWS.Services.Dispatchers.Timer is

   use Ada;

   type Handler is new AWS.Dispatchers.Handler with private;

   function Dispatch
     (Dispatcher : in Handler;
      Request    : in Status.Data)
      return Response.Data;
   --  Dispatch will call the time dispatcher that matches the current time.
   --  Note that if a callback returns the Response.Empty message, Dispatch
   --  will just continue to the next matching callback. In any cases, if no
   --  handler matches it will call the default callback. If no default
   --  callback is registered an error HTML message (code 404) is returned.

   type Period is private;

   subtype Year_Number   is Ada.Calendar.Year_Number;
   subtype Month_Number  is Ada.Calendar.Month_Number;
   subtype Day_Number    is Ada.Calendar.Day_Number;
   subtype Day_Name      is GNAT.Calendar.Day_Name;
   subtype Hour_Number   is GNAT.Calendar.Hour_Number;
   subtype Minute_Number is GNAT.Calendar.Minute_Number;
   subtype Second_Number is GNAT.Calendar.Second_Number;

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
      return Period;
   --  A period that is uniq in time

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
      return Period;
   --  A period that repeats each year

   function Monthly
     (From_Day    : in Day_Number;
      From_Hour   : in Hour_Number;
      From_Minute : in Minute_Number;
      From_Second : in Second_Number;
      To_Day      : in Day_Number;
      To_Hour     : in Hour_Number;
      To_Minute   : in Minute_Number;
      To_Second   : in Second_Number)
      return Period;
   --  A period that repeats each month

   function Weekly
     (From_Day    : in Day_Name;
      From_Hour   : in Hour_Number;
      From_Minute : in Minute_Number;
      From_Second : in Second_Number;
      To_Day      : in Day_Name;
      To_Hour     : in Hour_Number;
      To_Minute   : in Minute_Number;
      To_Second   : in Second_Number)
      return Period;
   --  A period that repeats each week

   function Daily
     (From_Hour   : in Hour_Number;
      From_Minute : in Minute_Number;
      From_Second : in Second_Number;
      To_Hour     : in Hour_Number;
      To_Minute   : in Minute_Number;
      To_Second   : in Second_Number)
      return Period;
   --  A period that repeats each day

   function Hourly
     (From_Minute : in Minute_Number;
      From_Second : in Second_Number;
      To_Minute   : in Minute_Number;
      To_Second   : in Second_Number)
      return Period;
   --  A period that repeats each hour

   function Minutely
     (From_Second : in Second_Number;
      To_Second   : in Second_Number)
      return Period;
   --  A period that repeats each minute

   procedure Register
     (Dispatcher : in out Handler;
      Name       : in     String;
      Period     : in     Timer.Period;
      Action     : in     AWS.Dispatchers.Handler'Class);
   --  Register a Period to use the specified dispatcher

   procedure Register
     (Dispatcher : in out Handler;
      Name       : in     String;
      Period     : in     Timer.Period;
      Action     : in     Response.Callback);
   --  Idem as above but take a callback procedure as parameter

   procedure Unregister
     (Dispatcher : in out Handler;
      Name       : in     String);
   --  Removes the period dispatcher named Name. Does nothing if Name is not
   --  found.

   procedure Register_Default_Callback
     (Dispatcher : in out Handler;
      Action     : in     AWS.Dispatchers.Handler'Class);
   --  Register the default callback. This will be used if no period
   --  matches the current time.

private

   use Ada.Strings.Unbounded;

   procedure Initialize (Dispatcher : in out Handler);
   procedure Finalize   (Dispatcher : in out Handler);

   type Kind is (Once, Yearly, Monthly, Weekly, Daily, Hourly, Minutely);

   type Date_Time is record
      Year   : Year_Number;
      Month  : Month_Number;
      Day    : Day_Number;
      N_Day  : Day_Name;
      Hour   : Hour_Number;
      Minute : Minute_Number;
      Second : Second_Number;
   end record;

   type Period is record
      Mode   : Kind;
      From   : Date_Time;
      To     : Date_Time;
   end record;

   type Node is record
      Name   : Unbounded_String;
      Period : Timer.Period;
      Action : AWS.Dispatchers.Handler_Class_Access;
   end record;

   type Node_Access is access Node;

   package Period_Table is
     new AI302.Containers.Vectors (Positive, Node_Access, "=");

   type Handler is new AWS.Dispatchers.Handler with record
      Action : AWS.Dispatchers.Handler_Class_Access;
      Table  : Period_Table.Vector;
   end record;

end AWS.Services.Dispatchers.Timer;
