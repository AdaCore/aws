------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2003-2013, AdaCore                     --
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

--  Dispatch a specific request to a callback depending on current time

with Ada.Calendar.Formatting;

with AWS.Dispatchers;
with AWS.Response;
with AWS.Status;

private with Ada.Containers.Vectors;
private with Ada.Strings.Unbounded;

package AWS.Services.Dispatchers.Timer is

   use Ada;

   type Handler is new AWS.Dispatchers.Handler with private;

   type Period is private;

   subtype Year_Number   is Calendar.Year_Number;
   subtype Month_Number  is Calendar.Month_Number;
   subtype Day_Number    is Calendar.Day_Number;
   subtype Day_Name      is Calendar.Formatting.Day_Name;
   subtype Hour_Number   is Calendar.Formatting.Hour_Number;
   subtype Minute_Number is Calendar.Formatting.Minute_Number;
   subtype Second_Number is Calendar.Formatting.Second_Number;

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
      To_Second   : Second_Number)
      return Period;
   --  A period that is uniq in time

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
      To_Second   : Second_Number)
      return Period;
   --  A period that repeats each year

   function Monthly
     (From_Day    : Day_Number;
      From_Hour   : Hour_Number;
      From_Minute : Minute_Number;
      From_Second : Second_Number;
      To_Day      : Day_Number;
      To_Hour     : Hour_Number;
      To_Minute   : Minute_Number;
      To_Second   : Second_Number)
      return Period;
   --  A period that repeats each month

   function Weekly
     (From_Day    : Day_Name;
      From_Hour   : Hour_Number;
      From_Minute : Minute_Number;
      From_Second : Second_Number;
      To_Day      : Day_Name;
      To_Hour     : Hour_Number;
      To_Minute   : Minute_Number;
      To_Second   : Second_Number)
      return Period;
   --  A period that repeats each week

   function Daily
     (From_Hour   : Hour_Number;
      From_Minute : Minute_Number;
      From_Second : Second_Number;
      To_Hour     : Hour_Number;
      To_Minute   : Minute_Number;
      To_Second   : Second_Number)
      return Period;
   --  A period that repeats each day

   function Hourly
     (From_Minute : Minute_Number;
      From_Second : Second_Number;
      To_Minute   : Minute_Number;
      To_Second   : Second_Number)
      return Period;
   --  A period that repeats each hour

   function Minutely
     (From_Second : Second_Number;
      To_Second   : Second_Number)
      return Period;
   --  A period that repeats each minute

   procedure Register
     (Dispatcher : in out Handler;
      Name       : String;
      Period     : Timer.Period;
      Action     : AWS.Dispatchers.Handler'Class);
   --  Register a Period to use the specified dispatcher

   procedure Register
     (Dispatcher : in out Handler;
      Name       : String;
      Period     : Timer.Period;
      Action     : Response.Callback);
   --  Idem as above but take a callback procedure as parameter

   procedure Unregister
     (Dispatcher : in out Handler;
      Name       : String);
   --  Removes the period dispatcher named Name. Does nothing if Name is not
   --  found.

   procedure Register_Default_Callback
     (Dispatcher : in out Handler;
      Action     : AWS.Dispatchers.Handler'Class);
   --  Register the default callback. This will be used if no period
   --  matches the current time.

private

   use Ada.Strings.Unbounded;

   overriding procedure Initialize (Dispatcher : in out Handler);
   overriding procedure Finalize   (Dispatcher : in out Handler);

   overriding function Dispatch
     (Dispatcher : Handler;
      Request    : Status.Data) return Response.Data;
   --  Dispatch will call the time dispatcher that matches the current time.
   --  Note that if a callback returns the Response.Empty message, Dispatch
   --  will just continue to the next matching callback. In any cases, if no
   --  handler matches it will call the default callback. If no default
   --  callback is registered an error HTML message (code 404) is returned.

   overriding function Clone (Dispatcher : Handler) return Handler;
   --  Returns a deep copy of the dispatcher

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
     new Ada.Containers.Vectors (Positive, Node_Access, "=");

   type Handler is new AWS.Dispatchers.Handler with record
      Action : AWS.Dispatchers.Handler_Class_Access;
      Table  : Period_Table.Vector;
   end record;

end AWS.Services.Dispatchers.Timer;
