------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2000-2006                          --
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

--  Use direct calls to Win32 system routines instead of the POSIX library
--  and get the current UTC/GMT time from the system.

with Interfaces;

package body AWS.OS_Lib is

   use Ada;

   ------------------------
   -- Win32 declarations --
   ------------------------

   type System_Time is record
      Year         : Interfaces.Unsigned_16;
      Month        : Interfaces.Unsigned_16;
      Day_Of_Week  : Interfaces.Unsigned_16;
      Day          : Interfaces.Unsigned_16;
      Hour         : Interfaces.Unsigned_16;
      Minute       : Interfaces.Unsigned_16;
      Second       : Interfaces.Unsigned_16;
      Milli_Second : Interfaces.Unsigned_16;
   end record;

   --  Import Win32 routines

   procedure GetSystemTime (Time : out System_Time);
   pragma Import (Stdcall, GetSystemTime, "GetSystemTime");

   function Systime_To_Time (UTC : System_Time) return Calendar.Time;
   --  Convert a Win32 System_Time to an Calendar.Time

   ---------------
   -- GMT_Clock --
   ---------------

   function GMT_Clock return Ada.Calendar.Time is
      UTC : System_Time;
   begin
      GetSystemTime (UTC);
      return Systime_To_Time (UTC);
   end GMT_Clock;

   ---------------------
   -- Systime_To_Time --
   ---------------------

   function Systime_To_Time (UTC : System_Time) return Calendar.Time is
   begin
      return Calendar.Time_Of
        (Integer (UTC.Year),
         Integer (UTC.Month),
         Integer (UTC.Day),
         Duration (Integer (UTC.Hour)   * 3600 +
             Integer (UTC.Minute) * 60 +
             Integer (UTC.Second)) +
           Duration (UTC.Milli_Second) / 1000);
   end Systime_To_Time;

end AWS.OS_Lib;
