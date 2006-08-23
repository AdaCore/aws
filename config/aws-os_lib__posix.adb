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

--  This implementation use a POSIX interface and should be usable under many
--  systems and is compiler independant.

with Interfaces.C;

package body AWS.OS_Lib is

   use Ada;

   ---------------
   -- GMT_Clock --
   ---------------

   function GMT_Clock return Calendar.Time is

      use Interfaces;
      use type Interfaces.C.int;

      type tm is record
         tm_sec   : C.int;
         tm_min   : C.int;
         tm_hour  : C.int;
         tm_mday  : C.int;
         tm_mon   : C.int;
         tm_year  : C.int;
         tm_wday  : C.int;
         tm_yday  : C.int;
         tm_isdst : C.int;
      end record;
      pragma Convention (C, tm);
      --  This tm structure is ok for Windows, Linux and Solaris.

      type tm_access is access tm;
      pragma Convention (C, tm_access);

      procedure time (ltime : access C.long);
      pragma Import (C, time);

      function gmtime (ltime : access C.long) return tm_access;
      pragma Import (C, gmtime);

      ltime : aliased C.long;
      gmt   : tm;

   begin
      time (ltime'Unchecked_Access);
      gmt := gmtime (ltime'Unchecked_Access).all;

      return Calendar.Time_Of
        (Calendar.Year_Number (1900 + gmt.tm_year),
         Calendar.Month_Number (gmt.tm_mon + 1),
         Calendar.Day_Number (gmt.tm_mday),
         Calendar.Day_Duration (gmt.tm_hour * 3600
           + gmt.tm_min * 60
           + gmt.tm_sec));
   end GMT_Clock;

end AWS.OS_Lib;
