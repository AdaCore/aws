------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                            Copyright (C) 2000                            --
--                Dmitriy Anisimkov, Sune Falck, Pascal Obry                --
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

--  This implementation use a POSIX interface and should be usable under many
--  systems and is compiler independant.

with Ada.Calendar;
with Ada.Streams;
with Interfaces.C;

with POSIX;
with POSIX.Calendar;
with POSIX.File_Status;

package body AWS.OS_Lib is

   ---------------------
   -- Is_Regular_File --
   ---------------------

   function Is_Regular_File (Filename : in String) return Boolean is
   begin
      return POSIX.File_Status.Is_Regular_File
        (POSIX.File_Status.Get_File_Status (POSIX.To_POSIX_String (Filename)));
   exception
      when others =>
         return False;
   end Is_Regular_File;

   ------------------
   -- Is_Directory --
   ------------------

   function Is_Directory (Filename : in String) return Boolean is
   begin
      return POSIX.File_Status.Is_Directory
        (POSIX.File_Status.Get_File_Status (POSIX.To_POSIX_String (Filename)));
   exception
      when others =>
         return False;
   end Is_Directory;

   --------------------
   -- File_Timestamp --
   --------------------

   function File_Timestamp (Filename : in String)
                           return Ada.Calendar.Time is
   begin
      return POSIX.Calendar.To_Time
        (POSIX.File_Status.Last_Modification_Time_Of
         (POSIX.File_Status.Get_File_Status
          (POSIX.To_POSIX_String (Filename))));
   exception
      when POSIX.POSIX_Error =>
         raise No_Such_File;
   end File_Timestamp;

   ---------------
   -- File_Size --
   ---------------

   function File_Size (Filename : in String)
                      return Ada.Streams.Stream_Element_Offset
   is
      use POSIX.File_Status;
   begin
      return Ada.Streams.Stream_Element_Offset
        ((Size_Of (Get_File_Status (POSIX.To_POSIX_String (Filename)))));
   exception
      when POSIX.POSIX_Error =>
         raise No_Such_File;
   end File_Size;

   ---------------
   -- GMT_Clock --
   ---------------

   function GMT_Clock return Ada.Calendar.Time is

      use Interfaces;

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

      use Ada.Calendar;
      use type Interfaces.C.int;

   begin
      time (ltime'Unchecked_Access);
      gmt := gmtime (ltime'Unchecked_Access).all;

      return Time_Of (Year_Number (1900 + gmt.tm_year),
                      Month_Number (gmt.tm_mon + 1),
                      Day_Number (gmt.tm_mday),
                      Day_Duration (gmt.tm_hour * 3600
                                    + gmt.tm_min * 60
                                    + gmt.tm_sec));
   end GMT_Clock;

end AWS.OS_Lib;
