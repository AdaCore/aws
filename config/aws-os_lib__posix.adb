------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2000-2003                          --
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

--  $Id$

--  This implementation use a POSIX interface and should be usable under many
--  systems and is compiler independant.

with Ada.Calendar;
with Ada.Streams;
with Ada.Strings.Fixed;
with Interfaces.C;

with POSIX;
with POSIX.Calendar;
with POSIX.File_Status;
with POSIX.Files;

package body AWS.OS_Lib is

   use Ada;
   use POSIX;

   ---------------
   -- Base_Name --
   ---------------

   function Base_Name
     (Path   : in String;
      Suffix : in String := "")
      return String
   is
      K : Natural := Strings.Fixed.Index (Path, "/", Strings.Backward);
      L : Natural;
   begin
      if K = 0 then
         K := Path'First;
      else
         K := K + 1;
      end if;

      if Path (Path'Last - Suffix'Length + 1 .. Path'Last) = Suffix then
         L := Path'Last - Suffix'Length;
      else
         L := Path'Last;
      end if;

      return Path (K .. L);
   end Base_Name;

   --------------------
   -- File_Extension --
   --------------------

   function File_Extension (Path : in String) return String is
      K : constant Natural
        := Strings.Fixed.Index (Path, ".", Strings.Backward);
   begin
      if K = 0 then
         return "";
      else
         return Path (K .. Path'Last);
      end if;
   end File_Extension;

   ---------------
   -- File_Name --
   ---------------

   function File_Name (Path : in String) return String is
   begin
      return Base_Name (Path);
   end File_Name;

   ---------------
   -- File_Size --
   ---------------

   function File_Size
     (Filename : in String)
      return Streams.Stream_Element_Offset
   is
      use POSIX.File_Status;
   begin
      return Streams.Stream_Element_Offset
        ((Size_Of (Get_File_Status (To_POSIX_String (Filename)))));
   exception
      when POSIX.POSIX_Error =>
         raise No_Such_File;
   end File_Size;

   --------------------
   -- File_Timestamp --
   --------------------

   function File_Timestamp (Filename : in String) return Ada.Calendar.Time is
   begin
      return POSIX.Calendar.To_Time
        (File_Status.Last_Modification_Time_Of
           (File_Status.Get_File_Status
              (To_POSIX_String (Filename))));
   exception
      when POSIX.POSIX_Error =>
         raise No_Such_File;
   end File_Timestamp;

   -------------------------------
   -- For_Every_Directory_Entry --
   -------------------------------

   procedure For_Every_Directory_Entry (Directory_Name : in String) is

      procedure Action
        (D_Entry : in     Files.Directory_Entry;
         Quit    : in out Boolean);

      ------------
      -- Action --
      ------------

      procedure Action
        (D_Entry : in     Files.Directory_Entry;
         Quit    : in out Boolean)
      is
         Filename : constant String
           := To_String (Files.Filename_Of (D_Entry));
      begin
         Action (Filename, Is_Directory (Filename), Quit);
      end Action;

      --------------
      -- Iterator --
      --------------

      procedure Iterator is
         new POSIX.Files.For_Every_Directory_Entry (Action);

   begin
      Iterator (To_POSIX_String (Directory_Name));
   exception
      when POSIX_Error =>
         raise No_Such_File;
   end For_Every_Directory_Entry;

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

   ------------------
   -- Is_Directory --
   ------------------

   function Is_Directory (Filename : in String) return Boolean is
   begin
      return File_Status.Is_Directory
        (File_Status.Get_File_Status (To_POSIX_String (Filename)));
   exception
      when others =>
         return False;
   end Is_Directory;

   ---------------------
   -- Is_Regular_File --
   ---------------------

   function Is_Regular_File (Filename : in String) return Boolean is
   begin
      return File_Status.Is_Regular_File
        (File_Status.Get_File_Status (To_POSIX_String (Filename)));
   exception
      when others =>
         return False;
   end Is_Regular_File;

end AWS.OS_Lib;
