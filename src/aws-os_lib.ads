------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2000-2003                          --
--                                ACT-Europe                                --
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

with Ada.Calendar;
with Ada.Streams;

package AWS.OS_Lib is

   No_Such_File : exception;

   function Is_Regular_File (Filename : in String) return Boolean;
   pragma Inline (Is_Regular_File);
   --  Returns True if Filename is a regular file and is readable

   function Is_Directory (Filename : in String) return Boolean;
   pragma Inline (Is_Directory);
   --  Returns True if Filename is a directory

   function File_Size (Filename : in String)
      return Ada.Streams.Stream_Element_Offset;
   pragma Inline (File_Size);
   --  Returns Filename's size in bytes

   function File_Time_Stamp (Filename : in String) return Ada.Calendar.Time;
   pragma Inline (File_Time_Stamp);
   --  Get the time for last modification to a file in UTC/GMT

   function GMT_Clock return Ada.Calendar.Time;
   pragma Inline (GMT_Clock);
   --  Returns current UTC/GMT time

   --------------------------
   -- Directory Operations --
   --------------------------

   generic
      with procedure Action
        (Filename     : in     String;
         Is_Directory : in     Boolean;
         Quit         : in out Boolean);
   procedure For_Every_Directory_Entry (Directory_Name : in String);
   --  Reads all entries in Directory_Name and calls Action for each
   --  one. Is_Directory is set to True if Filename is a directory. Quit can
   --  be set to True to stop the iterator. Raises No_Such_File if
   --  Directory_Name does not exists.

   function Base_Name
     (Path   : in String;
      Suffix : in String := "")
      return String;
   --  Any directory prefix is removed. If Suffix is non-empty and is a
   --  suffix of Path, it is removed.

   function File_Extension (Path : in String) return String;
   --  Return the file extension. This is the string after the last dot
   --  character in File_Name (Path). It returns the empty string if no
   --  extension is found. The returned value does contains the file
   --  extension separator (dot character).

   function File_Name (Path : in String) return String;
   --  Returns the file name and the file extension if present. It removes all
   --  path information. This is equivalent to Base_Name with default Extension
   --  value.

end AWS.OS_Lib;
