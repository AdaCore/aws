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

   function File_Timestamp (Filename : in String) return Ada.Calendar.Time;
   pragma Inline (File_Timestamp);
   --  Get the time for last modification to a file in UTC/GMT

   function GMT_Clock return Ada.Calendar.Time;
   pragma Inline (GMT_Clock);
   --  Returns current UTC/GMT time

   --------------------------
   -- Directory Operations --
   --------------------------

   type Dir_Type is limited private;

   Directory_Error : exception;

   procedure Open (Dir : out Dir_Type; Dir_Name : in String);
   --  Opens the directory named by Dir_Name and returns a Dir_Type value
   --  that refers to this directory, and is positioned at the first entry.
   --  Raises Directory_Error if Dir_Name cannot be accessed. In that case
   --  Dir will be set to Null_Dir.

   procedure Read
     (Dir  : in out Dir_Type;
      Str  :    out String;
      Last :    out Natural);
   --  Reads the next entry from the directory and sets Str to the name
   --  of that entry. Last is the index in Str such that Str (Last) is the
   --  last character written. Last is 0 when there are no more files in the
   --  directory. If Str is too small for the file name, the file name will
   --  be truncated before being copied to Str. The list of files returned
   --  includes directories in systems providing a hierarchical directory
   --  structure, including . (the current directory) and .. (the parent
   --  directory) in systems providing these entries. Raises Directory_Error
   --  if Dir has not be opened.

   procedure Close (Dir : in out Dir_Type);
   --  Closes the directory stream refered to by Dir. After calling Close
   --  Is_Open will return False. Dir will be set to Null_Dir.
   --  Raises Directory_Error if Dir has not be opened (Dir = Null_Dir).

   function Base_Name
     (Path   : in String;
      Suffix : in String    := "")
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

private

   type Dir_Type_Hidden;
   type Dir_Type is access Dir_Type_Hidden;

end AWS.OS_Lib;
